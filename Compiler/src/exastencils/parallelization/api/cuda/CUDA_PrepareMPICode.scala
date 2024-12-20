//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.parallelization.api.cuda

import scala.collection.mutable
import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.ir.IR_IV_CommBuffer
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi._
import exastencils.parallelization.ir.IR_HasParallelizationInfo
import exastencils.timing.ir.IR_TimerFunctions
import exastencils.util.NoDuplicateWrapper
import exastencils.util.ir._

/// CUDA_PrepareMPICode

object CUDA_PrepareMPICode extends DefaultStrategy("Prepare CUDA relevant code by adding memory transfer statements " +
  "and annotating for later kernel transformation") with CUDA_PrepareFragmentLoops {

  val fragLoopCollector = new IR_FragmentLoopCollector
  val commKernelCollector = new IR_CommunicationKernelCollector
  val fctNameCollector = new IR_FctNameCollector
  this.register(fctNameCollector)
  this.register(fragLoopCollector)
  this.register(commKernelCollector)
  this.onBefore = () => this.resetCollectors()

  var fieldAccesses = HashMap[String, IR_IV_FieldData]()
  var bufferAccesses = HashMap[String, IR_IV_CommBuffer]()

  var accessedElementsFragLoop : mutable.HashMap[IR_ScopedStatement with IR_HasParallelizationInfo, CUDA_AccessedElementsInFragmentLoop] = mutable.HashMap()

  override def collectAccessedBuffers(stmt : IR_Statement*) = {
    // don't filter here - memory transfer code is still required
    fieldAccesses.clear()
    bufferAccesses.clear()

    stmt foreach {
      case send : MPI_Send    => processRead(send.buffer)
      case recv : MPI_Receive => processWrite(recv.buffer)

      case bcast : MPI_Bcast =>
        processRead(bcast.buffer)
        processWrite(bcast.buffer)

      case gather : MPI_Gather =>
        processWrite(gather.recvbuf)
        processRead(gather.sendbuf)

      case reduce : MPI_AllReduce =>
        processWrite(reduce.recvbuf)
        if (("MPI_IN_PLACE" : IR_Expression) == reduce.sendbuf)
          processRead(reduce.recvbuf)
        else
          processRead(reduce.sendbuf)

      case reduce : MPI_Reduce =>
        processWrite(reduce.recvbuf)
        if (("MPI_IN_PLACE" : IR_Expression) == reduce.sendbuf)
          processRead(reduce.recvbuf)
        else
          processRead(reduce.sendbuf)
      case _ =>
    }
  }

  def mapFieldAccess(access : IR_MultiDimFieldAccess, inWriteOp : Boolean) = {
    val field = access.field
    var identifier = field.codeName

    identifier = (if (inWriteOp) "write_" else "read_") + identifier

    // TODO: array fields
    if (field.numSlots > 1) {
      access.slot match {
        case IR_SlotAccess(_, offset) => identifier += s"_o$offset"
        case IR_IntegerConstant(slot) => identifier += s"_s$slot"
        case other                    => identifier += s"_s${ other.prettyprint }"
      }
    }

    val fieldData = IR_IV_FieldData(access.field, Duplicate(access.slot), Duplicate(access.fragIdx))
    fieldAccesses.put(identifier, fieldData)
  }

  def mapFieldPtrAccess(fieldData : IR_IV_FieldData, inWriteOp : Boolean) {
    val field = fieldData.field
    var identifier = field.codeName

    identifier = (if (inWriteOp) "write_" else "read_") + identifier

    // TODO: array fields
    if (field.numSlots > 1) {
      fieldData.slot match {
        case IR_SlotAccess(_, offset) => identifier += s"_o$offset"
        case IR_IntegerConstant(slot) => identifier += s"_s$slot"
        case other                    => identifier += s"_s${ other.prettyprint }"
      }
    }

    fieldAccesses.put(identifier, fieldData)
  }

  def mapBuffer(buffer : IR_IV_CommBuffer, inWriteOp : Boolean) = {
    var identifier = buffer.resolveName()
    val neighIdx = buffer.neighIdx

    identifier = (if (inWriteOp) "write_" else "read_") + identifier + "_" + neighIdx

    bufferAccesses.put(identifier, buffer)
  }

  def processRead(expr : IR_Expression) {
    expr match {
      case access : IR_MultiDimFieldAccess => mapFieldAccess(access, false)
      case field : IR_IV_FieldData         => mapFieldPtrAccess(field, false)
      case buffer : IR_IV_CommBuffer       => mapBuffer(buffer, false)
      case IR_PointerOffset(base, _)       => processRead(base)

      case IR_AddressOf(IR_VariableAccess("timerValue", IR_DoubleDatatype)) => // ignore
      case IR_VariableAccess("timesToPrint", _)                             => // ignore

      case other => Logger.warn("Found unexpected expression: " + other)
    }
  }

  def processWrite(expr : IR_Expression) {
    expr match {
      case access : IR_MultiDimFieldAccess => mapFieldAccess(access, true)
      case field : IR_IV_FieldData         => mapFieldPtrAccess(field, true)
      case buffer : IR_IV_CommBuffer       => mapBuffer(buffer, true)
      case IR_PointerOffset(base, _)       => processWrite(base)

      case IR_AddressOf(IR_VariableAccess("timerValue", IR_DoubleDatatype)) => // ignore
      case IR_VariableAccess("timesToPrint", _)                             => // ignore

      case other => Logger.warn("Found unexpected expression: " + other)
    }
  }

  def getHostDeviceSyncStmts(mpiStmt : MPI_Statement) = {
    val (beforeHost, afterHost) = (ListBuffer[IR_Statement](), ListBuffer[IR_Statement]())
    val (beforeDevice, afterDevice) = (ListBuffer[IR_Statement](), ListBuffer[IR_Statement]())

    // get accessed buffers
    collectAccessedBuffers(mpiStmt)

    // determine execution ( = comm/comp ) stream
    val executionStream = CUDA_Stream.getStream(fragLoopCollector, commKernelCollector)

    // host sync stmts

    beforeHost ++= syncEventsBeforeHost(executionStream)

    afterHost ++= syncFlagsAfterHost()

    // device sync stmts

    if (!Knowledge.experimental_cuda_useStreams)
      beforeDevice += CUDA_DeviceSynchronize()

    beforeDevice ++= syncEventsBeforeDevice(executionStream)

    afterDevice ++= syncFlagsAfterDevice()

    (beforeHost, afterHost, beforeDevice, afterDevice)
  }

  // collect accessed elements for fragment loops with ContractingLoop and LoopOverDimensions nodes
  this += new Transformation("Collect accessed elements for fragment loop handling", {
    case mpiStmt : MPI_Statement      =>
      collectAccessedElementsFragmentLoop(ListBuffer(mpiStmt), fragLoopCollector, commKernelCollector,
        isParallel = true, fromMPIStatement = true, estimatedHostTime = 0.0, estimatedDeviceTime = 0.0)
      mpiStmt
  }, false)

  // replace orig enclosing fragment loop with handled fragment loop structure
  this += new Transformation("Create overlapping fragment loop structure", {
    case loop : IR_LoopOverFragments                                                                                     =>
      createFragLoopHandler(loop)
    case loop @ IR_ForLoop(IR_VariableDeclaration(_, name, _, _), _, _, _, _) if name == IR_LoopOverFragments.defIt.name =>
      createFragLoopHandler(loop)
  }, false)

  this += new Transformation("Process MPI statements", {
    case mpiStmt : MPI_Statement =>
      var skip = mpiStmt match {
        case MPI_Init | MPI_Finalize | MPI_Barrier => true
        case _                                     => false
      }

      // skip timer functions
      if (IR_TimerFunctions.functions.contains(fctNameCollector.getCurrentName))
        skip = true

      if (skip) {
        mpiStmt
      } else {
        val hostStmts = ListBuffer[IR_Statement]()
        val deviceStmts = ListBuffer[IR_Statement]()

        val (beforeHost, afterHost, beforeDevice, afterDevice) = getHostDeviceSyncStmts(mpiStmt)

        hostStmts ++= beforeHost
        deviceStmts ++= beforeDevice

        object CUDA_ReplaceAccessesInDeviceSpecMPI extends QuietDefaultStrategy("Replace accesses to fields and buffers to prepare device variants of MPI calls") {
          this += new Transformation("Search", {
            case access : IR_DirectFieldAccess =>
              val linearized = access.linearize
              val devField = CUDA_FieldDeviceData(linearized.field, Duplicate(linearized.slot), Duplicate(linearized.fragIdx))
              IR_ArrayAccess(devField, linearized.index)

            case fieldData : IR_IV_FieldData =>
              CUDA_FieldDeviceData(fieldData.field, fieldData.slot, fieldData.fragmentIdx)

            case buffer : IR_IV_CommBuffer =>
              CUDA_BufferDeviceData(buffer.field, buffer.direction, buffer.size, buffer.neighIdx, buffer.fragmentIdx)
          })
        }

        hostStmts += mpiStmt
        val mpiDeviceStmt = Duplicate(mpiStmt)
        CUDA_ReplaceAccessesInDeviceSpecMPI.applyStandalone(mpiDeviceStmt)
        deviceStmts += mpiDeviceStmt

        hostStmts ++= afterHost
        deviceStmts ++= afterDevice

        /// compile final switch
        val condWrapper = NoDuplicateWrapper[IR_Expression](null)
        getHostDeviceBranchingMPICondWrapper(condWrapper, hostStmts, deviceStmts)
      }
  }, false)
}
