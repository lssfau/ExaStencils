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
import exastencils.util.ir._

/// CUDA_PrepareMPICode

object CUDA_PrepareMPICode extends DefaultStrategy("Prepare CUDA relevant code by adding memory transfer statements " +
  "and annotating for later kernel transformation") with CUDA_PrepareBufferSync {

  val fragLoopCollector = new IR_FragmentLoopCollector
  val commKernelCollector = new IR_CommunicationKernelCollector
  val fctNameCollector = new IR_FctNameCollector
  this.register(fctNameCollector)
  this.register(fragLoopCollector)
  this.register(commKernelCollector)
  this.onBefore = () => this.resetCollectors()

  var fieldAccesses = HashMap[String, IR_IV_FieldData]()
  var bufferAccesses = HashMap[String, IR_IV_CommBuffer]()

  var fragLoopsWithHandling : mutable.HashMap[IR_ScopedStatement with IR_HasParallelizationInfo, CUDA_HandleFragmentLoops] = mutable.HashMap()

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
    identifier = (if (inWriteOp) "write_" else "read_") + identifier

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
    // don't filter here - memory transfer code is still required

    fieldAccesses.clear()
    bufferAccesses.clear()

    mpiStmt match {
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
    }

    // determine execution ( = comm/comp ) stream
    val executionStream = CUDA_Stream.getStream(fragLoopCollector, commKernelCollector)

    // collect elements accessed in enclosing fragment loop and create handler
    val enclosingFragLoop = fragLoopCollector.getEnclosingFragmentLoop()
    if (enclosingFragLoop.isDefined) {
      val emptyFragloopHandler = CUDA_HandleFragmentLoops(enclosingFragLoop.get, ListBuffer(), mutable.HashMap(), mutable.HashMap())
      val fragloopHandler = fragLoopsWithHandling.getOrElse(enclosingFragLoop.get, emptyFragloopHandler)

      // add accessed streams
      if (!fragloopHandler.streams.contains(executionStream))
        fragloopHandler.streams += executionStream

      // add accessed buffers/fields
      fragloopHandler.fieldAccesses ++= fieldAccesses.map { case (str, fAcc) => str -> IR_IV_FieldData(fAcc.field, fAcc.slot, fAcc.fragmentIdx) }
        .filterNot(acc => fragloopHandler.fieldAccesses.contains(acc._1))
      fragloopHandler.bufferAccesses ++= bufferAccesses.filterNot(acc => fragloopHandler.bufferAccesses.contains(acc._1))

      fragLoopsWithHandling.update(enclosingFragLoop.get, fragloopHandler)
    }

    // - host sync stmts -

    // wait for pending transfer events
    for (access <- fieldAccesses.toSeq.sortBy(_._1)) {
      val fieldData = access._2
      if (syncBeforeHost(access._1, fieldAccesses.keys))
        beforeHost += CUDA_WaitEvent(CUDA_PendingStreamTransfers(fieldData.field, fieldData.fragmentIdx), executionStream, "D2H")
    }
    for (access <- bufferAccesses.toSeq.sortBy(_._1)) {
      val buffer = access._2
      if (syncBeforeHost(access._1, bufferAccesses.keys))
        beforeHost += CUDA_WaitEvent(CUDA_PendingStreamTransfers(buffer.field, buffer.fragmentIdx), executionStream, "D2H")
    }

    // - device sync stmts -

    if (Knowledge.cuda_syncDeviceAfterKernelCalls)
      afterDevice += CUDA_DeviceSynchronize()

    // wait for pending transfer events
    for (access <- fieldAccesses.toSeq.sortBy(_._1)) {
      val fieldData = access._2
      if (syncBeforeDevice(access._1, fieldAccesses.keys))
        beforeDevice += CUDA_WaitEvent(CUDA_PendingStreamTransfers(fieldData.field, fieldData.fragmentIdx), executionStream, "H2D")
    }
    for (access <- bufferAccesses.toSeq.sortBy(_._1)) {
      val buffer = access._2
      if (syncBeforeDevice(access._1, bufferAccesses.keys))
        beforeDevice += CUDA_WaitEvent(CUDA_PendingStreamTransfers(buffer.field, buffer.fragmentIdx), executionStream, "H2D")
    }

    (beforeHost, afterHost, beforeDevice, afterDevice)
  }

  def addHostDeviceBranching(hostStmts : ListBuffer[IR_Statement], deviceStmts : ListBuffer[IR_Statement], loop : IR_LoopOverDimensions, earlyExit : Boolean) : ListBuffer[IR_Statement] = {
    if (earlyExit) {
      hostStmts
    } else {
      /// compile final switch
      val defaultChoice : IR_Expression = Knowledge.cuda_preferredExecution match {
        case "Host"        => 1 // CPU by default
        case "Device"      => 0 // GPU by default
        case "Performance" => if (loop.getAnnotation("perf_timeEstimate_host").get.asInstanceOf[Double] > loop.getAnnotation("perf_timeEstimate_device").get.asInstanceOf[Double]) 0 else 1 // decide according to performance estimates
        case "Condition"   => Knowledge.cuda_executionCondition
      }

      ListBuffer[IR_Statement](IR_IfCondition(defaultChoice, hostStmts, deviceStmts))
    }
  }

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
        val defaultChoice : IR_Expression = Knowledge.cuda_preferredExecution match {
          case _ if !Platform.hw_gpu_gpuDirectAvailable => 1 // if GPUDirect is not available default to CPU
          case "Host"                                   => 1 // CPU by default
          case "Device"                                 => 0 // GPU by default
          case "Performance"                            => 1 // FIXME: Knowledge flag
          case "Condition"                              => Knowledge.cuda_executionCondition
        }

        ListBuffer[IR_Statement](IR_IfCondition(defaultChoice, hostStmts, deviceStmts))
      }
  }, false)

  // replace orig enclosing fragment loop with handled fragment loop structure
  this += new Transformation("Create overlapping fragment loop structure", {
    case loop : IR_LoopOverFragments if fragLoopsWithHandling.contains(loop)                                                                                     =>
      fragLoopsWithHandling(loop)
    case loop @ IR_ForLoop(IR_VariableDeclaration(_, name, _, _), _, _, _, _) if name == IR_LoopOverFragments.defIt.name && fragLoopsWithHandling.contains(loop) =>
      fragLoopsWithHandling(loop)
  }, false)
}
