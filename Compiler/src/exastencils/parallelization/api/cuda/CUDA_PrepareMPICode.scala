package exastencils.parallelization.api.cuda

import scala.collection.Iterable
import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.ir.IR_IV_CommBuffer
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.core.collectors.FctNameCollector
import exastencils.datastructures._
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi._

/// CUDA_PrepareMPICode

object CUDA_PrepareMPICode extends DefaultStrategy("Prepare CUDA relevant code by adding memory transfer statements " +
  "and annotating for later kernel transformation") {
  val collector = new FctNameCollector
  this.register(collector)

  var fieldAccesses = HashMap[String, IR_IV_FieldData]()
  var bufferAccesses = HashMap[String, IR_IV_CommBuffer]()

  def syncBeforeHost(access : String, others : Iterable[String]) = {
    var sync = true
    if (access.startsWith("write") && !Knowledge.cuda_syncHostForWrites)
      sync = false // skip write accesses if demanded
    if (access.startsWith("write") && others.exists(_ == "read" + access.substring("write".length)))
      sync = false // skip write access for read/write accesses
    sync
  }

  def syncAfterHost(access : String, others : Iterable[String]) = {
    access.startsWith("write")
  }

  def syncBeforeDevice(access : String, others : Iterable[String]) = {
    var sync = true
    if (access.startsWith("write") && !Knowledge.cuda_syncDeviceForWrites)
      sync = false // skip write accesses if demanded
    if (access.startsWith("write") && others.exists(_ == "read" + access.substring("write".length)))
      sync = false // skip write access for read/write accesses
    sync
  }

  def syncAfterDevice(access : String, others : Iterable[String]) = {
    access.startsWith("write")
  }

  def mapFieldAccess(access : IR_MultiDimFieldAccess, inWriteOp : Boolean) = {
    val field = access.fieldSelection.field
    var identifier = field.codeName

    identifier = (if (inWriteOp) "write_" else "read_") + identifier

    // TODO: array fields
    if (field.numSlots > 1) {
      access.fieldSelection.slot match {
        case IR_SlotAccess(_, offset) => identifier += s"_o$offset"
        case IR_IntegerConstant(slot) => identifier += s"_s$slot"
        case other                    => identifier += s"_s${ other.prettyprint }"
      }
    }

    val fieldSelection = access.fieldSelection
    val fieldData = IR_IV_FieldData(fieldSelection.field, fieldSelection.level, fieldSelection.slot, fieldSelection.fragIdx)
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
      case send : MPI_Send        => processRead(send.buffer)
      case recv : MPI_Receive     => processWrite(recv.buffer)
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

    // host sync stmts

    for (access <- fieldAccesses.toSeq.sortBy(_._1)
    ) {
      val field = access._2

      // add data sync statements
      if (syncBeforeHost(access._1, fieldAccesses.keys)
      )
        beforeHost += CUDA_UpdateHostData(Duplicate(access._2)).expand().inner // expand here to avoid global expand afterwards

      // update flags for written fields
      if (syncAfterHost(access._1, fieldAccesses.keys)
      )
        afterHost += IR_Assignment(CUDA_HostDataUpdated(field.field, field.slot), IR_BooleanConstant(true))
    }

    for (access <- bufferAccesses.toSeq.sortBy(_._1)) {
      val buffer = access._2

      // add buffer sync statements
      if (syncBeforeHost(access._1, bufferAccesses.keys))
        beforeHost += CUDA_UpdateHostBufferData(Duplicate(access._2)).expand().inner // expand here to avoid global expand afterwards

      // update flags for written buffers
      if (syncAfterHost(access._1, bufferAccesses.keys))
        afterHost += IR_Assignment(CUDA_HostBufferDataUpdated(buffer.field, buffer.direction, buffer.neighIdx), IR_BooleanConstant(true))
    }

    // device sync stmts

    if (Knowledge.cuda_syncDeviceAfterKernelCalls)
      afterDevice += CUDA_DeviceSynchronize()

    for (access <- fieldAccesses.toSeq.sortBy(_._1)
    ) {
      val field = access._2

      // add data sync statements
      if (syncBeforeDevice(access._1, fieldAccesses.keys)
      )
        beforeDevice += CUDA_UpdateDeviceData(Duplicate(access._2)).expand().inner // expand here to avoid global expand afterwards

      // update flags for written fields
      if (syncAfterDevice(access._1, fieldAccesses.keys)
      )
        afterDevice += IR_Assignment(CUDA_DeviceDataUpdated(field.field, field.slot), IR_BooleanConstant(true))
    }

    for (access <- bufferAccesses.toSeq.sortBy(_._1)) {
      val buffer = access._2

      // add data sync statements
      if (syncBeforeDevice(access._1, bufferAccesses.keys))
        beforeDevice += CUDA_UpdateDeviceBufferData(Duplicate(access._2)).expand().inner // expand here to avoid global expand afterwards

      // update flags for written fields
      if (syncAfterDevice(access._1, bufferAccesses.keys))
        afterDevice += IR_Assignment(CUDA_DeviceBufferDataUpdated(buffer.field, buffer.direction, buffer.neighIdx), IR_BooleanConstant(true))
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
      val skip = mpiStmt match {
        case MPI_Init | MPI_Finalize | MPI_Barrier => true
        case _                                     => false
      }

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
              val fieldSel = linearized.fieldSelection
              val devField = CUDA_FieldDeviceData(fieldSel.field, fieldSel.level, fieldSel.slot, fieldSel.fragIdx)
              IR_ArrayAccess(devField, linearized.index)

            case fieldData : IR_IV_FieldData =>
              CUDA_FieldDeviceData(fieldData.field, fieldData.level, fieldData.slot, fieldData.fragmentIdx)

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
          case "Host"        => 1 // CPU by default
          case "Device"      => 0 // GPU by default
          case "Performance" => 1 // FIXME: Knowledge flag
          case "Condition"   => Knowledge.cuda_executionCondition
        }

        ListBuffer[IR_Statement](IR_IfCondition(defaultChoice, hostStmts, deviceStmts))
      }
  }, false)
}
