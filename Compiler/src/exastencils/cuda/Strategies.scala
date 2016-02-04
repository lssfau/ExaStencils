package exastencils.cuda

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.data._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

object SplitLoopsForHostAndDevice extends DefaultStrategy("Splitting loops into host and device instances") {
  this += new Transformation("Processing LoopOverDimensions nodes", {
    case loop : LoopOverDimensions if (loop.reduction.isEmpty) // TODO: support reductions
    => {
      GatherLocalFieldAccess.fieldAccesses.clear
      GatherLocalFieldAccess.applyStandalone(Scope(loop.body))

      /// compile host statements
      var hostStmts = ListBuffer[Statement]()

      // add data sync statements
      for (access <- GatherLocalFieldAccess.fieldAccesses) {
        val syncWithDeviceForWrite = true // TODO: move flag to knowledge or other appropriate location
        if (syncWithDeviceForWrite || access._1.startsWith("read")) // skip write accesses if demanded
          hostStmts += CUDA_UpdateHostData(Duplicate(access._2)).expand.inner // expand here to avoid global expand afterwards
      }

      // add original loop
      hostStmts += loop

      // update flags for written fields
      for (access <- GatherLocalFieldAccess.fieldAccesses) {
        val fieldSelection = access._2.fieldSelection
        if (access._1.startsWith("write"))
          hostStmts += AssignmentStatement(iv.HostDataUpdated(fieldSelection.field, fieldSelection.slot), BooleanConstant(true))
      }

      /// compile device statements
      var deviceStmts = ListBuffer[Statement]()

      // add data sync statements
      for (access <- GatherLocalFieldAccess.fieldAccesses) {
        val syncWithDeviceForWrite = true // TODO: move flag to knowledge or other appropriate location
        if (syncWithDeviceForWrite || access._1.startsWith("read")) // skip write accesses if demanded
          deviceStmts += CUDA_UpdateDeviceData(Duplicate(access._2)).expand.inner // expand here to avoid global expand afterwards
      }

      // add kernel and kernel call
      val kernelFunctions = StateManager.findFirst[KernelFunctions]().get

      val kernel = Kernel(
        kernelFunctions.getIdentifier,
        loop.numDimensions,
        loop.indices,
        loop.body,
        loop.reduction,
        loop.condition)

      kernelFunctions.addKernel(kernel)
      deviceStmts += new FunctionCallExpression(kernel.getWrapperFctName)

      // update flags for written fields
      for (access <- GatherLocalFieldAccess.fieldAccesses) {
        val fieldSelection = access._2.fieldSelection
        if (access._1.startsWith("write"))
          deviceStmts += AssignmentStatement(iv.DeviceDataUpdated(fieldSelection.field, fieldSelection.slot), BooleanConstant(true))
      }

      /// compile final switch
      ConditionStatement(IntegerConstant(1),
        hostStmts,
        deviceStmts)
    }
  }, false)
}

object GatherLocalFieldAccess extends QuietDefaultStrategy("Gathering local FieldAccess nodes") {
  var fieldAccesses = HashMap[String, FieldAccess]()
  var inWriteOp = false

  def mapFieldAccess(access : FieldAccess) = {
    val field = access.fieldSelection.field
    var identifier = field.identifier

    identifier = (if (inWriteOp) "write_" else "read_") + identifier

    // TODO: array fields
    if (field.numSlots > 1) {
      access.fieldSelection.slot match {
        case SlotAccess(_, offset) => identifier += s"_o$offset"
        case IntegerConstant(slot) => identifier += s"_s$slot"
        case _                     => identifier += s"_s${access.fieldSelection.slot.prettyprint}"
      }
    }

    fieldAccesses.put(identifier, access)
  }

  this += new Transformation("Searching", {
    case assign : AssignmentStatement =>
      inWriteOp = true
      GatherLocalFieldAccess.applyStandalone(ExpressionStatement(assign.dest))
      inWriteOp = false
      GatherLocalFieldAccess.applyStandalone(ExpressionStatement(assign.src))
      assign
    case access : FieldAccess =>
      mapFieldAccess(access)
      access
  }, false)
}
