package exastencils.cuda

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.SortedSet

import exastencils.core._
import exastencils.data._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.omp._
import exastencils.polyhedron._

object SplitLoopsForHostAndDevice extends DefaultStrategy("Splitting loops into host and device instances") {
  this += new Transformation("Processing LoopOverDimensions nodes", {
    case loop : LoopOverDimensions => { // don't filter here - memory transfer code is still required
      GatherLocalFieldAccess.fieldAccesses.clear
      GatherLocalFieldAccess.applyStandalone(Scope(loop.body))

      /// compile host statements
      var hostStmts = ListBuffer[Statement]()

      // add data sync statements
      for (access <- GatherLocalFieldAccess.fieldAccesses.toSeq.sortBy(_._1)) {
        var sync = true
        if (access._1.startsWith("write") && !Knowledge.experimental_cuda_syncHostForWrites)
          sync = false // skip write accesses if demanded
        if (access._1.startsWith("write") && GatherLocalFieldAccess.fieldAccesses.contains("read" + access._1.substring("write".length)))
          sync = false // skip write access for read/write accesses
        if (sync)
          hostStmts += CUDA_UpdateHostData(Duplicate(access._2)).expand.inner // expand here to avoid global expand afterwards
      }

      // add original loop
      hostStmts += loop

      // update flags for written fields
      for (access <- GatherLocalFieldAccess.fieldAccesses.toSeq.sortBy(_._1)) {
        val fieldSelection = access._2.fieldSelection
        if (access._1.startsWith("write"))
          hostStmts += AssignmentStatement(iv.HostDataUpdated(fieldSelection.field, fieldSelection.slot), BooleanConstant(true))
      }

      // check for elimination criteria
      var earlyExit = false
      if (loop.reduction.isDefined)
        earlyExit = true // always use host until reductions are supported // TODO: support reductions
      if (!loop.isInstanceOf[PolyhedronAccessable])
        earlyExit = true // always use host for special loops
      if (!loop.isInstanceOf[OMP_PotentiallyParallel])
        earlyExit = true // always use host for un-parallelizable loops

      /// compile device statements
      if (earlyExit) {
        hostStmts
      } else {
        var deviceStmts = ListBuffer[Statement]()

        // add data sync statements
        for (access <- GatherLocalFieldAccess.fieldAccesses.toSeq.sortBy(_._1)) {
          var sync = true
          if (access._1.startsWith("write") && !Knowledge.experimental_cuda_syncDeviceForWrites)
            sync = false // skip write accesses if demanded
          if (access._1.startsWith("write") && GatherLocalFieldAccess.fieldAccesses.contains("read" + access._1.substring("write".length)))
            sync = false // skip write access for read/write accesses
          if (sync)
            deviceStmts += CUDA_UpdateDeviceData(Duplicate(access._2)).expand.inner // expand here to avoid global expand afterwards
        }

        // add kernel and kernel call
        val kernelFunctions = StateManager.findFirst[KernelFunctions]().get

        GatherLocalVariableAccesses.clear
        GatherLocalVariableAccesses.applyStandalone(Scope(loop.body))
        val variableAccesses = GatherLocalVariableAccesses.accesses.toSeq.sortBy(_._1).map(_._2).to[ListBuffer]

        val kernel = Kernel(
          kernelFunctions.getIdentifier,
          variableAccesses,
          loop.numDimensions,
          loop.indices,
          loop.body,
          loop.reduction,
          loop.condition)

        kernelFunctions.addKernel(Duplicate(kernel))
        deviceStmts += FunctionCallExpression(kernel.getWrapperFctName, variableAccesses.map(_.asInstanceOf[Expression]))
        if (Knowledge.experimental_cuda_syncDeviceAfterKernelCalls)
          deviceStmts += CUDA_DeviceSynchronize()

        // update flags for written fields
        for (access <- GatherLocalFieldAccess.fieldAccesses.toSeq.sortBy(_._1)) {
          val fieldSelection = access._2.fieldSelection
          if (access._1.startsWith("write"))
            deviceStmts += AssignmentStatement(iv.DeviceDataUpdated(fieldSelection.field, fieldSelection.slot), BooleanConstant(true))
        }

        /// compile final switch
        var defaultChoice = Knowledge.experimental_cuda_preferredExecution match {
          case "Host"        => 1 // CPU by default
          case "Device"      => 0 // GPU by default
          case "Performance" => if (loop.getAnnotation("perf_timeEstimate_host").get.value.asInstanceOf[Double] > loop.getAnnotation("perf_timeEstimate_device").get.value.asInstanceOf[Double]) 0 else 1 // decide according to performance estimates
        }

        ConditionStatement(defaultChoice, hostStmts, deviceStmts)
      }
    }
  }, false)
}

object GatherLocalFieldAccess extends QuietDefaultStrategy("Gathering local FieldAccess nodes") {
  var fieldAccesses = HashMap[String, FieldAccess]()
  var inWriteOp = false

  def mapFieldAccess(access : FieldAccess) = {
    val field = access.fieldSelection.field
    var identifier = field.codeName

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
      if ("=" != assign.op) // add compound assignments as read and write accesses
        GatherLocalFieldAccess.applyStandalone(ExpressionStatement(assign.dest))
      GatherLocalFieldAccess.applyStandalone(ExpressionStatement(assign.src))
      assign
    case access : FieldAccess =>
      mapFieldAccess(access)
      access
  }, false)
}

object GatherLocalVariableAccesses extends QuietDefaultStrategy("Gathering local VariableAccess nodes") {
  var accesses = HashMap[String, VariableAccess]()
  var ignoredAccesses = SortedSet[String]()

  def clear = {
    accesses = HashMap[String, VariableAccess]()
    ignoredAccesses = (0 to Knowledge.dimensionality + 2 /* FIXME: find a way to determine max dimensionality */ ).map(dim => dimToString(dim)).to[SortedSet]
  }

  this += new Transformation("Searching", {
    case decl : VariableDeclarationStatement =>
      ignoredAccesses += decl.name
      decl
    case access : VariableAccess if !ignoredAccesses.contains(access.name) =>
      accesses.put(access.name, access)
      access
  }, false)
}
