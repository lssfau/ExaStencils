package exastencils.cuda

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.SortedSet

import exastencils.core._
import exastencils.core.collectors.FctNameCollector
import exastencils.data._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.omp._
import exastencils.polyhedron._

object AnnotateLoopsForCUDATransformation extends DefaultStrategy("Annotate loops that should late be transformed " +
  "into CUDA code") {
  this += new Transformation("Annotate LoopOverDimensions nodes", {
    case loop : LoopOverDimensions => {
      loop.annotate("CUDALoop")
      println("Annotate a loop as CUDA loop: " + loop)
      loop
    }
  }, false)
}

object FindAnnotatedCUDALoops extends DefaultStrategy("Find annotated CUDA loops") {
  this += new Transformation("Find CUDA loops", {
    case loop : ForLoopStatement => {
      if (loop.hasAnnotation("CUDALoop"))
        println("This node is an annotated CUDA loop: " + loop)
      loop
    }
  }, true)
}

object TransformAnnotatedCUDALoops extends DefaultStrategy("Find annotated CUDA Loops that should be transformed") {
  val collector = new FctNameCollector
  this.register(collector)

  this += new Transformation("Find CUDA Loops", {
    case loop : ForLoopStatement => {
      var output : OutputType = loop
      if (loop.hasAnnotation("CUDALoop")) {
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

        /// compile device statements
        if (earlyExit) {
          output = hostStmts
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
            kernelFunctions.getIdentifier(collector.getCurrentName),
            variableAccesses,
            1,
            IndexRange(MultiIndex(Array(loop.end)), MultiIndex(Array(loop.end))),
            loop.body,
            loop.reduction,
            None)

          kernelFunctions.addKernel(Duplicate(kernel))

          // process return value of kernel wrapper call if reduction is required
          if (loop.reduction.isDefined) {
            val red = loop.reduction.get
            deviceStmts += AssignmentStatement(red.target,
              BinaryOperators.CreateExpression(red.op, red.target,
                FunctionCallExpression(kernel.getWrapperFctName, variableAccesses.map(_.asInstanceOf[Expression]))))
          } else {
            deviceStmts += FunctionCallExpression(kernel.getWrapperFctName, variableAccesses.map(_.asInstanceOf[Expression]))
          }

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
            case "Performance" => if (loop.getAnnotation("perf_timeEstimate_host").get.asInstanceOf[Double] > loop.getAnnotation("perf_timeEstimate_device").get.asInstanceOf[Double]) 0 else 1 // decide according to performance estimates
          }

          output = ConditionStatement(defaultChoice, hostStmts, deviceStmts)
        }
      }
      output
    }
  }, false)
}

object SplitLoopsAfterPolyForHostAndDevice extends DefaultStrategy("Splitting loops into host and device instances " +
  "after applying polyhedral optimizations") {
  val collector = new FctNameCollector
  this.register(collector)

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
      if (!loop.isInstanceOf[PolyhedronAccessible])
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
          kernelFunctions.getIdentifier(collector.getCurrentName),
          variableAccesses,
          loop.numDimensions,
          loop.indices,
          loop.body,
          loop.reduction,
          loop.condition)

        kernelFunctions.addKernel(Duplicate(kernel))

        // process return value of kernel wrapper call if reduction is required
        if (loop.reduction.isDefined) {
          val red = loop.reduction.get
          deviceStmts += AssignmentStatement(red.target,
            BinaryOperators.CreateExpression(red.op, red.target,
              FunctionCallExpression(kernel.getWrapperFctName, variableAccesses.map(_.asInstanceOf[Expression]))))
        } else {
          deviceStmts += FunctionCallExpression(kernel.getWrapperFctName, variableAccesses.map(_.asInstanceOf[Expression]))
        }

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
          case "Performance" => if (loop.getAnnotation("perf_timeEstimate_host").get.asInstanceOf[Double] > loop.getAnnotation("perf_timeEstimate_device").get.asInstanceOf[Double]) 0 else 1 // decide according to performance estimates
        }

        ConditionStatement(defaultChoice, hostStmts, deviceStmts)
      }
    }
  }, false)
}

object SplitLoopsForHostAndDevice extends DefaultStrategy("Splitting loops into host and device instances") {
  val collector = new FctNameCollector
  this.register(collector)

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
      if (!loop.isInstanceOf[PolyhedronAccessible])
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
          kernelFunctions.getIdentifier(collector.getCurrentName),
          variableAccesses,
          loop.numDimensions,
          loop.indices,
          loop.body,
          loop.reduction,
          loop.condition)

        kernelFunctions.addKernel(Duplicate(kernel))

        // process return value of kernel wrapper call if reduction is required
        if (loop.reduction.isDefined) {
          val red = loop.reduction.get
          deviceStmts += AssignmentStatement(red.target,
            BinaryOperators.CreateExpression(red.op, red.target,
              FunctionCallExpression(kernel.getWrapperFctName, variableAccesses.map(_.asInstanceOf[Expression]))))
        } else {
          deviceStmts += FunctionCallExpression(kernel.getWrapperFctName, variableAccesses.map(_.asInstanceOf[Expression]))
        }

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
          case "Performance" => if (loop.getAnnotation("perf_timeEstimate_host").get.asInstanceOf[Double] > loop.getAnnotation("perf_timeEstimate_device").get.asInstanceOf[Double]) 0 else 1 // decide according to performance estimates
        }

        ConditionStatement(defaultChoice, hostStmts, deviceStmts)
      }
    }
  }, false)
}

object AdaptKernelDimensionalities extends DefaultStrategy("Reduce kernel dimensionality where necessary") {
  this += new Transformation("Process kernel nodes", {
    case kernel : Kernel => {
      while (kernel.numDimensions > Platform.hw_cuda_maxNumDimsBlock) {
        def it = LoopOverDimensions.defItForDim(kernel.numDimensions - 1)
        kernel.body = ListBuffer[Statement](ForLoopStatement(
          new VariableDeclarationStatement(it, kernel.indices.begin.last),
          LowerExpression(it, kernel.indices.end.last),
          AssignmentStatement(it, 1, "+="),
          kernel.body))
        kernel.indices.begin.dropRight(1)
        kernel.indices.end.dropRight(1)
        kernel.numDimensions -= 1
      }
      kernel
    }
  })
}

object HandleKernelReductions extends DefaultStrategy("Handle reductions in device kernels") {
  this += new Transformation("Process kernel nodes", {
    case kernel : Kernel if kernel.reduction.isDefined => {
      // update assignments according to reduction clauses
      val index = LoopOverDimensions.defIt(kernel.numDimensions)
      val stride = (
        LoopOverDimensions.evalMaxIndex(kernel.indices.end, kernel.numDimensions, true),
        LoopOverDimensions.evalMinIndex(kernel.indices.begin, kernel.numDimensions, true)).zipped.map(_ - _)
      ReplaceReductionAssignements.redTarget = kernel.reduction.get.target.name
      ReplaceReductionAssignements.replacement = ReductionDeviceDataAccess(iv.ReductionDeviceData(stride.reduce(_ * _)), index, new MultiIndex(stride))
      ReplaceReductionAssignements.applyStandalone(Scope(kernel.body))
      kernel
    }
  })
}

object ReplaceReductionAssignements extends QuietDefaultStrategy("Replace assignments to reduction targets") {
  var redTarget : String = ""
  var replacement : Expression = NullExpression

  this += new Transformation("Searching", {
    case assignment : AssignmentStatement => {
      assignment.dest match {
        case VariableAccess(redTarget, _) =>
          assignment.dest = Duplicate(replacement)
        // assignment.op = "=" // don't modify assignments - there could be inlined loops
        case _ =>
      }
      assignment
    }
  })
}

object GatherLocalFieldAccess extends QuietDefaultStrategy("Gathering local FieldAccess nodes") {
  var fieldAccesses = HashMap[String, FieldAccessLike]()
  var inWriteOp = false

  def mapFieldAccess(access : FieldAccessLike) = {
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
    case access : FieldAccessLike =>
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
