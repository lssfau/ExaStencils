package exastencils.cuda

import exastencils.core._
import exastencils.core.collectors._
import exastencils.data._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.omp._
import exastencils.optimization._
import exastencils.polyhedron._

import scala.annotation._
import scala.collection.mutable._
import scala.collection.{ SortedSet => _, _ }

/**
 * This transformation annotates LoopOverDimensions and LoopOverDimensions enclosed with ContractingLoops.
 * Additionally required statements for memory transfer are added. This transformation is applied before resolving
 * ContractingLoops to guarantee that memory transfer statements appear only before and after a resolved
 * ContractingLoop (required for temporal blocking).
 */
object PrepareCudaRelevantCode extends DefaultStrategy("Prepare CUDA relevant code") {
  val CudaLoopAnnotation = "CUDALoop"
  val CudaLoopTransformAnnotation = "CUDALoopToTransform"
  val collector = new FctNameCollector
  this.register(collector)

  def addMemoryTransferStatements(originalStatement : Statement, originalLoop : LoopOverDimensions) : OutputType = {
    GatherLocalFieldAccess.fieldAccesses.clear
    GatherLocalFieldAccess.applyStandalone(Scope(originalLoop.body))

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

    // add original statement
    hostStmts += Duplicate(originalStatement)

    // update flags for written fields
    for (access <- GatherLocalFieldAccess.fieldAccesses.toSeq.sortBy(_._1)) {
      val fieldSelection = access._2.fieldSelection
      if (access._1.startsWith("write"))
        hostStmts += AssignmentStatement(iv.HostDataUpdated(fieldSelection.field, fieldSelection.slot), BooleanConstant(true))
    }

    // every LoopOverDimensions statement is potentially worse to transform in CUDA code
    // Exceptions:
    // 1. this loop is a special one and cannot be optimized in polyhedral model
    // 2. this loop has no parallel potential
    // use the host for dealing with the two exceptional cases
    val cudaSuitable = originalLoop.isInstanceOf[PolyhedronAccessible] && originalLoop.isInstanceOf[OMP_PotentiallyParallel]

    /// compile device statements
    if (!cudaSuitable) {
      hostStmts
    } else {
      originalLoop.annotate(CudaLoopAnnotation)
      originalLoop.annotate(CudaLoopTransformAnnotation)
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

      deviceStmts += Duplicate(originalStatement)

      if (Knowledge.experimental_cuda_syncDeviceAfterKernelCalls)
        deviceStmts += CUDA_DeviceSynchronize()

      // update flags for written fields
      for (access <- GatherLocalFieldAccess.fieldAccesses.toSeq.sortBy(_._1)) {
        val fieldSelection = access._2.fieldSelection
        if (access._1.startsWith("write"))
          deviceStmts += AssignmentStatement(iv.DeviceDataUpdated(fieldSelection.field, fieldSelection.slot), BooleanConstant(true))
      }

      /// compile final switch
      val defaultChoice = Knowledge.experimental_cuda_preferredExecution match {
        case "Host" => 1 // CPU by default
        case "Device" => 0 // GPU by default
        case "Performance" => if (originalLoop.getAnnotation("perf_timeEstimate_host").get.asInstanceOf[Double] > originalLoop.getAnnotation("perf_timeEstimate_device").get.asInstanceOf[Double]) 0 else 1 // decide according to performance estimates
      }

      ConditionStatement(defaultChoice, hostStmts, deviceStmts)
    }
  }

  this += new Transformation("Prepare CUDA relevant code by adding memory transfer statements and annotating for " +
    "later kernel transformation", {
    case contractingLoop : ContractingLoop =>
      val content = contractingLoop.statements.find(s =>
        s.isInstanceOf[ConditionStatement] || s.isInstanceOf[LoopOverDimensions])

      content match {
        case Some(ConditionStatement(cond, ListBuffer(loop : LoopOverDimensions), ListBuffer())) =>
          addMemoryTransferStatements(contractingLoop, loop)
        case Some(loop : LoopOverDimensions) =>
          addMemoryTransferStatements(contractingLoop, loop)
        case None => contractingLoop
      }
    case loop : LoopOverDimensions =>
      addMemoryTransferStatements(loop, loop)
  }, false)
}

/**
 * This transformation is used to convert annotated code into CUDA kernel code and is applied after the polyhedral
 * optimization to work on optimized for loops.
 */
object ExtractHostAndDeviceCode extends DefaultStrategy("Transform annotated CUDA loop in kernel code") {
  val collector = new FctNameCollector
  this.register(collector)

  def verifyLoopSuitability(loop : ForLoopStatement) : Boolean = {
    loop.begin.isInstanceOf[VariableDeclarationStatement] &&
      (loop.end.isInstanceOf[LowerExpression] || loop.end.isInstanceOf[LowerEqualExpression]) &&
      loop.inc.isInstanceOf[AssignmentStatement] &&
      loop.inc.asInstanceOf[AssignmentStatement].src.isInstanceOf[IntegerConstant] &&
      loop.inc.asInstanceOf[AssignmentStatement].src.asInstanceOf[IntegerConstant].value == 1 &&
      loop.isInstanceOf[OptimizationHint] && loop.asInstanceOf[OptimizationHint].isParallel
  }

  def calculateCollapsingLoops(loop : ForLoopStatement, recursionDepth : Int = 1) : ListBuffer[ForLoopStatement] = {
    val innerLoopCandidate = loop.body.head
    val loops = ListBuffer[ForLoopStatement](loop)

    innerLoopCandidate match {
      case innerLoop : ForLoopStatement if recursionDepth < Platform.hw_cuda_maxNumDimsBlock && loop.body.size == 1 &&
        verifyLoopSuitability(innerLoop) =>
        loops ++ calculateCollapsingLoops(innerLoop, recursionDepth + 1)
      case _ => loops
    }
  }

  @tailrec
  def pruneKernelBody(body : ListBuffer[Statement], surplus : ListBuffer[ForLoopStatement]) : ListBuffer[Statement] = {
    if (surplus.isEmpty || !(body.head.isInstanceOf[ForLoopStatement] && (body.head equals surplus.head))) {
      body
    } else {
      pruneKernelBody(body.head.asInstanceOf[ForLoopStatement].body, surplus.tail)
    }
  }

  def createLoopBounds(loops : ListBuffer[ForLoopStatement]) = {
    var loopVariables = ListBuffer[String]()
    var lowerBounds = ListBuffer[Expression]()
    var upperBounds = ListBuffer[Expression]()

    loops foreach { loop =>
      val loopDeclaration = loop.begin.asInstanceOf[VariableDeclarationStatement]
      loopVariables += loopDeclaration.name
      lowerBounds += loopDeclaration.expression.get
      upperBounds += (loop.end match {
        case l : LowerExpression => l.right
        case e : LowerEqualExpression => e.right
        case o => o
      })
    }

    (loopVariables, lowerBounds, upperBounds)
  }

  this += new Transformation("Transform annotated CUDA loops in CUDA kernel code", {
    case loop : ForLoopStatement if loop.hasAnnotation(PrepareCudaRelevantCode.CudaLoopAnnotation) && loop
      .hasAnnotation(PrepareCudaRelevantCode.CudaLoopTransformAnnotation) && verifyLoopSuitability(loop) =>

      // remove the annotation first to guarantee single application of this transformation.
      loop.removeAnnotation(PrepareCudaRelevantCode.CudaLoopTransformAnnotation)

      val innerLoops = calculateCollapsingLoops(loop)
      val (loopVariables, lowerBounds, upperBounds) = createLoopBounds(innerLoops)
      val kernelBody = pruneKernelBody(ListBuffer[Statement](loop), innerLoops)
      val deviceStatements = ListBuffer[Statement]()

      // add kernel and kernel call
      val kernelFunctions = StateManager.findFirst[KernelFunctions]().get

      // collect local variable accesses because these variables need to be passed to the kernel at call
      GatherLocalVariableAccesses.clear()
      GatherLocalVariableAccesses.applyStandalone(new Scope(loop))
      val variableAccesses = GatherLocalVariableAccesses.accesses.toSeq.sortBy(_._1).map(_._2).to[ListBuffer]

      val kernel = ExpKernel(
        kernelFunctions.getIdentifier(collector.getCurrentName),
        variableAccesses,
        loopVariables,
        lowerBounds,
        upperBounds,
        kernelBody,
        loop.reduction)

      kernelFunctions.addKernel(Duplicate(kernel))

      // process return value of kernel wrapper call if reduction is required
      if (loop.reduction.isDefined) {
        val red = loop.reduction.get
        deviceStatements += AssignmentStatement(red.target,
          BinaryOperators.CreateExpression(red.op, red.target,
            FunctionCallExpression(kernel.getWrapperFctName, variableAccesses.map(_.asInstanceOf[Expression]))))
      } else {
        deviceStatements += FunctionCallExpression(kernel.getWrapperFctName, variableAccesses.map(_.asInstanceOf[Expression]))
      }

      deviceStatements
  }, false)
}

object SplitLoopsForHostAndDevice extends DefaultStrategy("Splitting loops into host and device instances") {
  val collector = new FctNameCollector
  this.register(collector)

  this += new Transformation("Processing LoopOverDimensions nodes", {
    case loop : LoopOverDimensions =>
      // don't filter here - memory transfer code is still required
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

        GatherLocalVariableAccesses.clear()
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
        val defaultChoice = Knowledge.experimental_cuda_preferredExecution match {
          case "Host" => 1 // CPU by default
          case "Device" => 0 // GPU by default
          case "Performance" => if (loop.getAnnotation("perf_timeEstimate_host").get.asInstanceOf[Double] > loop.getAnnotation("perf_timeEstimate_device").get.asInstanceOf[Double]) 0 else 1 // decide according to performance estimates
        }

        ConditionStatement(defaultChoice, hostStmts, deviceStmts)
      }
  }, false)
}

object AdaptKernelDimensionalities extends DefaultStrategy("Reduce kernel dimensionality where necessary") {
  this += new Transformation("Process kernel nodes", {
    case kernel : Kernel =>
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
  })
}

object HandleKernelReductions extends DefaultStrategy("Handle reductions in device kernels") {
  this += new Transformation("Process kernel nodes", {
    case kernel : ExpKernel if kernel.reduction.isDefined =>
      // update assignments according to reduction clauses
      kernel.evalIndexBounds()
      val index = new MultiIndex((0 until kernel.executionConfigurationDimensionality).map(dim =>
        new VariableAccess(dimToString(dim), Some(IntegerDatatype)) : Expression).toArray)

      val stride = (kernel.maxIndices, kernel.minIndices).zipped.map(_ - _)
      ReplaceReductionAssignements.redTarget = kernel.reduction.get.target.name
      ReplaceReductionAssignements.replacement = ReductionDeviceDataAccess(iv.ReductionDeviceData(stride.product), index, new MultiIndex(stride))
      ReplaceReductionAssignements.applyStandalone(Scope(kernel.body))
      kernel
    case kernel : Kernel if kernel.reduction.isDefined =>
      // update assignments according to reduction clauses
      val index = LoopOverDimensions.defIt(kernel.numDimensions)
      val stride = (
        LoopOverDimensions.evalMaxIndex(kernel.indices.end, kernel.numDimensions, printWarnings = true),
        LoopOverDimensions.evalMinIndex(kernel.indices.begin, kernel.numDimensions, printWarnings = true)).zipped.map(_ - _)
      ReplaceReductionAssignements.redTarget = kernel.reduction.get.target.name
      ReplaceReductionAssignements.replacement = ReductionDeviceDataAccess(iv.ReductionDeviceData(stride.product), index, new MultiIndex(stride))
      ReplaceReductionAssignements.applyStandalone(Scope(kernel.body))
      kernel
  })
}

object ReplaceReductionAssignements extends QuietDefaultStrategy("Replace assignments to reduction targets") {
  var redTarget : String = ""
  var replacement : Expression = NullExpression

  this += new Transformation("Searching", {
    case assignment : AssignmentStatement =>
      assignment.dest match {
        case VariableAccess(redTarget, _) =>
          assignment.dest = Duplicate(replacement)
        // assignment.op = "=" // don't modify assignments - there could be inlined loops
        case _ =>
      }
      assignment
  })
}

object GatherLocalFieldAccess extends QuietDefaultStrategy("Gathering local FieldAccess nodes") {
  var fieldAccesses = mutable.HashMap[String, FieldAccessLike]()
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
        case _ => identifier += s"_s${access.fieldSelection.slot.prettyprint}"
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
  var accesses = mutable.HashMap[String, VariableAccess]()
  var ignoredAccesses = mutable.SortedSet[String]()

  def clear() = {
    accesses = mutable.HashMap[String, VariableAccess]()
    ignoredAccesses = (0 to Knowledge.dimensionality + 2 /* FIXME: find a way to determine max dimensionality */ ).map(dim => dimToString(dim)).to[mutable.SortedSet]
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
