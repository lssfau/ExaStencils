package exastencils.cuda

import exastencils.core._
import exastencils.core.collectors._
import exastencils.data._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.omp._
import exastencils.optimization._
import exastencils.polyhedron._
import exastencils.util._

import scala.annotation._
import scala.collection.mutable._
import scala.collection.{ SortedSet => _, _ }

/**
 * Collection of constants and util functions for the CUDA transformations.
 */
object CudaStrategiesUtils {
  val CUDA_LOOP_ANNOTATION = "CUDALoop"
  val CUDA_LOOP_TRANSFORM_ANNOTATION = "CUDALoopToTransform"

  /**
   * Check if the loop meets some basic conditions for transforming a ForLoopStatement into CUDA code.
   *
   * @param loop the ForLoopStatement that should be checked
   * @return <code>true</code> if the loop meets the conditions; <code>false</code> otherwise
   */
  def verifyCudaLoopSuitability(loop : ForLoopStatement) : Boolean = {
    loop.begin.isInstanceOf[VariableDeclarationStatement] &&
      (loop.end.isInstanceOf[LowerExpression] || loop.end.isInstanceOf[LowerEqualExpression]) &&
      loop.inc.isInstanceOf[AssignmentStatement]
  }

  /**
   * Check if the loop can be parallelized.
   *
   * @param loop the ForLoopStatement that should be checked
   * @return <code>true</code> if it is a parallel loop; <code>false</code> otherwise
   */
  def verifyCudaLoopParallel(loop : ForLoopStatement) : Boolean = {
    loop.inc.isInstanceOf[AssignmentStatement] &&
      loop.inc.asInstanceOf[AssignmentStatement].src.isInstanceOf[IntegerConstant] &&
      IntegerConstant(1).equals(loop.inc.asInstanceOf[AssignmentStatement].src.asInstanceOf[IntegerConstant]) &&
      loop.isInstanceOf[OptimizationHint] && loop.asInstanceOf[OptimizationHint].isParallel
  }

  /**
   * Collect information about the loop variables, lower and upper bounds, and the step size.
   *
   * @param loops the list of ForLoopStatement that should be traversed
   * @return lists of extracted information
   */
  def extractRelevantLoopInformation(loops : ListBuffer[ForLoopStatement]) = {
    var loopVariables = ListBuffer[String]()
    var lowerBounds = ListBuffer[Expression]()
    var upperBounds = ListBuffer[Expression]()
    var stepSize = ListBuffer[Expression]()

    loops foreach { loop =>
      val loopDeclaration = loop.begin.asInstanceOf[VariableDeclarationStatement]
      loopVariables += loopDeclaration.name
      lowerBounds += loopDeclaration.expression.get
      upperBounds += (loop.end match {
        case l : LowerExpression => l.right
        case e : LowerEqualExpression => e.right
        case o => o
      })
      stepSize += (loop.inc match {
        case AssignmentStatement(_, src : Expression, "=") => src
        case _ => new IntegerConstant(1)
      })
    }

    (loopVariables, lowerBounds, upperBounds, stepSize)
  }
}

/**
 * This transformation annotates LoopOverDimensions and LoopOverDimensions enclosed within ContractingLoops.
 * Additionally required statements for memory transfer are added.
 */
object PrepareCudaRelevantCode extends DefaultStrategy("Prepare CUDA relevant code by adding memory transfer statements " +
  "and annotating for later kernel transformation") {
  val collector = new FctNameCollector
  this.register(collector)

  /**
   * Add statements for memory transfer between host and device to the original statement and install switch to device if host or device code should be executed.
   *
   * @param originalStatement the original statement that should be enriched
   * @param originalLoop the loop that is part of the original statement
   * @return the enriched statements
   */
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
      originalLoop.annotate(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION)
      originalLoop.annotate(CudaStrategiesUtils.CUDA_LOOP_TRANSFORM_ANNOTATION)
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

  this += new Transformation("Processing ContractingLoop and LoopOverDimensions nodes", {
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
 * This transformation is used to calculate the annotations for nested loops of a CUDA loop.
 */
object CalculateAnnotationsOfNestedCudaLoops extends DefaultStrategy("Calculate the annotations for nested loops of a CUDA annotated loop") {
  val collector = new FctNameCollector
  this.register(collector)

  /**
   * Just annotate inner loops as CUDA loop.
   *
   * @param loop the parent loop that should be annotated and traversed
   */
  def annotateInnerCudaLoops(loop : ForLoopStatement) : Unit = {
    loop.annotate(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION)
    loop.body.foreach {
      case x : ForLoopStatement =>
        annotateInnerCudaLoops(x)
      case _ =>
    }
  }

  /**
   * Annotate the inner loops of a ForLoopStatement with CUDA annotations and at the same time calculate the extrema for the loop bounds that are required later.
   *
   * @param loop the loop that should be inspected
   * @param extremaMap a map containing the extrema for some loop variables
   */
  def calculateLoopAnnotations(loop : ForLoopStatement, toKernel : Boolean = true, extremaMap : mutable.HashMap[String, (Long, Long)] = mutable.HashMap[String, (Long, Long)]()) : Unit = {
    loop.annotate(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION)

    if (CudaStrategiesUtils.verifyCudaLoopSuitability(loop)) {
      try {
        val (loopVariables, lowerBounds, upperBounds, _) = CudaStrategiesUtils.extractRelevantLoopInformation(ListBuffer(loop))
        extremaMap.put(loopVariables.head, (SimplifyExpression.evalIntegralExtrema(lowerBounds.head, extremaMap)_1, SimplifyExpression.evalIntegralExtrema(upperBounds.head, extremaMap)_2))
        loop.annotate(SimplifyExpression.EXTREMA_MAP, extremaMap)

        var shouldInnersBeParallel = toKernel
        if (CudaStrategiesUtils.verifyCudaLoopParallel(loop) && toKernel) {
          loop.annotate(CudaStrategiesUtils.CUDA_LOOP_TRANSFORM_ANNOTATION)
          shouldInnersBeParallel = false
        }

        loop.body.filter(x => x.isInstanceOf[ForLoopStatement]).foreach {
          case innerLoop : ForLoopStatement =>
            calculateLoopAnnotations(innerLoop, shouldInnersBeParallel, extremaMap)
          case _ =>
        }
      } catch {
        case e : EvaluationException =>
          Logger.warning(s"""Error annotating the inner loops! Failed to calculate bounds extrema: '${e.msg}'""")
          annotateInnerCudaLoops(loop)
      }
    } else {
      annotateInnerCudaLoops(loop)
    }
  }

  this += new Transformation("Processing ForLoopStatement nodes", {
    case loop : ForLoopStatement if loop.hasAnnotation(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION) && loop
      .hasAnnotation(CudaStrategiesUtils.CUDA_LOOP_TRANSFORM_ANNOTATION) =>
      calculateLoopAnnotations(loop)
      loop
  }, false)
}

/**
 * This transformation is used to convert annotated code into CUDA kernel code.
 */
object ExtractHostAndDeviceCode extends DefaultStrategy("Transform annotated CUDA loop in kernel code") {
  val collector = new FctNameCollector
  this.register(collector)

  /**
   * Calculate the collapsing loops beginning with the innermost loop that is free to collapse with the surrounding loops.
   *
   * @param loop the outer loop
   * @return list of collapsing loops starting with the innermost loop
   */
  def calculateCollapsingLoops(loop : ForLoopStatement) : ListBuffer[ForLoopStatement] = {
    val innerLoopCandidate = loop.body.head
    val loops = ListBuffer[ForLoopStatement](loop)

    innerLoopCandidate match {
      case innerLoop : ForLoopStatement if loop.body.size == 1 &&
        CudaStrategiesUtils.verifyCudaLoopSuitability(innerLoop) =>
        calculateCollapsingLoops(innerLoop) ++ loops
      case _ => loops
    }
  }

  /**
   * Trim the loop headers to have the inner loop body to spare.
   *
   * @param body the statements that should be traversed
   * @param surplus the for loop statements that should be removed
   * @return the inner body without loop headers
   */
  @tailrec
  def pruneKernelBody(body : ListBuffer[Statement], surplus : ListBuffer[ForLoopStatement]) : ListBuffer[Statement] = {
    if (surplus.isEmpty || !(body.head.isInstanceOf[ForLoopStatement] && (body.head equals surplus.head))) {
      body
    } else {
      pruneKernelBody(body.head.asInstanceOf[ForLoopStatement].body, surplus.tail)
    }
  }

  this += new Transformation("Processing ForLoopStatement nodes", {
    case loop : ForLoopStatement if loop.hasAnnotation(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION) &&
      loop.hasAnnotation(CudaStrategiesUtils.CUDA_LOOP_TRANSFORM_ANNOTATION) &&
      CudaStrategiesUtils.verifyCudaLoopSuitability(loop) &&
      CudaStrategiesUtils.verifyCudaLoopParallel(loop) =>

      // remove the annotation first to guarantee single application of this transformation.
      loop.removeAnnotation(CudaStrategiesUtils.CUDA_LOOP_TRANSFORM_ANNOTATION)

      val innerLoops = calculateCollapsingLoops(loop)
      val (loopVariables, lowerBounds, upperBounds, stepSize) = CudaStrategiesUtils.extractRelevantLoopInformation(innerLoops)
      val kernelBody = pruneKernelBody(ListBuffer[Statement](loop), innerLoops.reverse)
      val deviceStatements = ListBuffer[Statement]()

      // add kernel and kernel call
      val kernelFunctions = StateManager.findFirst[KernelFunctions]().get

      // collect local variable accesses because these variables need to be passed to the kernel at call
      GatherLocalVariableAccesses.clear()
      GatherLocalVariableAccesses.applyStandalone(new Scope(loop))
      val variableAccesses = GatherLocalVariableAccesses.accesses.toSeq.sortBy(_._1).map(_._2).to[ListBuffer]

      var extremaMap = mutable.HashMap[String, (Long, Long)]()

      if (loop.hasAnnotation(SimplifyExpression.EXTREMA_MAP)) {
        extremaMap = loop.getAnnotation(SimplifyExpression.EXTREMA_MAP).get.asInstanceOf[mutable.HashMap[String, (Long, Long)]]
      }

      val kernel = ExpKernel(
        kernelFunctions.getIdentifier(collector.getCurrentName),
        variableAccesses,
        loopVariables,
        lowerBounds,
        upperBounds,
        stepSize,
        kernelBody,
        loop.reduction,
        extremaMap)

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
    case kernel : ExpKernel =>
      while (kernel.dimensionality > Platform.hw_cuda_maxNumDimsBlock) {
        def it = new VariableAccess(ExpKernel.KernelVariablePrefix + dimToString(kernel.dimensionality - 1), Some(IntegerDatatype))
        kernel.body = ListBuffer[Statement](ForLoopStatement(
          new VariableDeclarationStatement(it, kernel.lowerBounds.last),
          LowerExpression(it, kernel.upperBounds.last),
          AssignmentStatement(it, kernel.stepSize.last, "+="),
          kernel.body))
        kernel.dimensionality -= 1
      }
      kernel
  })
}

object HandleKernelReductions extends DefaultStrategy("Handle reductions in device kernels") {
  this += new Transformation("Process kernel nodes", {
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
    case kernel : ExpKernel if kernel.reduction.isDefined =>
      // update assignments according to reduction clauses
      kernel.evalIndexBounds()
      val index = new MultiIndex((0 until kernel.dimensionality).map(dim =>
        new VariableAccess(dimToString(dim), Some(IntegerDatatype)) : Expression).toArray)

      val stride = (kernel.maxIndices, kernel.minIndices).zipped.map((x, y) => new SubtractionExpression(x, y) : Expression)

      ReplaceReductionAssignements.redTarget = kernel.reduction.get.target.name
      ReplaceReductionAssignements.replacement = ReductionDeviceDataAccess(iv.ReductionDeviceData(new MultiplicationExpression(ListBuffer[Expression](stride : _*))), index, new MultiIndex(stride))
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
