package exastencils.cuda

import scala.annotation._
import scala.collection.mutable._
import scala.collection.{ Set, SortedSet => _, _ }

import exastencils.base.ir._
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

/**
  * Collection of constants and util functions for the CUDA transformations.
  */
object CudaStrategiesUtils {
  val CUDA_LOOP_ANNOTATION = "CUDALoop"
  val CUDA_BAND_START = "CUDABandStart"
  val CUDA_BAND_PART = "CUDABandPart"
  val CUDA_INNER = "CUDAInner"
  val CUDA_BODY_DECL = "CUDABodyDeclarations"

  /**
    * Check if the loop meets some basic conditions for transforming a ForLoopStatement into CUDA code.
    *
    * @param loop the ForLoopStatement that should be checked
    * @return <code>true</code> if the loop meets the conditions; <code>false</code> otherwise
    */
  def verifyCudaLoopSuitability(loop : ForLoopStatement) : Boolean = {
    loop.begin.isInstanceOf[VariableDeclarationStatement] &&
      (loop.end.isInstanceOf[IR_LowerExpression] || loop.end.isInstanceOf[IR_LowerEqualExpression]) &&
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
      loop.inc.asInstanceOf[AssignmentStatement].src.isInstanceOf[IR_IntegerConstant] &&
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
    var lowerBounds = ListBuffer[IR_Expression]()
    var upperBounds = ListBuffer[IR_Expression]()
    var stepSize = ListBuffer[IR_Expression]()

    loops foreach { loop =>
      val loopDeclaration = loop.begin.asInstanceOf[VariableDeclarationStatement]
      loopVariables += loopDeclaration.name
      lowerBounds += loopDeclaration.expression.get
      upperBounds += (loop.end match {
        case l : IR_LowerExpression      =>
          l.right
        case e : IR_LowerEqualExpression =>
          IR_AdditionExpression(e.right, IR_IntegerConstant(1))
        case o                           => o
      })
      stepSize += (loop.inc match {
        case AssignmentStatement(_, src : IR_Expression, "=") => src
        case _                                                => IR_IntegerConstant(1)
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

  def getHostDeviceSyncStmts(loop : LoopOverDimensions, isParallel : Boolean) = {
    val (beforeHost, afterHost) = (ListBuffer[IR_Statement](), ListBuffer[IR_Statement]())
    val (beforeDevice, afterDevice) = (ListBuffer[IR_Statement](), ListBuffer[IR_Statement]())
    // don't filter here - memory transfer code is still required
    GatherLocalFieldAccess.fieldAccesses.clear
    GatherLocalFieldAccess.applyStandalone(Scope(loop.body))

    // host sync stmts

    // add data sync statements
    for (access <- GatherLocalFieldAccess.fieldAccesses.toSeq.sortBy(_._1)) {
      var sync = true
      if (access._1.startsWith("write") && !Knowledge.cuda_syncHostForWrites)
        sync = false // skip write accesses if demanded
      if (access._1.startsWith("write") && GatherLocalFieldAccess.fieldAccesses.contains("read" + access._1.substring("write".length)))
        sync = false // skip write access for read/write accesses
      if (sync)
        beforeHost += CUDA_UpdateHostData(Duplicate(access._2)).expand().inner // expand here to avoid global expand afterwards
    }

    // update flags for written fields
    for (access <- GatherLocalFieldAccess.fieldAccesses.toSeq.sortBy(_._1)) {
      val fieldSelection = access._2.fieldSelection
      if (access._1.startsWith("write"))
        afterHost += AssignmentStatement(iv.HostDataUpdated(fieldSelection.field, fieldSelection.slot), IR_BooleanConstant(true))
    }

    // device sync stmts

    if (isParallel) {
      // before: add data sync statements
      for (access <- GatherLocalFieldAccess.fieldAccesses.toSeq.sortBy(_._1)) {
        var sync = true
        if (access._1.startsWith("write") && !Knowledge.cuda_syncDeviceForWrites)
          sync = false // skip write accesses if demanded
        if (access._1.startsWith("write") && GatherLocalFieldAccess.fieldAccesses.contains("read" + access._1.substring("write".length)))
          sync = false // skip write access for read/write accesses
        if (sync)
          beforeDevice += CUDA_UpdateDeviceData(Duplicate(access._2)).expand().inner // expand here to avoid global expand afterwards
      }

      if (Knowledge.cuda_syncDeviceAfterKernelCalls)
        afterDevice += CUDA_DeviceSynchronize()

      // update flags for written fields
      for (access <- GatherLocalFieldAccess.fieldAccesses.toSeq.sortBy(_._1)) {
        val fieldSelection = access._2.fieldSelection
        if (access._1.startsWith("write"))
          afterDevice += AssignmentStatement(iv.DeviceDataUpdated(fieldSelection.field, fieldSelection.slot), IR_BooleanConstant(true))
      }
    }

    (beforeHost, afterHost, beforeDevice, afterDevice)
  }

  def addHostDeviceBranching(hostStmts : ListBuffer[IR_Statement], deviceStmts : ListBuffer[IR_Statement], loop : LoopOverDimensions, earlyExit : Boolean) : ListBuffer[IR_Statement] = {
    if (earlyExit) {
      hostStmts
    } else {
      /// compile final switch
      val defaultChoice = Knowledge.cuda_preferredExecution match {
        case "Host"        => 1 // CPU by default
        case "Device"      => 0 // GPU by default
        case "Performance" => if (loop.getAnnotation("perf_timeEstimate_host").get.asInstanceOf[Double] > loop.getAnnotation("perf_timeEstimate_device").get.asInstanceOf[Double]) 0 else 1 // decide according to performance estimates
      }

      ListBuffer[IR_Statement](ConditionStatement(defaultChoice, hostStmts, deviceStmts))
    }
  }

  this += new Transformation("Processing ContractingLoop and LoopOverDimensions nodes", {
    case cl : ContractingLoop      =>
      var hostStmts = new ListBuffer[IR_Statement]()
      var deviceStmts = new ListBuffer[IR_Statement]()
      val fieldOffset = new mutable.HashMap[(String, Int), Int]()
      val fields = new mutable.HashMap[(String, Int), Field]()
      var hostCondStmt : ConditionStatement = null
      var deviceCondStmt : ConditionStatement = null

      val containedLoop = cl.statements.find(s =>
        s.isInstanceOf[ConditionStatement] || s.isInstanceOf[LoopOverDimensions]) match {
        case Some(ConditionStatement(cond, trueBody : ListBuffer[IR_Statement], ListBuffer())) =>
          val bodyWithoutComments = trueBody.filterNot(x => x.isInstanceOf[CommentStatement])
          bodyWithoutComments match {
            case ListBuffer(loop : LoopOverDimensions) => loop
            case _                                     => LoopOverDimensions(0, IndexRange(new MultiIndex(), new MultiIndex()), ListBuffer[IR_Statement]())
          }
        case Some(loop : LoopOverDimensions)                                                   =>
          loop
        case None                                                                              => LoopOverDimensions(0, IndexRange(new MultiIndex(), new MultiIndex()), ListBuffer[IR_Statement]())
      }

      // every LoopOverDimensions statement is potentially worse to transform in CUDA code
      // Exceptions:
      // 1. this loop is a special one and cannot be optimized in polyhedral model
      // 2. this loop has no parallel potential
      // use the host for dealing with the two exceptional cases
      val isParallel = containedLoop.isInstanceOf[PolyhedronAccessible] && containedLoop.isInstanceOf[OMP_PotentiallyParallel]

      // calculate memory transfer statements for host and device
      val (beforeHost, afterHost, beforeDevice, afterDevice) = getHostDeviceSyncStmts(containedLoop, isParallel)

      hostStmts ++= beforeHost
      deviceStmts ++= beforeDevice

      // resolve contracting loop
      for (i <- 1 to cl.number)
        for (stmt <- cl.statements)
          stmt match {
            case AdvanceSlotStatement(iv.CurrentSlot(field, fragment)) =>
              val fKey = (field.identifier, field.level)
              fieldOffset(fKey) = fieldOffset.getOrElse(fKey, 0) + 1
              fields(fKey) = field

            case cStmt @ ConditionStatement(cond, trueBody : ListBuffer[IR_Statement], ListBuffer()) =>
              val bodyWithoutComments = trueBody.filterNot(x => x.isInstanceOf[CommentStatement])
              bodyWithoutComments match {
                case ListBuffer(l : LoopOverDimensions) =>
                  val nju = cl.processLoopOverDimensions(l, cl.number - i, fieldOffset)

                  if (hostCondStmt == null || cond != hostCondStmt.condition) {
                    hostCondStmt = Duplicate(cStmt)
                    hostCondStmt.trueBody.clear()
                    deviceCondStmt = Duplicate(hostCondStmt)
                    hostStmts += hostCondStmt
                    deviceStmts += deviceCondStmt
                  }
                  hostCondStmt.trueBody += nju

                  if (isParallel) {
                    val njuCuda = Duplicate(nju)
                    njuCuda.annotate(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION, collector.getCurrentName)
                    deviceCondStmt.trueBody += njuCuda
                  }
                case _                                  =>
              }
            case l : LoopOverDimensions                                                              =>
              val loop = cl.processLoopOverDimensions(l, cl.number - i, fieldOffset)
              hostStmts += loop

              if (isParallel) {
                val loopCuda = Duplicate(loop)
                loopCuda.annotate(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION, collector.getCurrentName)
                deviceStmts += loopCuda
              }
          }

      hostStmts ++= afterHost
      deviceStmts ++= afterDevice

      val res = addHostDeviceBranching(hostStmts, deviceStmts, containedLoop, !isParallel)

      for ((fKey, offset) <- fieldOffset) {
        val field = fields(fKey)
        res += AssignmentStatement(iv.CurrentSlot(field), (iv.CurrentSlot(field) + offset) Mod field.numSlots)
      }

      res
    case loop : LoopOverDimensions =>
      val hostStmts = ListBuffer[IR_Statement]()
      val deviceStmts = ListBuffer[IR_Statement]()

      // every LoopOverDimensions statement is potentially worse to transform in CUDA code
      // Exceptions:
      // 1. this loop is a special one and cannot be optimized in polyhedral model
      // 2. this loop has no parallel potential
      // use the host for dealing with the two exceptional cases
      val isParallel = loop.isInstanceOf[PolyhedronAccessible] && loop.isInstanceOf[OMP_PotentiallyParallel]

      // calculate memory transfer statements for host and device
      val (beforeHost, afterHost, beforeDevice, afterDevice) = getHostDeviceSyncStmts(loop, isParallel)

      hostStmts ++= beforeHost
      deviceStmts ++= beforeDevice

      hostStmts += loop
      if (isParallel) {
        val loopCuda = Duplicate(loop)
        loopCuda.annotate(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION, collector.getCurrentName)
        deviceStmts += loopCuda
      }

      hostStmts ++= afterHost
      deviceStmts ++= afterDevice

      addHostDeviceBranching(hostStmts, deviceStmts, loop, !isParallel)
  }, false)
}

/**
  * This transformation is used to calculate the annotations for CUDA loops.
  */
object CalculateCudaLoopsAnnotations extends DefaultStrategy("Calculate the annotations for CUDA loops") {
  val collector = new FctNameCollector
  this.register(collector)

  /**
    * Annotate the inner CUDA loops.
    *
    * @param loop the loop to traverse
    */
  def annotateInnerCudaLoops(loop : ForLoopStatement) : Unit = {
    loop.body.foreach {
      case x : ForLoopStatement =>
        x.annotate(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION, CudaStrategiesUtils.CUDA_INNER)
        annotateInnerCudaLoops(x)
      case _                    =>
    }
  }

  /**
    * Calculate the CUDA loops that are part of the band.
    *
    * @param extremaMap the map with the extrema for the loop iterators
    * @param loop       the current loop to traverse
    */
  def calculateLoopsInBand(extremaMap : mutable.HashMap[String, (Long, Long)], loop : ForLoopStatement) : Unit = {
    loop.body.head match {
      case innerLoop : ForLoopStatement if loop.body.size == 1 &&
        CudaStrategiesUtils.verifyCudaLoopSuitability(innerLoop) && CudaStrategiesUtils.verifyCudaLoopParallel(innerLoop) =>
        try {
          val (loopVariables, lowerBounds, upperBounds, _) = CudaStrategiesUtils.extractRelevantLoopInformation(ListBuffer(innerLoop))
          var loopDependsOnSurroundingIterators = false
          FindSurroundingLoopIteratorUsages.loopIterators = extremaMap.keySet
          FindSurroundingLoopIteratorUsages.usedLoopIterators.clear()
          FindSurroundingLoopIteratorUsages.applyStandalone(new Scope(IR_ExpressionStatement(lowerBounds.head)))
          loopDependsOnSurroundingIterators |= FindSurroundingLoopIteratorUsages.usedLoopIterators.nonEmpty
          FindSurroundingLoopIteratorUsages.usedLoopIterators.clear()
          FindSurroundingLoopIteratorUsages.applyStandalone(new Scope(IR_ExpressionStatement(upperBounds.head)))
          loopDependsOnSurroundingIterators |= FindSurroundingLoopIteratorUsages.usedLoopIterators.nonEmpty

          extremaMap.put(loopVariables.head, (SimplifyExpression.evalIntegralExtrema(lowerBounds.head, extremaMap) _1, SimplifyExpression.evalIntegralExtrema(upperBounds.head, extremaMap) _2))
          innerLoop.annotate(SimplifyExpression.EXTREMA_MAP, extremaMap)

          if (loopDependsOnSurroundingIterators) {
            innerLoop.annotate(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION, CudaStrategiesUtils.CUDA_INNER)
            annotateInnerCudaLoops(innerLoop)
          } else {
            innerLoop.annotate(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION, CudaStrategiesUtils.CUDA_BAND_PART)
            calculateLoopsInBand(extremaMap, innerLoop)
          }
        } catch {
          case e : EvaluationException =>
            Logger.warning(s"""Error annotating the inner loops! Failed to calculate bounds extrema: '${ e.msg }'""")
            innerLoop.annotate(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION, CudaStrategiesUtils.CUDA_INNER)
            annotateInnerCudaLoops(innerLoop)
        }
      case _                                                                                                              =>
        loop.body.foreach {
          case x : ForLoopStatement =>
            annotateInnerCudaLoops(x)
          case _                    =>
        }
    }
  }

  /**
    * Calculate extrema values for the loop iterators and annotate loop with adequate CUDA loop annotation.
    *
    * @param extremaMap the map containing the extrema values for the loop iterators
    * @param loop       the loop to traverse
    */
  def updateLoopAnnotations(extremaMap : mutable.HashMap[String, (Long, Long)], loop : ForLoopStatement, bodyDecl : ListBuffer[IR_Statement] = ListBuffer[IR_Statement]()) : Unit = {
    if (CudaStrategiesUtils.verifyCudaLoopSuitability(loop)) {
      try {
        val (loopVariables, lowerBounds, upperBounds, _) = CudaStrategiesUtils.extractRelevantLoopInformation(ListBuffer(loop))
        extremaMap.put(loopVariables.head, (SimplifyExpression.evalIntegralExtrema(lowerBounds.head, extremaMap) _1, SimplifyExpression.evalIntegralExtrema(upperBounds.head, extremaMap) _2))
        loop.annotate(SimplifyExpression.EXTREMA_MAP, extremaMap)

        if (CudaStrategiesUtils.verifyCudaLoopParallel(loop)) {
          loop.annotate(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION, CudaStrategiesUtils.CUDA_BAND_START)
          loop.annotate(CudaStrategiesUtils.CUDA_BODY_DECL, bodyDecl)

          calculateLoopsInBand(extremaMap, loop)
        } else {
          val innerLoops : ListBuffer[ForLoopStatement] = loop.body.filter(x => x.isInstanceOf[ForLoopStatement]).asInstanceOf[ListBuffer[ForLoopStatement]]

          // if there is no more inner loop and a band start is not found, add the body declarations to this loop
          if (innerLoops.nonEmpty) {
            innerLoops.foreach(updateLoopAnnotations(extremaMap, _))
          } else {
            loop.annotate(CudaStrategiesUtils.CUDA_BODY_DECL, bodyDecl)
          }

        }
      } catch {
        case e : EvaluationException =>
          Logger.warning(s"""Error while searching for band start! Failed to calculate bounds extrema: '${ e.msg }'""")
      }
    }
  }

  this += new Transformation("Processing ForLoopStatement nodes", {
    case loop : ForLoopStatement if loop.hasAnnotation(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION) =>
      loop.removeAnnotation(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION)
      updateLoopAnnotations(mutable.HashMap[String, (Long, Long)](), loop)
      loop
    case scope : Scope if scope.hasAnnotation(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION)          =>
      scope.removeAnnotation(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION)

      val varDeclarations = scope.body.takeWhile(x => x.isInstanceOf[VariableDeclarationStatement])
      val remainingBody = scope.body.dropWhile(x => x.isInstanceOf[VariableDeclarationStatement])

      remainingBody match {
        case ListBuffer(c : CommentStatement, loop : ForLoopStatement) =>
          updateLoopAnnotations(mutable.HashMap[String, (Long, Long)](), loop, varDeclarations)
          new Scope(c, loop)
        case _                                                         =>
          scope
      }
  }, false)
}

/**
  * This transformation is used to convert annotated code into CUDA kernel code.
  */
object ExtractHostAndDeviceCode extends DefaultStrategy("Transform annotated CUDA loop in kernel code") {
  val collector = new FctNameCollector
  this.register(collector)

  /**
    * Collect all loops in the band.
    *
    * @param loop the outer loop of the band
    * @return list of loops in the band
    */
  def collectLoopsInKernel(loop : ForLoopStatement, condition : ForLoopStatement => Boolean) : ListBuffer[ForLoopStatement] = {
    val innerLoopCandidate = loop.body.head
    val loops = ListBuffer[ForLoopStatement](loop)

    innerLoopCandidate match {
      case innerLoop : ForLoopStatement if condition.apply(innerLoop) =>
        collectLoopsInKernel(innerLoop, condition) ++ loops
      case _                                                          => loops
    }
  }

  /**
    * Trim the loop headers to have the inner loop body to spare.
    *
    * @param body    the statements that should be traversed
    * @param surplus the for loop statements that should be removed
    * @return the inner body without loop headers
    */
  @tailrec
  def pruneKernelBody(body : ListBuffer[IR_Statement], surplus : ListBuffer[ForLoopStatement]) : ListBuffer[IR_Statement] = {
    if (surplus.isEmpty || !(body.head.isInstanceOf[ForLoopStatement] && (body.head equals surplus.head))) {
      body
    } else {
      pruneKernelBody(body.head.asInstanceOf[ForLoopStatement].body, surplus.tail)
    }
  }

  this += new Transformation("Processing ForLoopStatement nodes", {
    case loop : ForLoopStatement if loop.hasAnnotation(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION) &&
      loop.getAnnotation(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION).contains(CudaStrategiesUtils.CUDA_BAND_START) =>

      // remove the annotation first to guarantee single application of this transformation.
      loop.annotate(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION)

      val parallelLoops = (x : ForLoopStatement) => {
        x.hasAnnotation(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION) &&
          x.getAnnotation(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION).contains(CudaStrategiesUtils.CUDA_BAND_PART)
      }
      val allLoops = (x : ForLoopStatement) => {
        x.hasAnnotation(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION) &&
          (x.getAnnotation(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION).contains(CudaStrategiesUtils.CUDA_BAND_PART) ||
            x.getAnnotation(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION).contains(CudaStrategiesUtils.CUDA_INNER))
      }
      val parallelInnerLoops = collectLoopsInKernel(loop, parallelLoops)
      val allInnerLoops = collectLoopsInKernel(loop, allLoops)
      val (loopVariables, lowerBounds, upperBounds, stepSize) = CudaStrategiesUtils.extractRelevantLoopInformation(allInnerLoops)
      val kernelBody = pruneKernelBody(ListBuffer[IR_Statement](loop), parallelInnerLoops.reverse)

      if (loop.hasAnnotation(CudaStrategiesUtils.CUDA_BODY_DECL)) {
        loop.getAnnotation(CudaStrategiesUtils.CUDA_BODY_DECL).getOrElse(ListBuffer[VariableDeclarationStatement]()).asInstanceOf[ListBuffer[VariableDeclarationStatement]].foreach(x => kernelBody.prepend(x))
      }

      val deviceStatements = ListBuffer[IR_Statement]()

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

      val kernel = Kernel(
        kernelFunctions.getIdentifier(collector.getCurrentName),
        Duplicate(parallelInnerLoops.length),
        variableAccesses.map(s => FunctionArgument(s.name, s.datatype.get)),
        Duplicate(loopVariables),
        Duplicate(lowerBounds),
        Duplicate(upperBounds),
        Duplicate(stepSize),
        Duplicate(kernelBody),
        Duplicate(loop.reduction),
        Duplicate(extremaMap))

      kernelFunctions.addKernel(Duplicate(kernel))

      // process return value of kernel wrapper call if reduction is required
      if (loop.reduction.isDefined) {
        val red = loop.reduction.get
        deviceStatements += AssignmentStatement(red.target,
          IR_BinaryOperators.createExpression(red.op, red.target,
            FunctionCallExpression(kernel.getWrapperFctName, variableAccesses.map(_.asInstanceOf[IR_Expression]))))
      } else {
        deviceStatements += FunctionCallExpression(kernel.getWrapperFctName, variableAccesses.map(_.asInstanceOf[IR_Expression]))
      }

      deviceStatements
  }, false)
}

object AdaptKernelDimensionalities extends DefaultStrategy("Reduce kernel dimensionality where necessary") {
  this += new Transformation("Process kernel nodes", {
    case kernel : Kernel =>
      while (kernel.parallelDims > Platform.hw_cuda_maxNumDimsBlock) {
        def it = VariableAccess(Kernel.KernelVariablePrefix + Kernel.KernelGlobalIndexPrefix + dimToString(kernel.parallelDims - 1), Some(IR_IntegerDatatype))
        kernel.body = ListBuffer[IR_Statement](ForLoopStatement(
          new VariableDeclarationStatement(it, kernel.lowerBounds.last),
          IR_LowerExpression(it, kernel.upperBounds.last),
          AssignmentStatement(it, kernel.stepSize.last, "+="),
          kernel.body))
        kernel.parallelDims -= 1
      }
      kernel
  })
}

object HandleKernelReductions extends DefaultStrategy("Handle reductions in device kernels") {
  this += new Transformation("Process kernel nodes", {
    case kernel : Kernel if kernel.reduction.isDefined =>
      // update assignments according to reduction clauses
      kernel.evalIndexBounds()
      val index = MultiIndex((0 until kernel.parallelDims).map(dim =>
        VariableAccess(Kernel.KernelVariablePrefix + Kernel.KernelGlobalIndexPrefix + dimToString(dim), Some(IR_IntegerDatatype)) : IR_Expression).toArray)

      val stride = (kernel.maxIndices, kernel.minIndices).zipped.map((x, y) => IR_SubtractionExpression(x, y) : IR_Expression)

      ReplaceReductionAssignements.redTarget = kernel.reduction.get.target.name
      ReplaceReductionAssignements.replacement = ReductionDeviceDataAccess(iv.ReductionDeviceData(IR_MultiplicationExpression(ListBuffer[IR_Expression](stride : _*))), index, MultiIndex(stride))
      ReplaceReductionAssignements.applyStandalone(Scope(kernel.body))
      kernel
  })
}

object ReplaceReductionAssignements extends QuietDefaultStrategy("Replace assignments to reduction targets") {
  var redTarget : String = ""
  var replacement : IR_Expression = IR_NullExpression

  this += new Transformation("Searching", {
    case assignment : AssignmentStatement =>
      assignment.dest match {
        case va : VariableAccess if redTarget.equals(va.name) =>
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
        case SlotAccess(_, offset)    => identifier += s"_o$offset"
        case IR_IntegerConstant(slot) => identifier += s"_s$slot"
        case _                        => identifier += s"_s${ access.fieldSelection.slot.prettyprint }"
      }
    }

    fieldAccesses.put(identifier, access)
  }

  this += new Transformation("Searching", {
    case assign : AssignmentStatement =>
      inWriteOp = true
      GatherLocalFieldAccess.applyStandalone(IR_ExpressionStatement(assign.dest))
      inWriteOp = false
      if ("=" != assign.op) // add compound assignments as read and write accesses
        GatherLocalFieldAccess.applyStandalone(IR_ExpressionStatement(assign.dest))
      GatherLocalFieldAccess.applyStandalone(IR_ExpressionStatement(assign.src))
      assign
    case access : FieldAccessLike     =>
      mapFieldAccess(access)
      access
  }, false)
}

object GatherLocalVariableAccesses extends QuietDefaultStrategy("Gathering local VariableAccess nodes") {
  var accesses = mutable.HashMap[String, VariableAccess]()
  var ignoredAccesses = mutable.SortedSet[String]()

  def clear() = {
    accesses = mutable.HashMap[String, VariableAccess]()
    ignoredAccesses = (0 to Knowledge.dimensionality + 2 /* FIXME: find a way to determine max dimensionality */).map(dim => dimToString(dim)).to[mutable.SortedSet]
  }

  this += new Transformation("Searching", {
    case decl : VariableDeclarationStatement                               =>
      ignoredAccesses += decl.name
      decl
    case access : VariableAccess if !ignoredAccesses.contains(access.name) =>
      accesses.put(access.name, access)
      access
  }, false)
}

object FindSurroundingLoopIteratorUsages extends QuietDefaultStrategy("Search for local VariableAccess nodes") {
  var loopIterators : Set[String] = Set[String]()
  var usedLoopIterators = ListBuffer[String]()

  this += new Transformation("Searching", {
    case access @ VariableAccess(name : String, _) if loopIterators.contains(name) =>
      usedLoopIterators += name
      access
  }, false)
}
