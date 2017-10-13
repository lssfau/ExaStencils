package exastencils.parallelization.api.cuda

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_InternalVariable
import exastencils.communication.ir._
import exastencils.config._
import exastencils.core._
import exastencils.datastructures.Node
import exastencils.deprecated.ir.IR_DimToString
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression

/// CUDA_Kernel

object CUDA_Kernel {
  def wrapperPostfix = "_wrapper"
  val KernelVariablePrefix = "_cu_"
  val KernelGlobalIndexPrefix = "global_"
  val KernelLocalIndexPrefix = "local_"
  val CUDASharedMemoryAccess = "CUDASharedMemoryAccess"
  val ConstantIndexPart = "ConstantIndexPart"
}

// TODO: refactor -> less (convoluted) code
case class CUDA_Kernel(var identifier : String,
    var parallelDims : Int,
    var passThroughArgs : ListBuffer[IR_FunctionArgument],
    var loopVariables : ListBuffer[String],
    var lowerBounds : ListBuffer[IR_Expression],
    var upperBounds : ListBuffer[IR_Expression],
    var stepSize : ListBuffer[IR_Expression],
    var body : ListBuffer[IR_Statement],
    var reduction : Option[IR_Reduction] = None,
    var loopVariableExtrema : Map[String, (Long, Long)] = Map[String, (Long, Long)]()) extends Node {

  import CUDA_Kernel._

  var firstNSeqDims = loopVariables.size - parallelDims
  var smemCanBeUsed = Knowledge.cuda_useSharedMemory && firstNSeqDims == 0 && stepSize.forall(x => IR_IntegerConstant(1).equals(x))
  var spatialBlockingCanBeApplied = smemCanBeUsed && Knowledge.cuda_spatialBlockingWithSmem && parallelDims == Platform.hw_cuda_maxNumDimsBlock
  var executionDim = if (spatialBlockingCanBeApplied) parallelDims - 1 else math.min(Platform.hw_cuda_maxNumDimsBlock, parallelDims)

  // properties required for shared memory analysis and shared memory allocation
  var fieldNames = ListBuffer[String]()
  var fieldBaseIndex = HashMap[String, IR_ExpressionIndex]()
  var fieldForSharedMemory = HashMap[String, IR_DirectFieldAccess]()
  var fieldOffset = HashMap[String, IR_ExpressionIndex]()
  var fieldAccessesForSharedMemory = HashMap[String, List[IR_MultiDimFieldAccess]]()
  var leftDeviations = HashMap[String, Array[Long]]()
  var leftDeviation = HashMap[String, Long]()
  var rightDeviations = HashMap[String, Array[Long]]()
  var rightDeviation = HashMap[String, Long]()
  var sharedArraySize = HashMap[String, Array[Long]]()
  var fieldDatatype = HashMap[String, IR_Datatype]()

  var evaluatedAccesses = false
  var linearizedFieldAccesses = HashMap[String, IR_LinearizedFieldAccess]()
  var writtenFieldAccesses = HashMap[String, IR_LinearizedFieldAccess]()
  var bufferAccesses = HashMap[String, IR_IV_CommBuffer]()
  var ivAccesses = HashMap[String, IR_InternalVariable]()

  var evaluatedIndexBounds = false
  var minIndices = Array[Long]()
  var maxIndices = Array[Long]()

  var evaluatedExecutionConfiguration = false
  var requiredThreadsPerDim = Array[Long]()
  var numThreadsPerBlock = Array[Long]()
  var numBlocksPerDim = Array[Long]()

  // thread ids
  var localThreadId = Array[IR_Expression]()
  var globalThreadId = Array[IR_Expression]()

  def getKernelFctName : String = identifier
  def getWrapperFctName : String = identifier + wrapperPostfix

  init()

  /**
    * Initializes some of the properties and starts the shared memory analysis if specified.
    */
  def init() : Unit = {
    evalIndexBounds()
    evalExecutionConfiguration()
    evalThreadIds()

    // call this function already in constructor to work on DirectFieldAccesses where indices are not yet linearized.
    if (smemCanBeUsed) {
      if (executionDim < parallelDims) {
        globalThreadId = globalThreadId :+ IR_VariableAccess(loopVariables(executionDim), IR_IntegerDatatype)
      }
      evaluateAccessesForSharedMemory()
    }
  }

  /**
    * Perform shared memory analysis and collect all FieldAccesses that are worth to store in shared memory.
    */
  def evaluateAccessesForSharedMemory() : Unit = {
    // 1. get all field accesses in the body and ask for amount of shared memory
    CUDA_GatherFieldAccessLike.loopVariables.clear
    CUDA_GatherFieldAccessLike.loopVariables = loopVariables
    CUDA_GatherFieldAccessLike.fieldAccesses.clear
    CUDA_GatherFieldAccessLike.fieldIndicesConstantPart.clear
    CUDA_GatherFieldAccessLike.maximalFieldDim = math.min(parallelDims, Platform.hw_cuda_maxNumDimsBlock)
    CUDA_GatherFieldAccessLike.writtenFields.clear
    CUDA_GatherFieldAccessLike.applyStandalone(IR_Scope(body))
    var fieldToFieldAccesses = CUDA_GatherFieldAccessLike.fieldAccesses
    val writtenFields = CUDA_GatherFieldAccessLike.writtenFields
    val fieldIndicesConstantPart = CUDA_GatherFieldAccessLike.fieldIndicesConstantPart
    var availableSharedMemory = if (Knowledge.cuda_favorL1CacheOverSharedMemory) Platform.hw_cuda_cacheMemory.toLong else Platform.hw_cuda_sharedMemory.toLong

    // 2. Perform shared memory analysis
    // the more field accesses to a field the more important it is to store this field in shared memory
    fieldToFieldAccesses = fieldToFieldAccesses.filter(fa => fa._2.nonEmpty)
    fieldToFieldAccesses = ListMap(fieldToFieldAccesses.toSeq.sortWith(_._2.size > _._2.size) : _*)
    var foundSomeAppropriateField = false
    for (fa <- fieldToFieldAccesses) {
      val name = fa._1
      val fieldAccesses = fa._2

      // 2.1 collect some basic information required for further calculations
      var requiredMemoryInByte = 0L
      val offset = fieldAccesses.head.fieldSelection.fieldLayout.referenceOffset
      val baseIndex = (loopVariables.take(offset.length), offset).zipped.map((x, y) => IR_Addition(IR_VariableAccess(x, IR_IntegerDatatype), y)).toArray[IR_Expression]

      // 2.2 calculate negative and positive deviation from the basic field index
      val leftDeviationFromBaseIndex = fieldIndicesConstantPart(name).foldLeft(Array.fill(parallelDims)(0L))((acc, m) => (acc, m, offset).zipped.map((x, y, z) => {
        math.min(IR_SimplifyExpression.evalIntegral(x), IR_SimplifyExpression.evalIntegral(IR_Subtraction(y, z)))
      })).map(x => math.abs(x))
      var firstDeviation = leftDeviationFromBaseIndex.head
      var isSameRadius = leftDeviationFromBaseIndex.forall(x => firstDeviation.equals(x))

      val rightDeviationFromBaseIndex = fieldIndicesConstantPart(name).foldLeft(Array.fill(parallelDims)(0L))((acc, m) => (acc, m, offset).zipped.map((x, y, z) => {
        math.max(IR_SimplifyExpression.evalIntegral(x), IR_SimplifyExpression.evalIntegral(IR_Subtraction(y, z)))
      }))
      firstDeviation = rightDeviationFromBaseIndex.head
      isSameRadius &= rightDeviationFromBaseIndex.forall(x => firstDeviation.equals(x))
      isSameRadius &= leftDeviationFromBaseIndex.head.equals(rightDeviationFromBaseIndex.head)

      if (spatialBlockingCanBeApplied && !isSameRadius) {
        Logger.warning("Cannot apply spatial blocking with shared memory, because the stencil is not symmetric")
        spatialBlockingCanBeApplied = false
        executionDim = math.min(Platform.hw_cuda_maxNumDimsBlock, parallelDims)
        evaluatedIndexBounds = false
        evaluatedExecutionConfiguration = false
        evalIndexBounds()
        evalExecutionConfiguration()
        evalThreadIds()
      }

      // 2.3 calculate the required amount of shared memory
      val numDupLayersLeft = fieldAccesses.head.fieldSelection.fieldLayout.layoutsPerDim.map(x => x.numDupLayersLeft)
      val numDupLayersRight = fieldAccesses.head.fieldSelection.fieldLayout.layoutsPerDim.map(x => x.numDupLayersRight)
      val numInnerLayers = fieldAccesses.head.fieldSelection.fieldLayout.layoutsPerDim.map(x => x.numInnerLayers)
      val numPointsInStencil = (numDupLayersLeft, numInnerLayers, numDupLayersRight).zipped.map((x, y, z) => x + y + z)
      val numPointsInStencilPerThreadBlock = (numPointsInStencil, numThreadsPerBlock).zipped.map((x, y) => math.min(x, y))
      val arraySize = ((numPointsInStencilPerThreadBlock, leftDeviationFromBaseIndex).zipped.map(_ + _), rightDeviationFromBaseIndex).zipped.map(_ + _)
      requiredMemoryInByte = arraySize.product * (fieldAccesses.head.fieldSelection.fieldLayout.datatype match {
        case IR_RealDatatype   => if (Knowledge.useDblPrecision) 8 else 4
        case IR_DoubleDatatype => 8
        case IR_FloatDatatype  => 4
        case _                 => -1
      })

      // 2.4 consider this field for shared memory if all conditions are met
      if (!writtenFields.contains(name) && fieldAccesses.size > 1 && requiredMemoryInByte < availableSharedMemory && (leftDeviationFromBaseIndex.head > 0 || rightDeviationFromBaseIndex.head > 0)) {
        val access = IR_DirectFieldAccess(fieldAccesses.head.fieldSelection, IR_ExpressionIndex(baseIndex))
        access.allowLinearization = true
        fieldNames += name
        fieldBaseIndex(name) = IR_ExpressionIndex(baseIndex)
        fieldForSharedMemory(name) = access
        fieldAccesses.indices.foreach(x => fieldAccesses(x).annotate(ConstantIndexPart, fieldIndicesConstantPart(name)(x)))
        fieldAccessesForSharedMemory(name) = fieldAccesses
        fieldOffset(name) = Duplicate(offset)
        leftDeviations(name) = leftDeviationFromBaseIndex
        leftDeviation(name) = leftDeviationFromBaseIndex.head
        rightDeviations(name) = rightDeviationFromBaseIndex
        rightDeviation(name) = rightDeviationFromBaseIndex.head
        sharedArraySize(name) = arraySize
        fieldDatatype(name) = access.fieldSelection.fieldLayout.datatype
        foundSomeAppropriateField = true
        availableSharedMemory -= requiredMemoryInByte
      }
    }

    // 3. remove annotation from all fields that will not be stored in shared memory
    fieldToFieldAccesses.retain((key, value) => !fieldNames.contains(key))
    fieldToFieldAccesses.foreach(fa => fa._2.foreach(a => a.allowLinearization = false))

    // 4. ensure correct executionDim if no appropriate field was found
    if (!foundSomeAppropriateField) {
      executionDim = math.min(Platform.hw_cuda_maxNumDimsBlock, parallelDims)
      evaluatedExecutionConfiguration = false
      evalExecutionConfiguration()
      evalThreadIds()
    }
  }

  /**
    * Create global and local thread ids used in the kernel.
    */
  def evalThreadIds() = {
    localThreadId = (0 until executionDim).map(dim => {
      IR_VariableAccess(KernelVariablePrefix + KernelLocalIndexPrefix + IR_DimToString(dim), IR_IntegerDatatype)
    }).toArray[IR_Expression]
    globalThreadId = (0 until executionDim).map(dim => {
      IR_VariableAccess(KernelVariablePrefix + KernelGlobalIndexPrefix + IR_DimToString(dim), IR_IntegerDatatype)
    }).toArray[IR_Expression]
  }

  /**
    * Check the accesses in the loop to create valid function calls.
    */
  def evalAccesses() = {
    if (!evaluatedAccesses) {
      CUDA_GatherLinearizedFieldAccess.clear()
      CUDA_GatherLinearizedFieldAccess.applyStandalone(IR_Scope(body))
      linearizedFieldAccesses = CUDA_GatherLinearizedFieldAccess.fieldAccesses

      CUDA_GatherLinearizedBufferAccess.clear()
      CUDA_GatherLinearizedBufferAccess.applyStandalone(IR_Scope(body))
      bufferAccesses = CUDA_GatherLinearizedBufferAccess.bufferAccesses

      if (Knowledge.cuda_spatialBlockingWithROC) {
        CUDA_GatherLinearizedFieldAccessWrites.writtenFieldAccesses.clear
        CUDA_GatherLinearizedFieldAccessWrites.applyStandalone(IR_Scope(body))
        writtenFieldAccesses = CUDA_GatherLinearizedFieldAccessWrites.writtenFieldAccesses
      }

      CUDA_GatherIVs.ivAccesses.clear
      CUDA_GatherIVs.applyStandalone(IR_Scope(body))
      ivAccesses = CUDA_GatherIVs.ivAccesses

      // postprocess iv's -> generate parameter names
      var cnt = 0
      val processedIVs = HashMap[String, IR_InternalVariable]()
      for (ivAccess <- ivAccesses) {
        processedIVs.put(ivAccess._2.resolveName + "_" + cnt, ivAccess._2)
        cnt += 1
      }
      ivAccesses = processedIVs

      evaluatedAccesses = true
    }
  }

  /**
    * Evaluate the index bounds to calculate minimal and maximal valid indices. Required to check if some thread is out
    * of bounds or not.
    */
  def evalIndexBounds() = {
    if (!evaluatedIndexBounds) {
      minIndices = (loopVariables.size - 1 to 0 by -1).map(dim =>
        loopVariableExtrema.get(loopVariables(dim)) match {
          case Some((min : Long, max : Long)) => min
          case _                              =>
            Logger.warn(s"Start index for dimension $dim (${ lowerBounds(dim) }) could not be evaluated")
            0
        }).toArray.reverse.drop(firstNSeqDims)

      maxIndices = (loopVariables.size - 1 to 0 by -1).map(dim =>
        loopVariableExtrema.get(loopVariables(dim)) match {
          case Some((min : Long, max : Long)) => max
          case _                              =>
            Logger.warn(s"Start index for dimension $dim (${ upperBounds(dim) }) could not be evaluated")
            0
        }).toArray.reverse.drop(firstNSeqDims)

      evaluatedIndexBounds = true
    }
  }

  /**
    * Calculate the execution configuration for this kernel.
    */
  def evalExecutionConfiguration() = {
    if (!evaluatedExecutionConfiguration) {
      requiredThreadsPerDim = (maxIndices, minIndices).zipped.map(_ - _)

      if (null == requiredThreadsPerDim || requiredThreadsPerDim.product <= 0) {
        Logger.warn("Could not evaluate required number of threads for kernel " + identifier)
        requiredThreadsPerDim = (0 until executionDim).map(dim => 0 : Long).toArray // TODO: replace 0 with sth more suitable
      }

      numThreadsPerBlock = Knowledge.cuda_blockSizeAsVec.take(executionDim)

      numBlocksPerDim = (0 until executionDim).map(dim => {
        (requiredThreadsPerDim(dim) + numThreadsPerBlock(dim) - 1) / numThreadsPerBlock(dim)
      }).toArray

      evaluatedExecutionConfiguration = true
    }
  }

  def compileKernelBody : ListBuffer[IR_Statement] = {
    evalIndexBounds() // ensure that minimal and maximal indices are set correctly
    evalAccesses() // ensure that field accesses have been mapped
    evalExecutionConfiguration() // ensure that execution configuration is already calculated

    var statements = ListBuffer[IR_Statement]()

    // add CUDA global Thread ID (x,y,z) calculation for a dim3 execution configuration
    // global thread id x = blockIdx.x *blockDim.x + threadIdx.x + offset1;
    // global thread id y = blockIdx.y *blockDim.y + threadIdx.y + offset2;
    // global thread id z = blockIdx.z *blockDim.z + threadIdx.z + offset3;
    statements ++= (0 until executionDim).map(dim => {
      val it = IR_DimToString(dim)
      val variableName = KernelVariablePrefix + KernelGlobalIndexPrefix + it
      IR_VariableDeclaration(IR_IntegerDatatype, variableName,
        Some(stepSize(dim) * (IR_MemberAccess(IR_VariableAccess("blockIdx", IR_SpecialDatatype("dim3")), it) *
          IR_MemberAccess(IR_VariableAccess("blockDim", IR_SpecialDatatype("dim3")), it) +
          IR_MemberAccess(IR_VariableAccess("threadIdx", IR_SpecialDatatype("dim3")), it) +
          minIndices(dim))))
    })

    // add dimension index start and end point
    // add index bounds conditions
    val conditionParts = (0 until executionDim).map(dim => {
      val variableAccess = IR_VariableAccess(KernelVariablePrefix + KernelGlobalIndexPrefix + IR_DimToString(dim), IR_IntegerDatatype)
      IR_AndAnd(
        IR_GreaterEqual(variableAccess, s"${ KernelVariablePrefix }begin_$dim"), IR_Lower(variableAccess, s"${ KernelVariablePrefix }end_$dim"))
    })

    val condition = IR_VariableDeclaration(IR_BooleanDatatype, KernelVariablePrefix + "condition",
      Some(conditionParts.reduceLeft[IR_AndAnd] { (acc, n) =>
        IR_AndAnd(acc, n)
      }))
    val conditionAccess = IR_VariableAccess(KernelVariablePrefix + "condition", IR_BooleanDatatype)
    statements += condition

    if (smemCanBeUsed && fieldForSharedMemory.nonEmpty) {
      fieldNames.foreach(field => {
        val localPrefix = KernelVariablePrefix + "local_"
        val sharedMemoryStatements = ListBuffer[IR_Statement]()
        val sharedArrayStrides = IR_ExpressionIndex(sharedArraySize(field))
        var zDimLoopBody = ListBuffer[IR_Statement]()
        val current = IR_VariableAccess("current", IR_IntegerDatatype)

        // 1. Annotate the loop variables appearing in the shared memory accesses to guarantee the right substitution later
        CUDA_AnnotateLoopVariables.loopVariables = loopVariables
        CUDA_AnnotateLoopVariables.accessName = field
        CUDA_AnnotateLoopVariables.applyStandalone(IR_Scope(fieldAccessesForSharedMemory(field).map(x => IR_ExpressionStatement(x))))

        // 2. Add local Thread ID calculation for indexing shared memory
        statements ++= (0 until executionDim).map(dim => {
          val it = IR_DimToString(dim)
          val variableName = localPrefix + it
          IR_VariableDeclaration(IR_IntegerDatatype, variableName,
            Some(IR_MemberAccess(IR_VariableAccess("threadIdx", IR_SpecialDatatype("dim3")), it) +
              leftDeviations(field)(dim)))
        })

        // 3. Add shared memory declarations
        statements += CUDA_SharedArray(KernelVariablePrefix + field, fieldDatatype(field), sharedArraySize(field).reverse)

        if (spatialBlockingCanBeApplied) {
          // 4. Declarations for neighbors and current point
          val spatialBaseIndex = IR_ExpressionIndex(fieldBaseIndex(field).indices.take(executionDim) :+ fieldOffset(field).indices(executionDim))
          (1L to leftDeviation(field)).foreach(x => {
            statements += IR_VariableDeclaration(fieldDatatype(field), "behind" + x, 0)
          })
          (1L to rightDeviation(field)).foreach(x => {
            statements += IR_VariableDeclaration(fieldDatatype(field), "infront" + x, IR_DirectFieldAccess(fieldForSharedMemory(field).fieldSelection, spatialBaseIndex + IR_ExpressionIndex(Array[Long](0, 0, x))).linearize)
          })
          statements += IR_VariableDeclaration(fieldDatatype(field), "current", IR_DirectFieldAccess(fieldForSharedMemory(field).fieldSelection, spatialBaseIndex).linearize)

          // 5. Add statements for loop body in kernel (z-Dim)
          // 5.1 advance the slice (move the thread front)
          (leftDeviation(field) to 2L by -1).foreach(x => {
            sharedMemoryStatements += IR_Assignment(IR_VariableAccess("behind" + x, fieldDatatype(field)), IR_VariableAccess("behind" + (x - 1), fieldDatatype(field)))
          })
          sharedMemoryStatements += IR_Assignment(IR_VariableAccess("behind1", fieldDatatype(field)), current)
          sharedMemoryStatements += IR_Assignment(current, IR_VariableAccess("infront1", fieldDatatype(field)))
          (2L to rightDeviation(field)).foreach(x => {
            sharedMemoryStatements += IR_Assignment(IR_VariableAccess("infront" + x, fieldDatatype(field)), IR_VariableAccess("infront" + (x + 1), fieldDatatype(field)))
          })

          sharedMemoryStatements += IR_Assignment(IR_VariableAccess("infront" + rightDeviation(field), fieldDatatype(field)), IR_DirectFieldAccess(fieldForSharedMemory(field).fieldSelection, fieldBaseIndex(field) + IR_ExpressionIndex(Array[Long](0, 0, 1))).linearize)

          // 5.2 load from global memory into shared memory
          sharedMemoryStatements += IR_Assignment(new CUDA_SharedArrayAccess(KernelVariablePrefix + field, localThreadId.take(executionDim).reverse, sharedArrayStrides), current)
        } else {
          // 6. Load from global memory into shared memory
          sharedMemoryStatements += IR_Assignment(new CUDA_SharedArrayAccess(KernelVariablePrefix + field, localThreadId.reverse, sharedArrayStrides), IR_DirectFieldAccess(fieldForSharedMemory(field).fieldSelection, fieldForSharedMemory(field).index).linearize)
        }

        // 7. Add load operations as ConditionStatement to avoid index out of bounds exceptions in global memory
        // and sync threads afterwards to guarantee that every thread has the same memory state
        sharedMemoryStatements ++= (0 until executionDim).map(dim => {
          val it = IR_DimToString(dim)

          // 7.1 Check if current thread resides on the left border in any dimension
          val condition = IR_OrOr(IR_Lower(IR_MemberAccess(IR_VariableAccess("threadIdx", IR_SpecialDatatype("dim3")), it), leftDeviations(field)(dim)), IR_EqEq(globalThreadId(dim), s"${ KernelVariablePrefix }begin_$dim"))
          val conditionBody = ListBuffer[IR_Statement]()

          // 7.2 Calculate the offset from the left to the right border of the actual field
          val localFieldOffsetName : String = "localFieldOffset"
          conditionBody += IR_VariableDeclaration(IR_IntegerDatatype, localFieldOffsetName, Some(
            CUDA_Minimum(
              IR_Subtraction(IR_MemberAccess(IR_VariableAccess("blockDim", IR_SpecialDatatype("dim3")), it),
                IR_MemberAccess(IR_VariableAccess("threadIdx", IR_SpecialDatatype("dim3")), it)),
              IR_Subtraction(s"${ KernelVariablePrefix }end_$dim", globalThreadId(dim)))))
          val localFieldOffset = IR_VariableAccess(localFieldOffsetName, IR_IntegerDatatype)

          // 7.3 Calculate the indices for writing into the shared memory and loading from the global memory
          // 7.4 Thread residing on left border should load left neighbor and the right neighbor of the point residing
          // on the right border of the actual field
          (1L to leftDeviations(field)(dim)).foreach(x => {
            val localLeftIndex = Duplicate(localThreadId)
            localLeftIndex(dim) = IR_Subtraction(localLeftIndex(dim), x)
            val globalLeftIndex = IR_ExpressionIndex(Duplicate(globalThreadId)) + fieldOffset(field)
            globalLeftIndex(dim) = IR_Subtraction(globalLeftIndex(dim), x)

            conditionBody += IR_Assignment(new CUDA_SharedArrayAccess(KernelVariablePrefix + field, localLeftIndex.reverse, sharedArrayStrides), IR_DirectFieldAccess(fieldForSharedMemory(field).fieldSelection, globalLeftIndex).linearize)
          })
          (0L until rightDeviations(field)(dim)).foreach(x => {
            val localRightIndex = Duplicate(localThreadId)
            localRightIndex(dim) = IR_Addition(IR_Addition(localRightIndex(dim), localFieldOffset), x)
            val globalRightIndex = IR_ExpressionIndex(Duplicate(globalThreadId)) + fieldOffset(field)
            globalRightIndex(dim) = IR_Addition(IR_Addition(globalRightIndex(dim), localFieldOffset), x)

            conditionBody += IR_Assignment(new CUDA_SharedArrayAccess(KernelVariablePrefix + field, localRightIndex.reverse, sharedArrayStrides), IR_DirectFieldAccess(fieldForSharedMemory(field).fieldSelection, globalRightIndex).linearize)
          })

          IR_IfCondition(condition, conditionBody)
        })

        if (spatialBlockingCanBeApplied) {
          // 8. Complete loop body for spatial blocking
          zDimLoopBody += IR_IfCondition(conditionAccess, sharedMemoryStatements)
          zDimLoopBody += CUDA_SyncThreads()
          zDimLoopBody += IR_IfCondition(conditionAccess, body)
          zDimLoopBody += CUDA_SyncThreads()

          statements += IR_ForLoop(IR_VariableDeclaration(IR_IntegerDatatype, loopVariables(executionDim), s"${ KernelVariablePrefix }begin_$executionDim"), IR_Lower(IR_VariableAccess(loopVariables(executionDim), IR_IntegerDatatype), s"${ KernelVariablePrefix }end_$executionDim"), IR_Assignment(loopVariables(executionDim), IR_IntegerConstant(1), "+="), zDimLoopBody)

          // 9. Remove the used loop variable to avoid later complications in loop variable substitution
          loopVariables.remove(executionDim)
        } else {
          // 10. Add whole shared memory initialization wrapped in a ConditionStatement to the body
          statements += IR_IfCondition(conditionAccess, sharedMemoryStatements)

          // This may not be part of the ConditionStatement to avoid dead locks if some thread do not fulfill the condition
          statements += CUDA_SyncThreads()
          statements += IR_IfCondition(conditionAccess, body)
        }
      })
    } else {
      statements += IR_IfCondition(conditionAccess, body)
    }

    body = statements

    // add actual body after replacing field and iv accesses
    // replace FieldAccess nodes in body with shared memory accesses
    if (smemCanBeUsed) {
      fieldNames.foreach(field => {
        CUDA_ReplaceFieldAccessLike.fieldToOffset = fieldOffset(field)
        CUDA_ReplaceFieldAccessLike.offsetForSharedMemoryAccess = leftDeviation(field)
        CUDA_ReplaceFieldAccessLike.sharedArrayStrides = sharedArraySize(field)
        CUDA_ReplaceFieldAccessLike.executionDim = executionDim
        CUDA_ReplaceFieldAccessLike.baseIndex = fieldBaseIndex(field)
        CUDA_ReplaceFieldAccessLike.applySpatialBlocking = spatialBlockingCanBeApplied
        CUDA_ReplaceFieldAccessLike.applyStandalone(IR_Scope(body))
      })
    }

    CUDA_ReplaceLinearizedFieldAccess.applyStandalone(IR_Scope(body))

    CUDA_ReplaceLinearizedBufferAccess.applyStandalone(IR_Scope(body))

    CUDA_ReplaceIVs.ivAccesses = ivAccesses
    CUDA_ReplaceIVs.applyStandalone(IR_Scope(body))
    CUDA_ReplaceLoopVariables.loopVariables = loopVariables.drop(firstNSeqDims)
    CUDA_ReplaceLoopVariables.applyStandalone(IR_Scope(body))

    body
  }

  def compileWrapperFunction : IR_Function = {
    evalIndexBounds() // ensure that minimal and maximal indices are set correctly
    evalAccesses() // ensure that field accesses have been mapped
    evalExecutionConfiguration() // ensure that execution configuration is already calculated

    // substitute loop variable in bounds with appropriate fix values to get valid code in wrapper function
    CUDA_ReplaceLoopVariablesInWrapper.loopVariables.clear
    CUDA_ReplaceLoopVariablesInWrapper.loopVariables = loopVariables.drop(firstNSeqDims)
    CUDA_ReplaceLoopVariablesInWrapper.bounds = Duplicate(lowerBounds.drop(firstNSeqDims))
    val lowerArgs = Duplicate(lowerBounds.drop(firstNSeqDims))
    CUDA_ReplaceLoopVariablesInWrapper.applyStandalone(lowerArgs)

    val upperArgs = Duplicate(upperBounds.drop(firstNSeqDims))
    CUDA_ReplaceLoopVariablesInWrapper.bounds = Duplicate(upperBounds.drop(firstNSeqDims))
    CUDA_ReplaceLoopVariablesInWrapper.applyStandalone(upperArgs)

    // compile arguments for device function call
    var callArgs = ListBuffer[IR_Expression]()

    for (dim <- 0 until parallelDims) {
      callArgs += lowerArgs(dim)
      callArgs += upperArgs(dim)
    }

    for (fieldAccess <- linearizedFieldAccesses) {
      val fieldSelection = fieldAccess._2.fieldSelection
      callArgs += CUDA_FieldDeviceData(fieldSelection.field, fieldSelection.level, fieldSelection.slot)
    }

    if (Knowledge.cuda_useSharedMemory && fieldForSharedMemory.nonEmpty) {
      fieldNames.foreach(field => {
        val fieldSelection = fieldForSharedMemory(field).fieldSelection
        callArgs += CUDA_FieldDeviceData(fieldSelection.field, fieldSelection.level, fieldSelection.slot)
      })
    }

    for (bufferAccess <- bufferAccesses) {
      val buffer = bufferAccess._2
      callArgs += CUDA_BufferDeviceData(buffer.field, buffer.direction, buffer.size, buffer.neighIdx)
    }

    for (ivAccess <- ivAccesses)
      callArgs += Duplicate(ivAccess._2)

    for (variableAccess <- passThroughArgs) {
      callArgs += variableAccess.access
    }

    var body = ListBuffer[IR_Statement]()

    if (reduction.isDefined) {
      def bufSize = requiredThreadsPerDim.product
      def bufAccess = CUDA_ReductionDeviceData(bufSize)
      body += CUDA_Memset(bufAccess, 0, bufSize, reduction.get.target.datatype)
      body += CUDA_FunctionCallExperimental(getKernelFctName, callArgs, numThreadsPerBlock, numBlocksPerDim)
      body += IR_Return(Some(IR_FunctionCall(s"DefaultReductionKernel${ IR_BinaryOperators.opAsIdent(reduction.get.op) }_wrapper",
        ListBuffer[IR_Expression](bufAccess, bufSize))))

      CUDA_KernelFunctions.get.requiredRedKernels += reduction.get.op // request reduction kernel and wrapper
    } else {
      body += CUDA_FunctionCallExperimental(getKernelFctName, callArgs, numThreadsPerBlock, numBlocksPerDim)
    }

    val fct = IR_PlainFunction( /* FIXME: IR_LeveledFunction? */
      getWrapperFctName,
      if (reduction.isDefined) reduction.get.target.datatype else IR_UnitDatatype,
      Duplicate(passThroughArgs),
      body)

    fct.allowInlining = false
    fct.allowFortranInterface = false
    fct.functionQualifiers = "extern \"C\""

    fct
  }

  def compileKernelFunction : IR_Function = {
    evalIndexBounds() // ensure that minimal and maximal indices are set correctly
    evalAccesses() // ensure that field accesses have been mapped
    evalExecutionConfiguration() // ensure that execution configuration is already calculated

    // compile parameters for device function
    var fctParams = ListBuffer[IR_FunctionArgument]()

    for (dim <- 0 until parallelDims) {
      fctParams += IR_FunctionArgument(s"${ KernelVariablePrefix }begin_$dim", IR_IntegerDatatype)
      fctParams += IR_FunctionArgument(s"${ KernelVariablePrefix }end_$dim", IR_IntegerDatatype)
    }

    for (fieldAccess <- linearizedFieldAccesses) {
      val fieldSelection = fieldAccess._2.fieldSelection

      // add required parameter specifiers to take advantage of the read-only cache
      if (Knowledge.cuda_spatialBlockingWithROC && !writtenFieldAccesses.contains(fieldAccess._1)) {
        fctParams += IR_FunctionArgument(fieldAccess._1, IR_CUDAConstPointerDatatype(fieldSelection.field.resolveDeclType))
      } else {
        fctParams += IR_FunctionArgument(fieldAccess._1, IR_PointerDatatype(fieldSelection.field.resolveDeclType))
      }
    }

    if (fieldForSharedMemory.nonEmpty) {
      fieldNames.foreach(field => {
        fctParams += IR_FunctionArgument(field, IR_PointerDatatype(fieldForSharedMemory(field).fieldSelection.field.resolveDeclType))
      })
    }

    for (bufferAccess <- bufferAccesses)
      fctParams += IR_FunctionArgument(bufferAccess._1, IR_PointerDatatype(bufferAccess._2.field.resolveDeclType))

    for (ivAccess <- ivAccesses) {
      val access = IR_VariableAccess(ivAccess._1, ivAccess._2.resolveDatatype())
      val datatype = ivAccess._2.resolveDatatype()

      fctParams += IR_FunctionArgument(ivAccess._1, datatype)
    }

    for (variableAccess <- passThroughArgs) {
      fctParams += IR_FunctionArgument(variableAccess.name, variableAccess.datatype)
    }

    val fct = IR_PlainFunction( /* FIXME: IR_LeveledFunction? */ getKernelFctName, IR_UnitDatatype, fctParams, compileKernelBody)

    fct.allowInlining = false
    fct.allowFortranInterface = false
    fct.functionQualifiers = "__global__"

    fct.annotate("deviceOnly")

    fct
  }
}
