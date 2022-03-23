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

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_HigherDimensionalDatatype
import exastencils.baseExt.ir.IR_InternalVariable
import exastencils.communication.ir._
import exastencils.config._
import exastencils.core._
import exastencils.datastructures.Node
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression

/// CUDA_Kernel

object CUDA_Kernel {
  def wrapperPostfix = "_wrapper"
  val KernelVariablePrefix = "_cu_"
  val KernelGlobalIndexPrefix = "global_"
  val CUDASharedMemoryAccess = "CUDASharedMemoryAccess"
  val ConstantIndexPart = "ConstantIndexPart"

  def localThreadId(fieldName : String, dim : Int) = {
    IR_VariableAccess(KernelVariablePrefix + "local_" + fieldName + "_" + dim, IR_IntegerDatatype)
  }

  // determine return type for reduction
  def getReductionReturnDt(reductionDt : IR_Datatype) = {
    reductionDt match {
      case scalar : IR_ScalarDatatype => scalar
      // additional argument: pointer to be updated
      // -> No return type, but return by call-by-pointer semantic
      case _ : IR_HigherDimensionalDatatype => IR_UnitDatatype
    }
  }
}

// TODO: refactor -> less (convoluted) code
case class CUDA_Kernel(
    var kernelCount : Int,
    var identifier : String,
    var parallelDims : Int,
    var passThroughArgs : ListBuffer[IR_FunctionArgument],
    var loopVariables : ListBuffer[String],
    var lowerBounds : ListBuffer[IR_Expression],
    var upperBounds : ListBuffer[IR_Expression],
    var stepSize : ListBuffer[IR_Expression],
    var body : ListBuffer[IR_Statement],
    var reduction : Option[IR_Reduction] = None,
    var localReductionTarget : Option[IR_Expression] = None,
    var loopVariableExtrema : Map[String, (Long, Long)] = Map[String, (Long, Long)]()) extends Node {

  import CUDA_Kernel._

  var nrInnerSeqDims = loopVariables.size - parallelDims
  var smemCanBeUsed = Knowledge.cuda_useSharedMemory && nrInnerSeqDims == 0 && stepSize.forall(x => IR_IntegerConstant(1).equals(x))
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

  // return datatype of kernel
  val returnDt = if (reduction.isDefined)
    getReductionReturnDt(CUDA_Util.getReductionDatatype(reduction.get.target))
  else
    IR_UnitDatatype

  // determine if reduction returns result via pointer argument (true for hodt)
  def isReductionReturnCallByPointer(reductionDt : IR_Datatype) = {
    reductionDt.isInstanceOf[IR_HigherDimensionalDatatype] && returnDt == IR_UnitDatatype
  }

  // thread ids
  //var localThreadId = Array[IR_Expression]()
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
      val offset = fieldAccesses.head.field.layout.referenceOffset
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
      val numDupLayersLeft = fieldAccesses.head.field.layout.layoutsPerDim.map(x => x.numDupLayersLeft)
      val numDupLayersRight = fieldAccesses.head.field.layout.layoutsPerDim.map(x => x.numDupLayersRight)
      val numInnerLayers = fieldAccesses.head.field.layout.layoutsPerDim.map(x => x.numInnerLayers)
      val numPointsInStencil = (numDupLayersLeft, numInnerLayers, numDupLayersRight).zipped.map((x, y, z) => x + y + z)
      val numPointsInStencilPerThreadBlock = (numPointsInStencil, numThreadsPerBlock).zipped.map((x, y) => math.min(x, y))
      val arraySize = ((numPointsInStencilPerThreadBlock, leftDeviationFromBaseIndex).zipped.map(_ + _), rightDeviationFromBaseIndex).zipped.map(_ + _)
      requiredMemoryInByte = arraySize.product * (fieldAccesses.head.field.layout.datatype match {
        case IR_RealDatatype   => if (Knowledge.useDblPrecision) 8 else 4
        case IR_DoubleDatatype => 8
        case IR_FloatDatatype  => 4
        case _                 => -1
      })

      // 2.4 consider this field for shared memory if all conditions are met
      if (!writtenFields.contains(name) && fieldAccesses.size > 1 && requiredMemoryInByte < availableSharedMemory && (leftDeviationFromBaseIndex.head > 0 || rightDeviationFromBaseIndex.head > 0)) {
        val access = IR_DirectFieldAccess(fieldAccesses.head.field, Duplicate(fieldAccesses.head.slot), IR_ExpressionIndex(baseIndex))
        access.allowLinearization = false
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
        fieldDatatype(name) = access.field.layout.datatype
        foundSomeAppropriateField = true
        availableSharedMemory -= requiredMemoryInByte
      }
    }

    // 3. remove annotation from all fields that will not be stored in shared memory
    //fieldToFieldAccesses.retain((key, value) => !fieldNames.contains(key))
    fieldToFieldAccesses.foreach(fa => fa._2.foreach(_.allowLinearization = !fieldNames.contains(fa._1)))

    // 4. ensure correct executionDim if no appropriate field was found
    if (!foundSomeAppropriateField) {
      executionDim = math.min(Platform.hw_cuda_maxNumDimsBlock, parallelDims)
      evaluatedExecutionConfiguration = false
      evalExecutionConfiguration()
      evalThreadIds()
    }
  }

  def localThreadId(fieldName : String) = {
    (0 until executionDim).map(dim => CUDA_Kernel.localThreadId(fieldName, dim)).toArray[IR_Expression]
  }

  /**
    * Create global and local thread ids used in the kernel.
    */
  def evalThreadIds() = {
//    localThreadId = (0 until executionDim).map(dim => {
//      IR_VariableAccess(KernelVariablePrefix + KernelLocalIndexPrefix + IR_DimToString(dim), IR_IntegerDatatype)
//    }).toArray[IR_Expression]
    globalThreadId = (0 until executionDim).map(dim => {
      IR_VariableAccess(KernelVariablePrefix + KernelGlobalIndexPrefix + dim, IR_IntegerDatatype)
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
      CUDA_GatherIVs.applyStandalone(IR_Addition(lowerBounds))
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
      minIndices = (0 until loopVariables.size).map(dim =>
        loopVariableExtrema.get(loopVariables(dim)) match {
          case Some((min : Long, max : Long)) => min
          case _                              =>
            Logger.warn(s"Start index for dimension $dim (${ lowerBounds(dim) }) could not be evaluated")
            0
        }).toArray.drop(nrInnerSeqDims)

      maxIndices = (0 until loopVariables.size).map(dim =>
        loopVariableExtrema.get(loopVariables(dim)) match {
          case Some((min : Long, max : Long)) => max
          case _                              =>
            Logger.warn(s"Start index for dimension $dim (${ upperBounds(dim) }) could not be evaluated")
            0
        }).toArray.drop(nrInnerSeqDims)

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

      // adapt thread count for reduced dimensions
      if (Knowledge.cuda_foldBlockSizeForRedDimensionality)
        for (d <- executionDim until Knowledge.dimensionality)
          numThreadsPerBlock(0) *= Knowledge.cuda_blockSizeAsVec(d)

      numBlocksPerDim = (0 until executionDim).map(dim => {
        val inc = stepSize(dim) match {
          case IR_IntegerConstant(i) => i
          case _                     => 1
        }
        val nrThreads = (requiredThreadsPerDim(dim) + inc - 1) / inc
        (nrThreads + numThreadsPerBlock(dim) - 1) / numThreadsPerBlock(dim)
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
    // global thread id x = lowerBoundX + stepSizeX * (blockIdx.x * blockDim.x + threadIdx.x);
    // global thread id y = lowerBoundY + stepSizeY * (blockIdx.y * blockDim.y + threadIdx.y);
    // global thread id z = lowerBoundZ + stepSizeZ * (blockIdx.z * blockDim.z + threadIdx.z);
    val parStepSize = Duplicate(stepSize.view.drop(nrInnerSeqDims).toArray)
    val parLowerBounds = Duplicate(lowerBounds.view.drop(nrInnerSeqDims).toArray)
    statements ++= (0 until executionDim).map(dim => {
      val variableName = KernelVariablePrefix + KernelGlobalIndexPrefix + dim
      IR_VariableDeclaration(IR_IntegerDatatype, variableName,
        Some(parLowerBounds(dim) + (parStepSize(dim) *
          (IR_MemberAccess(IR_VariableAccess("blockIdx", IR_SpecialDatatype("dim3")), CUDA_Util.dimToMember(dim)) *
            IR_MemberAccess(IR_VariableAccess("blockDim", IR_SpecialDatatype("dim3")), CUDA_Util.dimToMember(dim)) +
            IR_MemberAccess(IR_VariableAccess("threadIdx", IR_SpecialDatatype("dim3")), CUDA_Util.dimToMember(dim))))))
    }).reverse // reverse, since the inner (lower dim) could depend on the outer (higher dim)

    // add dimension index start and end point
    // add index bounds conditions
    val conditionParts = (0 until executionDim).map(dim => {
      val variableAccess = IR_VariableAccess(KernelVariablePrefix + KernelGlobalIndexPrefix + dim, IR_IntegerDatatype)
      // lower bound is already enforced above
      //IR_AndAnd(IR_GreaterEqual(variableAccess, s"${ KernelVariablePrefix }begin_$dim"), IR_Lower(variableAccess, s"${ KernelVariablePrefix }end_$dim"))
      IR_Lower(variableAccess, s"${ KernelVariablePrefix }end_$dim")
    })

    val condition = IR_VariableDeclaration(IR_BooleanDatatype, KernelVariablePrefix + "condition",
      Some(conditionParts.reduceLeft[IR_Expression] { (acc, n) =>
        IR_AndAnd(acc, n)
      }))
    val conditionAccess = IR_VariableAccess(KernelVariablePrefix + "condition", IR_BooleanDatatype)
    statements += condition

    if (smemCanBeUsed && fieldForSharedMemory.nonEmpty) {
      var zDimLoopBody = ListBuffer[IR_Statement]()
      fieldNames.foreach(field => {
        val sharedMemoryStatements = ListBuffer[IR_Statement]()
        val sharedArrayStrides = IR_ExpressionIndex(sharedArraySize(field))
        val current = IR_VariableAccess("current", IR_IntegerDatatype)

        // 1. Annotate the loop variables appearing in the shared memory accesses to guarantee the right substitution later
        CUDA_AnnotateLoopVariables.loopVariables = loopVariables
        CUDA_AnnotateLoopVariables.accessName = field
        CUDA_AnnotateLoopVariables.applyStandalone(IR_Scope(fieldAccessesForSharedMemory(field).map(x => IR_ExpressionStatement(x))))

        // 2. Add local Thread ID calculation for indexing shared memory
        statements ++= (0 until executionDim).map(dim => {
          val variableName = CUDA_Kernel.localThreadId(field, dim).name
          IR_VariableDeclaration(IR_IntegerDatatype, variableName,
            Some(IR_MemberAccess(IR_VariableAccess("threadIdx", IR_SpecialDatatype("dim3")), CUDA_Util.dimToMember(dim)) +
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
            statements += IR_VariableDeclaration(fieldDatatype(field), "infront" + x, IR_DirectFieldAccess(fieldForSharedMemory(field).field, Duplicate(fieldForSharedMemory(field).slot), spatialBaseIndex + IR_ExpressionIndex(Array[Long](0, 0, x))).linearize)
          })
          statements += IR_VariableDeclaration(fieldDatatype(field), "current", IR_DirectFieldAccess(fieldForSharedMemory(field).field, Duplicate(fieldForSharedMemory(field).slot), spatialBaseIndex).linearize)

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

          sharedMemoryStatements += IR_Assignment(IR_VariableAccess("infront" + rightDeviation(field), fieldDatatype(field)), IR_DirectFieldAccess(fieldForSharedMemory(field).field, Duplicate(fieldForSharedMemory(field).slot), fieldBaseIndex(field) + IR_ExpressionIndex(Array[Long](0, 0, 1))).linearize)

          // 5.2 load from global memory into shared memory
          sharedMemoryStatements += IR_Assignment(new CUDA_SharedArrayAccess(KernelVariablePrefix + field, localThreadId(field).take(executionDim).reverse, sharedArrayStrides), current)
        } else {
          // 6. Load from global memory into shared memory
          sharedMemoryStatements += IR_Assignment(new CUDA_SharedArrayAccess(KernelVariablePrefix + field, localThreadId(field).reverse, sharedArrayStrides), IR_DirectFieldAccess(fieldForSharedMemory(field).field, Duplicate(fieldForSharedMemory(field).slot), fieldForSharedMemory(field).index).linearize)
        }

        // 7. Add load operations as ConditionStatement to avoid index out of bounds exceptions in global memory
        // and sync threads afterwards to guarantee that every thread has the same memory state
        sharedMemoryStatements ++= (0 until executionDim).map(dim => {
          // 7.1 Check if current thread resides on the left border in any dimension
          //val condition = IR_OrOr(IR_Lower(IR_MemberAccess(IR_VariableAccess("threadIdx", IR_SpecialDatatype("dim3")), it), leftDeviations(field)(dim)), IR_EqEq(globalThreadId(dim), s"${ KernelVariablePrefix }begin_$dim"))
          val condition = 0 EqEq IR_MemberAccess(IR_VariableAccess("threadIdx", IR_SpecialDatatype("dim3")), CUDA_Util.dimToMember(dim))
          val conditionBody = ListBuffer[IR_Statement]()

          // 7.2 Calculate the offset from the left to the right border of the actual field
          val localFieldOffsetName : String = "localFieldOffset"
          conditionBody += IR_VariableDeclaration(IR_IntegerDatatype, localFieldOffsetName, Some(
            CUDA_Minimum(
              IR_Subtraction(IR_MemberAccess(IR_VariableAccess("blockDim", IR_SpecialDatatype("dim3")), CUDA_Util.dimToMember(dim)),
                IR_MemberAccess(IR_VariableAccess("threadIdx", IR_SpecialDatatype("dim3")), CUDA_Util.dimToMember(dim))),
              IR_Subtraction(s"${ KernelVariablePrefix }end_$dim", globalThreadId(dim)))))
          val localFieldOffset = IR_VariableAccess(localFieldOffsetName, IR_IntegerDatatype)

          // 7.3 Calculate the indices for writing into the shared memory and loading from the global memory
          // 7.4 Thread residing on left border should load left neighbor and the right neighbor of the point residing
          // on the right border of the actual field
          (1L to leftDeviations(field)(dim)).foreach(x => {
            val localLeftIndex = Duplicate(localThreadId(field))
            localLeftIndex(dim) = IR_Subtraction(localLeftIndex(dim), x)
            val globalLeftIndex = IR_ExpressionIndex(Duplicate(globalThreadId)) + fieldOffset(field)
            globalLeftIndex(dim) = IR_Subtraction(globalLeftIndex(dim), x)

            conditionBody += IR_Assignment(new CUDA_SharedArrayAccess(KernelVariablePrefix + field, localLeftIndex.reverse, sharedArrayStrides), IR_DirectFieldAccess(fieldForSharedMemory(field).field, Duplicate(fieldForSharedMemory(field).slot), globalLeftIndex).linearize)
          })
          (0L until rightDeviations(field)(dim)).foreach(x => {
            val localRightIndex = Duplicate(localThreadId(field))
            localRightIndex(dim) = IR_Addition(IR_Addition(localRightIndex(dim), localFieldOffset), x)
            val globalRightIndex = IR_ExpressionIndex(Duplicate(globalThreadId)) + fieldOffset(field)
            globalRightIndex(dim) = IR_Addition(IR_Addition(globalRightIndex(dim), localFieldOffset), x)

            conditionBody += IR_Assignment(new CUDA_SharedArrayAccess(KernelVariablePrefix + field, localRightIndex.reverse, sharedArrayStrides), IR_DirectFieldAccess(fieldForSharedMemory(field).field, Duplicate(fieldForSharedMemory(field).slot), globalRightIndex).linearize)
          })

          IR_IfCondition(condition, conditionBody)
        })

        if (spatialBlockingCanBeApplied) {
          // 8. Complete loop body for spatial blocking
          zDimLoopBody += IR_IfCondition(conditionAccess, sharedMemoryStatements)
          zDimLoopBody += CUDA_SyncThreads()
        } else {
          // 10. Add whole shared memory initialization wrapped in a ConditionStatement to the body
          statements += IR_IfCondition(conditionAccess, sharedMemoryStatements)
        }
      })

      if (spatialBlockingCanBeApplied) {
        // 8. Complete loop body for spatial blocking
        zDimLoopBody += IR_IfCondition(conditionAccess, body)
        zDimLoopBody += CUDA_SyncThreads()

        statements += IR_ForLoop(IR_VariableDeclaration(IR_IntegerDatatype, loopVariables(executionDim), s"${ KernelVariablePrefix }begin_$executionDim"), IR_Lower(IR_VariableAccess(loopVariables(executionDim), IR_IntegerDatatype), s"${ KernelVariablePrefix }end_$executionDim"), IR_Assignment(loopVariables(executionDim), IR_IntegerConstant(1), "+="), zDimLoopBody)

        // 9. Remove the used loop variable to avoid later complications in loop variable substitution
        loopVariables.remove(executionDim)
      } else {
        // This may not be part of the ConditionStatement to avoid dead locks if some thread do not fulfill the condition
        statements += CUDA_SyncThreads()
        statements += IR_IfCondition(conditionAccess, body)
      }
    } else {
      statements += IR_IfCondition(conditionAccess, body)
    }

    body = statements

    // add actual body after replacing field and iv accesses
    // replace FieldAccess nodes in body with shared memory accesses
    if (smemCanBeUsed) {
      fieldNames.foreach(field => {
        CUDA_ReplaceFieldAccessLike.fieldToOffset = field
        CUDA_ReplaceFieldAccessLike.fieldOffset = fieldOffset(field)
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
    CUDA_ReplaceLoopVariables.loopVariables = loopVariables.drop(nrInnerSeqDims)
    CUDA_ReplaceLoopVariables.applyStandalone(IR_Scope(body))

    body
  }

  def compileWrapperFunction : IR_Function = {
    evalIndexBounds() // ensure that minimal and maximal indices are set correctly
    evalAccesses() // ensure that field accesses have been mapped
    evalExecutionConfiguration() // ensure that execution configuration is already calculated

    // substitute loop variable in bounds with appropriate fix values to get valid code in wrapper function
    CUDA_ReplaceLoopVariablesInWrapper.loopVariables.clear
    CUDA_ReplaceLoopVariablesInWrapper.loopVariables = loopVariables.drop(nrInnerSeqDims)
    // lowerArgs is not needed anymore since the initial values are directly computed in the CUDA code
    CUDA_ReplaceLoopVariablesInWrapper.bounds = Duplicate(lowerBounds.drop(nrInnerSeqDims))
    val lowerArgs = Duplicate(lowerBounds.drop(nrInnerSeqDims))
    CUDA_ReplaceLoopVariablesInWrapper.applyStandalone(lowerArgs)

    val upperArgs = Duplicate(upperBounds.drop(nrInnerSeqDims))
    CUDA_ReplaceLoopVariablesInWrapper.bounds = Duplicate(upperBounds.drop(nrInnerSeqDims))
    CUDA_ReplaceLoopVariablesInWrapper.applyStandalone(upperArgs)

    // compile arguments for device function call
    var callArgs = ListBuffer[IR_Expression]()

    for (dim <- 0 until parallelDims) {
      callArgs += lowerArgs(dim)
      callArgs += upperArgs(dim)
    }

    for (fieldAccess <- linearizedFieldAccesses)
      callArgs += CUDA_FieldDeviceData(fieldAccess._2.field, Duplicate(fieldAccess._2.slot), Duplicate(fieldAccess._2.fragIdx))

    if (Knowledge.cuda_useSharedMemory && fieldForSharedMemory.nonEmpty) {
      fieldNames.foreach(field => callArgs += CUDA_FieldDeviceData(fieldForSharedMemory(field).field, Duplicate(fieldForSharedMemory(field).slot)))
    }

    for (bufferAccess <- bufferAccesses) {
      val buffer = bufferAccess._2
      callArgs += CUDA_BufferDeviceData(buffer.field, buffer.direction, buffer.size, buffer.neighIdx)
    }

    for (ivAccess <- ivAccesses)
      callArgs += Duplicate(ivAccess._2)

    for (arg <- passThroughArgs)
      callArgs += arg.access

    val execCfg = new CUDA_ExecutionConfiguration(numBlocksPerDim.map(n => n : IR_Expression),
      numThreadsPerBlock.map(n => n : IR_Expression), CUDA_ComputeStream())

    var body = ListBuffer[IR_Statement]()

    if (reduction.isDefined) {
      val target = Duplicate(reduction.get.target)
      val resultDt = CUDA_Util.getReductionDatatype(target)
      val baseDt = resultDt.resolveBaseDatatype

      val bufSize = requiredThreadsPerDim.product
      val bufAccess = CUDA_ReductionDeviceData(bufSize, resultDt)
      var callArgsReduction = ListBuffer[IR_Expression](bufAccess, bufSize, execCfg.stream)

      body += CUDA_Memset(bufAccess, 0, bufSize, resultDt)
      body += CUDA_FunctionCall(getKernelFctName, callArgs, execCfg)

      // return value via pointer arg
      if (resultDt.isInstanceOf[IR_HigherDimensionalDatatype] && returnDt == IR_UnitDatatype) {
        val result = IR_FunctionArgument("reductionTmp", IR_PointerDatatype(baseDt))
        passThroughArgs += result
        callArgsReduction += result.access
      }

      // call default reduction kernel and potentially forward return value
      val callDefaultReductionKernel = IR_FunctionCall(CUDA_KernelFunctions.get.getRedKernelWrapperName(reduction.get.op, resultDt),
        callArgsReduction)
      body += (returnDt match {
        case IR_UnitDatatype =>
          callDefaultReductionKernel
        case _ =>
          IR_Return(Some(callDefaultReductionKernel))
      })

      CUDA_KernelFunctions.get.requiredRedKernels += Tuple2(reduction.get.op, Duplicate(target)) // request reduction kernel and wrapper
    } else {
      body += CUDA_FunctionCall(getKernelFctName, callArgs, execCfg)
    }

    val fct = IR_PlainFunction( /* FIXME: IR_LeveledFunction? */
      getWrapperFctName,
      returnDt,
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
      // begin is not needed anymore since the initial values are directly computed in the CUDA code
      fctParams += IR_FunctionArgument(s"${ KernelVariablePrefix }begin_$dim", IR_IntegerDatatype)
      fctParams += IR_FunctionArgument(s"${ KernelVariablePrefix }end_$dim", IR_IntegerDatatype)
    }

    for (fieldAccess <- linearizedFieldAccesses) {
      // add required parameter specifiers to take advantage of the read-only cache
      if (Knowledge.cuda_spatialBlockingWithROC && !writtenFieldAccesses.contains(fieldAccess._1)) {
        fctParams += IR_FunctionArgument(fieldAccess._1, IR_CUDAConstPointerDatatype(fieldAccess._2.field.resolveDeclType))
      } else {
        fctParams += IR_FunctionArgument(fieldAccess._1, IR_PointerDatatype(fieldAccess._2.field.resolveDeclType))
      }
    }

    if (fieldForSharedMemory.nonEmpty) {
      fieldNames.foreach(field => {
        fctParams += IR_FunctionArgument(field, IR_PointerDatatype(fieldDatatype(field).resolveDeclType))
      })
    }

    for (bufferAccess <- bufferAccesses)
      fctParams += IR_FunctionArgument(bufferAccess._1, IR_PointerDatatype(bufferAccess._2.field.resolveDeclType))

    for (ivAccess <- ivAccesses)
      fctParams += IR_FunctionArgument(ivAccess._1, ivAccess._2.resolveDatatype())

    for (variableAccess <- passThroughArgs)
      fctParams += IR_FunctionArgument(variableAccess.name, variableAccess.datatype)

    val fct = IR_PlainFunction( /* FIXME: IR_LeveledFunction? */ getKernelFctName, IR_UnitDatatype, fctParams, compileKernelBody)

    fct.allowInlining = false
    fct.allowFortranInterface = false
    fct.functionQualifiers = "__global__"

    fct.annotate("deviceOnly")

    fct
  }
}
