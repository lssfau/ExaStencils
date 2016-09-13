package exastencils.cuda

import scala.collection._
import scala.collection.mutable._
import scala.language.postfixOps

import exastencils.base.ir._
import exastencils.core._
import exastencils.data._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.prettyprinting._
import exastencils.util._

case class KernelFunctions() extends FunctionCollection("KernelFunctions/KernelFunctions",
  ListBuffer("cmath", "algorithm"), // provide math functions like sin, etc. as well as commonly used functions like min/max by default
  ListBuffer("Globals/Globals.h")) {

  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"
  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"
  if (Knowledge.cuda_enabled) {
    externalDependencies += "cuda.h"
    externalDependencies += "cuda_runtime.h"
  }

  var kernelCollection = ListBuffer[Kernel]()
  var requiredRedKernels = mutable.HashSet[String]()
  var counterMap = mutable.HashMap[String, Int]()

  def getIdentifier(fctName : String) : String = {
    val cnt = counterMap.getOrElse(fctName, -1) + 1
    counterMap.update(fctName, cnt)
    s"${ fctName }_k${ String.format("%03d", cnt : java.lang.Integer) }"
  }

  def addKernel(kernel : Kernel) = {
    kernelCollection += kernel
  }

  def convertToFunctions() = {
    for (kernel <- kernelCollection) {
      functions += kernel.compileKernelFunction
      functions += kernel.compileWrapperFunction
    }
    kernelCollection.clear // consume processed kernels

    // take care of reductions
    for (op <- requiredRedKernels) addDefaultReductionKernel(op)
    requiredRedKernels.clear // consume reduction requests
  }

  override def printSources = {
    for (f <- functions) {
      var fileName = f.asInstanceOf[FunctionStatement].name
      if (fileName.endsWith(Kernel.wrapperPostfix)) fileName = fileName.dropRight(Kernel.wrapperPostfix.length)
      val writer = PrettyprintingManager.getPrinter(s"${ baseName }_$fileName.cu")
      writer.addInternalDependency(s"$baseName.h")

      writer <<< f.prettyprint(PrintEnvironment.CUDA)
      writer <<< ""
    }
  }

  def addDefaultReductionKernel(op : String) = {
    val opAsIdent = IR_BinaryOperators.opAsIdent(op)
    val kernelName = "DefaultReductionKernel" + opAsIdent
    val wrapperName = kernelName + "_wrapper"

    // kernel function
    {
      def data = FunctionArgument("data", IR_PointerDatatype(IR_RealDatatype))
      def numElements = FunctionArgument("numElements", IR_IntegerDatatype /*FIXME: size_t*/)
      def stride = FunctionArgument("stride", IR_IntegerDatatype /*FIXME: size_t*/)
      def it = Duplicate(LoopOverDimensions.defItForDim(0))

      var fctBody = ListBuffer[IR_Statement]()

      // add index calculation
      // FIXME: datatype for VariableAccess
      fctBody += new VariableDeclarationStatement(it,
        MemberAccess(VariableAccess("blockIdx", None), it.name) *
          MemberAccess(VariableAccess("blockDim", None), it.name) +
          MemberAccess(VariableAccess("threadIdx", None), it.name))
      fctBody += AssignmentStatement(it, 2 * stride, "*=")

      // add index bounds conditions
      fctBody += new ConditionStatement(
        IR_OrOrExpression(IR_LowerExpression(it, 0), IR_GreaterEqualExpression(it, numElements)),
        ReturnStatement())

      // add values with stride
      fctBody += new ConditionStatement(
        IR_LowerExpression(it + stride, numElements),
        AssignmentStatement(ArrayAccess(data, it), IR_BinaryOperators.createExpression(op, ArrayAccess(data, it), ArrayAccess(data, it + stride))))

      // compile final kernel function
      var fct = FunctionStatement(
        IR_UnitDatatype,
        kernelName,
        ListBuffer(data, numElements, stride),
        fctBody,
        allowInlining = false, allowFortranInterface = false, "__global__")
      fct.annotate("deviceOnly")
      functions += fct
    }

    // wrapper function
    {
      def numElements = VariableAccess("numElements", Some(IR_SpecialDatatype("size_t") /*FIXME*/))
      def stride = VariableAccess("stride", Some(IR_SpecialDatatype("size_t") /*FIXME*/))
      def data = FunctionArgument("data", IR_PointerDatatype(IR_RealDatatype))
      def ret = VariableAccess("ret", Some(IR_RealDatatype))

      def blockSize = Knowledge.cuda_reductionBlockSize

      var fctBody = ListBuffer[IR_Statement]()

      // compile loop body
      def blocks = VariableAccess("blocks", Some(IR_SpecialDatatype("size_t")))
      var loopBody = ListBuffer[IR_Statement]()
      loopBody += new VariableDeclarationStatement(blocks, (numElements + (blockSize * stride - 1)) / (blockSize * stride))
      loopBody += new ConditionStatement(IR_EqEqExpression(0, blocks), AssignmentStatement(blocks, 1))
      loopBody += CUDA_FunctionCallExpression(kernelName, ListBuffer[IR_Expression](data, numElements, stride),
        Array[IR_Expression](blocks * blockSize /*FIXME: avoid x*BS/BS */), Array[IR_Expression](blockSize))

      fctBody += ForLoopStatement(
        new VariableDeclarationStatement(stride, 1),
        IR_LowerExpression(stride, numElements),
        AssignmentStatement(stride, 2, "*="),
        loopBody)

      fctBody += new VariableDeclarationStatement(ret)
      fctBody += CUDA_Memcpy(IR_AddressofExpression(ret), data, SizeOfExpression(IR_RealDatatype), "cudaMemcpyDeviceToHost")

      fctBody += ReturnStatement(Some(ret))

      // compile final wrapper function
      functions += FunctionStatement(
        IR_RealDatatype, // TODO: support other types
        wrapperName,
        ListBuffer(data, FunctionArgument("numElements", IR_IntegerDatatype /*FIXME: size_t*/)),
        fctBody,
        allowInlining = false, allowFortranInterface = false,
        "extern \"C\"")
    }
  }
}

object Kernel {
  def wrapperPostfix = "_wrapper"
  val KernelVariablePrefix = "_cu_"
  val KernelGlobalIndexPrefix = "global_"
  val KernelLocalIndexPrefix = "local_"
  val CUDASharedMemoryAccess = "CUDASharedMemoryAccess"
  val ConstantIndexPart = "ConstantIndexPart"
}

case class Kernel(var identifier : String,
    var parallelDims : Int,
    var passThroughArgs : ListBuffer[FunctionArgument],
    var loopVariables : ListBuffer[String],
    var lowerBounds : ListBuffer[IR_Expression],
    var upperBounds : ListBuffer[IR_Expression],
    var stepSize : ListBuffer[IR_Expression],
    var body : ListBuffer[IR_Statement],
    var reduction : Option[Reduction] = None,
    var loopVariableExtrema : mutable.Map[String, (Long, Long)] = mutable.Map[String, (Long, Long)]()) extends Node {

  import Kernel._

  var originalParallelDims = parallelDims
  var firstNSeqDims = loopVariables.size - parallelDims
  var smemCanBeUsed = Knowledge.cuda_useSharedMemory && firstNSeqDims == 0 && stepSize.forall(x => IR_IntegerConstant(1).equals(x))
  var spatialBlockingCanBeApplied = smemCanBeUsed && Knowledge.cuda_spatialBlockingWithSmem && parallelDims == Platform.hw_cuda_maxNumDimsBlock
  var executionDim = if (spatialBlockingCanBeApplied) parallelDims - 1 else math.min(Platform.hw_cuda_maxNumDimsBlock, parallelDims)

  // properties required for shared memory analysis and shared memory allocation
  var fieldNames = ListBuffer[String]()
  var fieldBaseIndex = mutable.HashMap[String, MultiIndex]()
  var fieldForSharedMemory = mutable.HashMap[String, DirectFieldAccess]()
  var fieldOffset = mutable.HashMap[String, MultiIndex]()
  var fieldAccessesForSharedMemory = mutable.HashMap[String, List[FieldAccessLike]]()
  var leftDeviations = mutable.HashMap[String, Array[Long]]()
  var leftDeviation = mutable.HashMap[String, Long]()
  var rightDeviations = mutable.HashMap[String, Array[Long]]()
  var rightDeviation = mutable.HashMap[String, Long]()
  var sharedArraySize = mutable.HashMap[String, Array[Long]]()
  var fieldDatatype = mutable.HashMap[String, IR_Datatype]()

  var evaluatedAccesses = false
  var linearizedFieldAccesses = mutable.HashMap[String, LinearizedFieldAccess]()
  var writtenFieldAccesses = mutable.HashMap[String, LinearizedFieldAccess]()
  var ivAccesses = mutable.HashMap[String, iv.InternalVariable]()

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
        globalThreadId = globalThreadId :+ VariableAccess(loopVariables(executionDim), Some(IR_IntegerDatatype))
      }
      evaluateAccessesForSharedMemory()
    }
  }

  /**
    * Perform shared memory analysis and collect all FieldAccesses that are worth to store in shared memory.
    */
  def evaluateAccessesForSharedMemory() : Unit = {
    // 1. get all field accesses in the body and ask for amount of shared memory
    GatherLocalFieldAccessLikeForSharedMemory.loopVariables.clear
    GatherLocalFieldAccessLikeForSharedMemory.loopVariables = loopVariables
    GatherLocalFieldAccessLikeForSharedMemory.fieldAccesses.clear
    GatherLocalFieldAccessLikeForSharedMemory.fieldIndicesConstantPart.clear
    GatherLocalFieldAccessLikeForSharedMemory.maximalFieldDim = math.min(parallelDims, Platform.hw_cuda_maxNumDimsBlock)
    GatherLocalFieldAccessLikeForSharedMemory.writtenFields.clear
    GatherLocalFieldAccessLikeForSharedMemory.applyStandalone(IR_Scope(body))
    var fieldToFieldAccesses = GatherLocalFieldAccessLikeForSharedMemory.fieldAccesses
    val writtenFields = GatherLocalFieldAccessLikeForSharedMemory.writtenFields
    val fieldIndicesConstantPart = GatherLocalFieldAccessLikeForSharedMemory.fieldIndicesConstantPart
    var availableSharedMemory = if (Knowledge.cuda_favorL1CacheOverSharedMemory) Platform.hw_cuda_cacheMemory.toLong else Platform.hw_cuda_sharedMemory.toLong

    // 2. Perform shared memory analysis
    // the more field accesses to a field the more important it is to store this field in shared memory
    fieldToFieldAccesses = fieldToFieldAccesses.filter(fa => fa._2.nonEmpty)
    fieldToFieldAccesses = mutable.ListMap(fieldToFieldAccesses.toSeq.sortWith(_._2.size > _._2.size) : _*)
    var foundSomeAppropriateField = false
    for (fa <- fieldToFieldAccesses) {
      val name = fa._1
      val fieldAccesses = fa._2

      // 2.1 colllect some basic informations required for further calculations
      var requiredMemoryInByte = 0L
      val offset = fieldAccesses.head.fieldSelection.fieldLayout.referenceOffset
      val baseIndex = (loopVariables.take(offset.length), offset).zipped.map((x, y) => IR_AdditionExpression(VariableAccess(x), y)).toArray[IR_Expression]

      // 2.2 calculate negative and positive deviation from the basic field index
      val leftDeviationFromBaseIndex = fieldIndicesConstantPart(name).foldLeft(Array.fill(parallelDims)(0L))((acc, m) => (acc, m, offset).zipped.map((x, y, z) => {
        math.min(SimplifyExpression.evalIntegral(x), SimplifyExpression.evalIntegral(IR_SubtractionExpression(y, z)))
      })).map(x => math.abs(x))
      var firstDeviation = leftDeviationFromBaseIndex.head
      var isSameRadius = leftDeviationFromBaseIndex.forall(x => firstDeviation.equals(x))

      val rightDeviationFromBaseIndex = fieldIndicesConstantPart(name).foldLeft(Array.fill(parallelDims)(0L))((acc, m) => (acc, m, offset).zipped.map((x, y, z) => {
        math.max(SimplifyExpression.evalIntegral(x), SimplifyExpression.evalIntegral(IR_SubtractionExpression(y, z)))
      }))
      firstDeviation = rightDeviationFromBaseIndex.head
      isSameRadius &= rightDeviationFromBaseIndex.forall(x => firstDeviation.equals(x))
      isSameRadius &= leftDeviationFromBaseIndex.head.equals(rightDeviationFromBaseIndex.head)

      if (spatialBlockingCanBeApplied && !isSameRadius) {
        Logger.warning("Cannot apply spatial blocking with shared memory, because the stencil is not symmetric")
        spatialBlockingCanBeApplied = false
        executionDim = if (spatialBlockingCanBeApplied) parallelDims - 1 else math.min(Platform.hw_cuda_maxNumDimsBlock, parallelDims)
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
        val access = DirectFieldAccess(fieldAccesses.head.fieldSelection, MultiIndex(baseIndex))
        access.annotate(LinearizeFieldAccesses.NO_LINEARIZATION)
        fieldNames += name
        fieldBaseIndex(name) = MultiIndex(baseIndex)
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
    fieldToFieldAccesses.foreach(fa => fa._2.foreach(a => a.removeAnnotation(LinearizeFieldAccesses.NO_LINEARIZATION)))

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
      VariableAccess(KernelVariablePrefix + KernelLocalIndexPrefix + dimToString(dim), Some(IR_IntegerDatatype))
    }).toArray[IR_Expression]
    globalThreadId = (0 until executionDim).map(dim => {
      VariableAccess(KernelVariablePrefix + KernelGlobalIndexPrefix + dimToString(dim), Some(IR_IntegerDatatype))
    }).toArray[IR_Expression]
  }

  /**
    * Check the accesses in the loop to create valid function calls.
    */
  def evalAccesses() = {
    if (!evaluatedAccesses) {
      GatherLocalLinearizedFieldAccess.fieldAccesses.clear
      GatherLocalLinearizedFieldAccess.applyStandalone(IR_Scope(body))
      linearizedFieldAccesses = GatherLocalLinearizedFieldAccess.fieldAccesses

      if (Knowledge.cuda_spatialBlockingWithROC) {
        GatherWrittenLocalLinearizedFieldAccess.writtenFieldAccesses.clear
        GatherWrittenLocalLinearizedFieldAccess.applyStandalone(IR_Scope(body))
        writtenFieldAccesses = GatherWrittenLocalLinearizedFieldAccess.writtenFieldAccesses
      }

      GatherLocalIVs.ivAccesses.clear
      GatherLocalIVs.applyStandalone(IR_Scope(body))
      ivAccesses = GatherLocalIVs.ivAccesses

      // postprocess iv's -> generate parameter names
      var cnt = 0
      val processedIVs = mutable.HashMap[String, iv.InternalVariable]()
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
        }).toArray.reverse

      maxIndices = (loopVariables.size - 1 to 0 by -1).map(dim =>
        loopVariableExtrema.get(loopVariables(dim)) match {
          case Some((min : Long, max : Long)) => max
          case _                              =>
            Logger.warn(s"Start index for dimension $dim (${ upperBounds(dim) }) could not be evaluated")
            0
        }).toArray.reverse

      evaluatedIndexBounds = true
    }
  }

  /**
    * Calculate the execution configuration for this kernel.
    */
  def evalExecutionConfiguration() = {
    if (!evaluatedExecutionConfiguration) {
      requiredThreadsPerDim = (maxIndices.drop(firstNSeqDims), minIndices.drop(firstNSeqDims)).zipped.map(_ - _)

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
      val it = dimToString(dim)
      val variableName = KernelVariablePrefix + KernelGlobalIndexPrefix + it
      VariableDeclarationStatement(IR_IntegerDatatype, variableName,
        Some(stepSize(dim) * (MemberAccess(VariableAccess("blockIdx", Some(IR_SpecialDatatype("dim3"))), it) *
          MemberAccess(VariableAccess("blockDim", Some(IR_SpecialDatatype("dim3"))), it) +
          MemberAccess(VariableAccess("threadIdx", Some(IR_SpecialDatatype("dim3"))), it) +
          minIndices(dim))))
    })

    // add dimension index start and end point
    // add index bounds conditions
    val conditionParts = (0 until executionDim).map(dim => {
      val variableAccess = VariableAccess(KernelVariablePrefix + KernelGlobalIndexPrefix + dimToString(dim), Some(IR_IntegerDatatype))
      IR_AndAndExpression(
        IR_GreaterEqualExpression(variableAccess, s"${ KernelVariablePrefix }begin_$dim"), IR_LowerExpression(variableAccess, s"${ KernelVariablePrefix }end_$dim"))
    })

    val condition = VariableDeclarationStatement(IR_BooleanDatatype, KernelVariablePrefix + "condition",
      Some(conditionParts.reduceLeft[IR_AndAndExpression] { (acc, n) =>
        IR_AndAndExpression(acc, n)
      }))
    val conditionAccess = VariableAccess(KernelVariablePrefix + "condition", Some(IR_BooleanDatatype))
    statements += condition

    if (smemCanBeUsed && fieldForSharedMemory.nonEmpty) {
      fieldNames.foreach(field => {
        val localPrefix = KernelVariablePrefix + "local_"
        val sharedMemoryStatements = ListBuffer[IR_Statement]()
        val sharedArrayStrides = new MultiIndex(sharedArraySize(field))
        var zDimLoopBody = ListBuffer[IR_Statement]()
        val current = new VariableAccess("current", IR_IntegerDatatype)

        // 1. Annotate the loop variables appearing in the shared memory accesses to guarantee the right substitution later
        AnnotatingLoopVariablesForSharedMemoryAccess.loopVariables = loopVariables
        AnnotatingLoopVariablesForSharedMemoryAccess.accessName = field
        AnnotatingLoopVariablesForSharedMemoryAccess.applyStandalone(IR_Scope(fieldAccessesForSharedMemory(field).map(x => IR_ExpressionStatement(x))))

        // 2. Add local Thread ID calculation for indexing shared memory
        statements ++= (0 until executionDim).map(dim => {
          val it = dimToString(dim)
          val variableName = localPrefix + it
          VariableDeclarationStatement(IR_IntegerDatatype, variableName,
            Some(MemberAccess(VariableAccess("threadIdx", Some(IR_SpecialDatatype("dim3"))), it) +
              leftDeviations(field)(dim)))
        })

        // 3. Add shared memory declarations
        statements += CUDA_SharedArray(KernelVariablePrefix + field, fieldDatatype(field), sharedArraySize(field).reverse)

        if (spatialBlockingCanBeApplied) {
          // 4. Declarations for neighbors and current point
          val spatialBaseIndex = MultiIndex(fieldBaseIndex(field).indices.take(executionDim) :+ fieldOffset(field).indices(executionDim))
          (1L to leftDeviation(field)).foreach(x => {
            statements += new VariableDeclarationStatement(fieldDatatype(field), "behind" + x, 0)
          })
          (1L to rightDeviation(field)).foreach(x => {
            statements += new VariableDeclarationStatement(fieldDatatype(field), "infront" + x, DirectFieldAccess(fieldForSharedMemory(field).fieldSelection, spatialBaseIndex + new MultiIndex(Array[Long](0, 0, x))).linearize)
          })
          statements += new VariableDeclarationStatement(fieldDatatype(field), "current", DirectFieldAccess(fieldForSharedMemory(field).fieldSelection, spatialBaseIndex).linearize)

          // 5. Add statements for loop body in kernel (z-Dim)
          // 5.1 advance the slice (move the thread front)
          (leftDeviation(field) to 2L by -1).foreach(x => {
            sharedMemoryStatements += AssignmentStatement(VariableAccess("behind" + x), VariableAccess("behind" + (x - 1)))
          })
          sharedMemoryStatements += AssignmentStatement(VariableAccess("behind1"), current)
          sharedMemoryStatements += AssignmentStatement(current, VariableAccess("infront1"))
          (2L to rightDeviation(field)).foreach(x => {
            sharedMemoryStatements += AssignmentStatement(VariableAccess("infront" + x), VariableAccess("infront" + (x + 1)))
          })

          sharedMemoryStatements += AssignmentStatement(VariableAccess("infront" + rightDeviation(field)), DirectFieldAccess(fieldForSharedMemory(field).fieldSelection, fieldBaseIndex(field) + new MultiIndex(Array[Long](0, 0, 1))).linearize)

          // 5.2 load from global memory into shared memory
          sharedMemoryStatements += AssignmentStatement(new CUDA_SharedArrayAccess(KernelVariablePrefix + field, localThreadId.take(executionDim).reverse, sharedArrayStrides), current)
        } else {
          // 6. Load from global memory into shared memory
          sharedMemoryStatements += AssignmentStatement(new CUDA_SharedArrayAccess(KernelVariablePrefix + field, localThreadId.reverse, sharedArrayStrides), DirectFieldAccess(fieldForSharedMemory(field).fieldSelection, fieldForSharedMemory(field).index).linearize)
        }

        // 7. Add load operations as ConditionStatement to avoid index out of bounds exceptions in global memory
        // and sync threads afterwards to guarantee that every thread has the same memory state
        sharedMemoryStatements ++= (0 until executionDim).map(dim => {
          val it = dimToString(dim)

          // 7.1 Check if current thread resides on the left border in any dimension
          val condition = IR_OrOrExpression(IR_LowerExpression(MemberAccess(VariableAccess("threadIdx", Some(IR_SpecialDatatype("dim3"))), it), leftDeviations(field)(dim)), IR_EqEqExpression(globalThreadId(dim), s"${ KernelVariablePrefix }begin_$dim"))
          val conditionBody = ListBuffer[IR_Statement]()

          // 7.2 Calculate the offset from the left to the right border of the actual field
          val localFieldOffsetName : String = "localFieldOffset"
          conditionBody += VariableDeclarationStatement(IR_IntegerDatatype, localFieldOffsetName, Some(
            CUDA_MinimumExpression(
              IR_SubtractionExpression(MemberAccess(VariableAccess("blockDim", Some(IR_SpecialDatatype("dim3"))), it),
                MemberAccess(VariableAccess("threadIdx", Some(IR_SpecialDatatype("dim3"))), it)),
              IR_SubtractionExpression(s"${ KernelVariablePrefix }end_$dim", globalThreadId(dim)))))
          val localFieldOffset = VariableAccess(localFieldOffsetName, Some(IR_IntegerDatatype))

          // 7.3 Calculate the indices for writing into the shared memory and loading from the global memory
          // 7.4 Thread residing on left border should load left neighbor and the right neighbor of the point residing
          // on the right border of the actual field
          (1L to leftDeviations(field)(dim)).foreach(x => {
            val localLeftIndex = Duplicate(localThreadId)
            localLeftIndex(dim) = IR_SubtractionExpression(localLeftIndex(dim), x)
            val globalLeftIndex = MultiIndex(Duplicate(globalThreadId)) + fieldOffset(field)
            globalLeftIndex(dim) = IR_SubtractionExpression(globalLeftIndex(dim), x)

            conditionBody += AssignmentStatement(new CUDA_SharedArrayAccess(KernelVariablePrefix + field, localLeftIndex.reverse, sharedArrayStrides), DirectFieldAccess(fieldForSharedMemory(field).fieldSelection, globalLeftIndex).linearize)
          })
          (0L until rightDeviations(field)(dim)).foreach(x => {
            val localRightIndex = Duplicate(localThreadId)
            localRightIndex(dim) = IR_AdditionExpression(IR_AdditionExpression(localRightIndex(dim), localFieldOffset), x)
            val globalRightIndex = MultiIndex(Duplicate(globalThreadId)) + fieldOffset(field)
            globalRightIndex(dim) = IR_AdditionExpression(IR_AdditionExpression(globalRightIndex(dim), localFieldOffset), x)

            conditionBody += AssignmentStatement(new CUDA_SharedArrayAccess(KernelVariablePrefix + field, localRightIndex.reverse, sharedArrayStrides), DirectFieldAccess(fieldForSharedMemory(field).fieldSelection, globalRightIndex).linearize)
          })

          new ConditionStatement(condition, conditionBody)
        })

        if (spatialBlockingCanBeApplied) {
          // 8. Complete loop body for spatial blocking
          zDimLoopBody += new ConditionStatement(conditionAccess, sharedMemoryStatements)
          zDimLoopBody += CUDA_SyncThreads()
          zDimLoopBody += new ConditionStatement(conditionAccess, body)
          zDimLoopBody += CUDA_SyncThreads()

          statements += ForLoopStatement(new VariableDeclarationStatement(IR_IntegerDatatype, loopVariables(executionDim), s"${ KernelVariablePrefix }begin_$executionDim"), IR_LowerExpression(VariableAccess(loopVariables(executionDim)), s"${ KernelVariablePrefix }end_$executionDim"), AssignmentStatement(loopVariables(executionDim), IR_IntegerConstant(1), "+="), zDimLoopBody)

          // 9. Remove the used loop variable to avoid later complications in loop variable substitution
          loopVariables.remove(executionDim)
        } else {
          // 10. Add whole shared memory initialization wrapped in a ConditionStatement to the body
          statements += new ConditionStatement(conditionAccess, sharedMemoryStatements)

          // This may not be part of the ConditionStatement to avoid dead locks if some thread do not fulfill the condition
          statements += CUDA_SyncThreads()
          statements += new ConditionStatement(conditionAccess, body)
        }
      })
    } else {
      statements += new ConditionStatement(conditionAccess, body)
    }

    body = statements

    // add actual body after replacing field and iv accesses
    // replace FieldAccess nodes in body with shared memory accesses
    if (smemCanBeUsed) {
      fieldNames.foreach(field => {
        ReplacingLocalFieldAccessLikeForSharedMemory.fieldToOffset = fieldOffset(field)
        ReplacingLocalFieldAccessLikeForSharedMemory.offsetForSharedMemoryAccess = leftDeviation(field)
        ReplacingLocalFieldAccessLikeForSharedMemory.sharedArrayStrides = sharedArraySize(field)
        ReplacingLocalFieldAccessLikeForSharedMemory.executionDim = executionDim
        ReplacingLocalFieldAccessLikeForSharedMemory.baseIndex = fieldBaseIndex(field)
        ReplacingLocalFieldAccessLikeForSharedMemory.applySpatialBlocking = spatialBlockingCanBeApplied
        ReplacingLocalFieldAccessLikeForSharedMemory.applyStandalone(IR_Scope(body))
      })
    }

    ReplacingLocalLinearizedFieldAccess.applyStandalone(IR_Scope(body))
    ReplacingLocalIVs.ivAccesses = ivAccesses
    ReplacingLocalIVs.applyStandalone(IR_Scope(body))
    ReplacingLocalIVArrays.applyStandalone(IR_Scope(body))
    ReplacingLoopVariables.loopVariables = loopVariables.drop(firstNSeqDims)
    ReplacingLoopVariables.applyStandalone(IR_Scope(body))

    body
  }

  def compileWrapperFunction : FunctionStatement = {
    evalIndexBounds() // ensure that minimal and maximal indices are set correctly
    evalAccesses() // ensure that field accesses have been mapped
    evalExecutionConfiguration() // ensure that execution configuration is already calculated

    // substitute loop variable in bounds with appropriate fix values to get valid code in wrapper function
    ReplacingLoopVariablesInWrapper.loopVariables.clear
    ReplacingLoopVariablesInWrapper.loopVariables = loopVariables.drop(firstNSeqDims)
    ReplacingLoopVariablesInWrapper.bounds = Duplicate(lowerBounds.drop(firstNSeqDims))
    val lowerArgs = Duplicate(lowerBounds.drop(firstNSeqDims))
    ReplacingLoopVariablesInWrapper.applyStandalone(lowerArgs)

    val upperArgs = Duplicate(upperBounds.drop(firstNSeqDims))
    ReplacingLoopVariablesInWrapper.bounds = Duplicate(upperBounds.drop(firstNSeqDims))
    ReplacingLoopVariablesInWrapper.applyStandalone(upperArgs)

    // compile arguments for device function call
    var callArgs = ListBuffer[IR_Expression]()

    for (dim <- 0 until parallelDims) {
      callArgs += lowerArgs(dim)
      callArgs += upperArgs(dim)
    }

    for (fieldAccess <- linearizedFieldAccesses) {
      val fieldSelection = fieldAccess._2.fieldSelection
      callArgs += iv.FieldDeviceData(fieldSelection.field, fieldSelection.level, fieldSelection.slot)
    }

    if (Knowledge.cuda_useSharedMemory && fieldForSharedMemory.nonEmpty) {
      fieldNames.foreach(field => {
        val fieldSelection = fieldForSharedMemory(field).fieldSelection
        callArgs += iv.FieldDeviceData(fieldSelection.field, fieldSelection.level, fieldSelection.slot)
      })
    }

    for (ivAccess <- ivAccesses) {
      val access = Duplicate(ivAccess._2)
      // Hack for Vec3 -> TODO: split Vec3 iv's into separate real iv's
      access.resolveDatatype match {
        case IR_SpecialDatatype("Vec3")  => callArgs += FunctionCallExpression("make_double3", (0 until 3).map(dim => ArrayAccess(ivAccess._2, dim) : IR_Expression).to[ListBuffer])
        case IR_SpecialDatatype("Vec3i") => callArgs ++= (0 until 3).map(dim => ArrayAccess(ivAccess._2, dim) : IR_Expression).to[ListBuffer]
        case _                           => callArgs += ivAccess._2
      }
    }

    for (variableAccess <- passThroughArgs) {
      callArgs += Duplicate(variableAccess)
    }

    var body = ListBuffer[IR_Statement]()

    if (reduction.isDefined) {
      def bufSize = requiredThreadsPerDim.product
      def bufAccess = iv.ReductionDeviceData(bufSize)
      body += CUDA_Memset(bufAccess, 0, bufSize, reduction.get.target.datatype.get)
      body += new CUDA_FunctionCallExperimentalExpression(getKernelFctName, callArgs, numThreadsPerBlock, numBlocksPerDim)
      body += ReturnStatement(Some(FunctionCallExpression(s"DefaultReductionKernel${ IR_BinaryOperators.opAsIdent(reduction.get.op) }_wrapper",
        ListBuffer[IR_Expression](bufAccess, bufSize))))

      StateManager.findFirst[KernelFunctions]().get.requiredRedKernels += reduction.get.op // request reduction kernel and wrapper
    } else {
      body += new CUDA_FunctionCallExperimentalExpression(getKernelFctName, callArgs, numThreadsPerBlock, numBlocksPerDim)
    }

    FunctionStatement(
      if (reduction.isDefined) reduction.get.target.datatype.get else IR_UnitDatatype,
      getWrapperFctName,
      Duplicate(passThroughArgs),
      body,
      allowInlining = false, allowFortranInterface = false,
      "extern \"C\"")
  }

  def compileKernelFunction : FunctionStatement = {
    evalIndexBounds() // ensure that minimal and maximal indices are set correctly
    evalAccesses() // ensure that field accesses have been mapped
    evalExecutionConfiguration() // ensure that execution configuration is already calculated

    // compile parameters for device function
    var fctParams = ListBuffer[FunctionArgument]()

    for (dim <- 0 until parallelDims) {
      fctParams += FunctionArgument(s"${ KernelVariablePrefix }begin_$dim", IR_IntegerDatatype)
      fctParams += FunctionArgument(s"${ KernelVariablePrefix }end_$dim", IR_IntegerDatatype)
    }

    for (fieldAccess <- linearizedFieldAccesses) {
      val fieldSelection = fieldAccess._2.fieldSelection

      // add required parameter specifiers to take advantage of the read-only cache
      if (Knowledge.cuda_spatialBlockingWithROC && !writtenFieldAccesses.contains(fieldAccess._1)) {
        fctParams += FunctionArgument(fieldAccess._1, IR_CUDAConstPointerDatatype(fieldSelection.field.resolveDeclType))
      } else {
        fctParams += FunctionArgument(fieldAccess._1, IR_PointerDatatype(fieldSelection.field.resolveDeclType))
      }
    }

    if (fieldForSharedMemory.nonEmpty) {
      fieldNames.foreach(field => {
        fctParams += FunctionArgument(field, IR_PointerDatatype(fieldForSharedMemory(field).fieldSelection.field.resolveDeclType))
      })
    }

    for (ivAccess <- ivAccesses) {
      val access = VariableAccess(ivAccess._1, Some(ivAccess._2.resolveDatatype))
      val datatype = ivAccess._2.resolveDatatype

      datatype match {
        case IR_SpecialDatatype("Vec3")  =>
          access.datatype = Some(IR_SpecialDatatype("double3"))
          fctParams += FunctionArgument(access.name, access.datatype.get)
        case IR_SpecialDatatype("Vec3i") =>
          fctParams ++= (0 until 3).map(dim => FunctionArgument(ivAccess._1 + '_' + dim, IR_SpecialDatatype("int"))).to[ListBuffer]
        case _                           => fctParams += FunctionArgument(ivAccess._1, datatype)
      }
    }

    for (variableAccess <- passThroughArgs) {
      fctParams += FunctionArgument(variableAccess.name, variableAccess.datatype)
    }

    val fct = FunctionStatement(
      IR_UnitDatatype, getKernelFctName, fctParams,
      compileKernelBody,
      allowInlining = false, allowFortranInterface = false, "__global__")

    fct.annotate("deviceOnly")

    fct
  }
}

object GatherLocalLinearizedFieldAccess extends QuietDefaultStrategy("Gathering local LinearizedFieldAccess nodes") {
  var fieldAccesses = mutable.HashMap[String, LinearizedFieldAccess]()

  def mapFieldAccess(access : LinearizedFieldAccess) = {
    val field = access.fieldSelection.field
    var identifier = field.codeName

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
    case access : LinearizedFieldAccess =>
      mapFieldAccess(access)
      access
  }, false)
}

object ReplacingLocalLinearizedFieldAccess extends QuietDefaultStrategy("Replacing local LinearizedFieldAccess nodes") {
  var fieldAccesses = mutable.HashMap[String, LinearizedFieldAccess]()

  def extractIdentifier(access : LinearizedFieldAccess) = {
    val field = access.fieldSelection.field
    var identifier = field.codeName

    // TODO: array fields
    if (field.numSlots > 1) {
      access.fieldSelection.slot match {
        case SlotAccess(_, offset)    => identifier += s"_o$offset"
        case IR_IntegerConstant(slot) => identifier += s"_s$slot"
        case _                        => identifier += s"_s${ access.fieldSelection.slot.prettyprint }"
      }
    }

    VariableAccess(identifier, Some(IR_PointerDatatype(field.resolveDeclType)))
  }

  this += new Transformation("Searching", {
    case access : LinearizedFieldAccess =>
      val identifier = extractIdentifier(access)
      ArrayAccess(identifier, access.index)
  })
}

object GatherWrittenLocalLinearizedFieldAccess extends QuietDefaultStrategy("Gathering local written LinearizedFieldAccess nodes for read-only cache usage") {
  var writtenFieldAccesses = mutable.HashMap[String, LinearizedFieldAccess]()

  def mapFieldAccess(access : LinearizedFieldAccess) = {
    val field = access.fieldSelection.field
    var identifier = field.codeName

    // TODO: array fields
    if (field.numSlots > 1) {
      access.fieldSelection.slot match {
        case SlotAccess(_, offset)    => identifier += s"_o$offset"
        case IR_IntegerConstant(slot) => identifier += s"_s$slot"
        case _                        => identifier += s"_s${ access.fieldSelection.slot.prettyprint }"
      }
    }

    writtenFieldAccesses.put(identifier, access)
  }

  this += new Transformation("Searching", {
    case stmt @ AssignmentStatement(access @ LinearizedFieldAccess(fieldSelection : FieldSelection, index : IR_Expression), _, _) =>
      mapFieldAccess(access)
      stmt
  })
}

object GatherLocalFieldAccessLikeForSharedMemory extends QuietDefaultStrategy("Gathering local FieldAccessLike nodes for shared memory") {
  var loopVariables = ListBuffer[String]()
  var fieldAccesses = new mutable.HashMap[String, List[FieldAccessLike]].withDefaultValue(Nil)
  var fieldIndicesConstantPart = new mutable.HashMap[String, List[Array[Long]]].withDefaultValue(Nil)
  var maximalFieldDim = Platform.hw_cuda_maxNumDimsBlock
  var writtenFields = ListBuffer[String]()

  def extractFieldIdentifier(access : FieldAccessLike) = {
    val field = access.fieldSelection.field
    var identifier = field.codeName

    if (field.numSlots > 1) {
      access.fieldSelection.slot match {
        case SlotAccess(_, offset)    => identifier += s"_o$offset"
        case IR_IntegerConstant(slot) => identifier += s"_s$slot"
        case _                        => identifier += s"_s${ access.fieldSelection.slot.prettyprint }"
      }
    }

    identifier
  }

  this += new Transformation("Searching", {
    case stmt @ AssignmentStatement(access : FieldAccessLike, _, _)                                   =>
      writtenFields += extractFieldIdentifier(access)
      stmt
    case access : FieldAccessLike if access.fieldSelection.fieldLayout.numDimsData <= maximalFieldDim =>
      val field = access.fieldSelection.field
      val identifier = extractFieldIdentifier(access)

      // Evaluate indices. Should be of the form "variable + offset". Ignore all other fields.
      var suitableForSharedMemory = field.fieldLayout.numDimsData <= Platform.hw_cuda_maxNumDimsBlock
      val accessIndices = access.index.indices
      val indexConstantPart = Array.fill[Long](accessIndices.length)(0)

      accessIndices.indices.foreach(i => {
        accessIndices(i) match {
          case IR_AdditionExpression(ListBuffer(va @ VariableAccess(name : String, _), IR_IntegerConstant(v : Long))) =>
            suitableForSharedMemory &= loopVariables.contains(name)
            indexConstantPart(i) = v
          case va @ VariableAccess(name : String, _)                                                                  =>
            suitableForSharedMemory &= loopVariables.contains(name)
          case IR_IntegerConstant(v : Long)                                                                           =>
            indexConstantPart(i) = v
          case _                                                                                                      =>
            suitableForSharedMemory = false
        }
      })

      if (suitableForSharedMemory) {
        access.annotate(LinearizeFieldAccesses.NO_LINEARIZATION)
        fieldAccesses(identifier) ::= access
        fieldIndicesConstantPart(identifier) ::= indexConstantPart
      }

      access
  }, true)
}

object ReplacingLocalFieldAccessLikeForSharedMemory extends QuietDefaultStrategy("Replacing local FieldAccessLike nodes for shared memory") {
  var fieldToOffset = new MultiIndex()
  var offsetForSharedMemoryAccess = 0L
  var sharedArrayStrides = Array[Long]()
  var executionDim = 0
  var baseIndex = new MultiIndex()
  var applySpatialBlocking = false

  def extractIdentifier(access : FieldAccessLike) = {
    val field = access.fieldSelection.field
    var identifier = field.codeName

    // TODO: array fields
    if (field.numSlots > 1) {
      access.fieldSelection.slot match {
        case SlotAccess(_, offset)    => identifier += s"_o$offset"
        case IR_IntegerConstant(slot) => identifier += s"_s$slot"
        case _                        => identifier += s"_s${ access.fieldSelection.slot.prettyprint }"
      }
    }

    identifier
  }

  this += new Transformation("Searching", {
    case access : FieldAccessLike =>
      val identifier = extractIdentifier(access)
      val deviation = (new MultiIndex(access.getAnnotation(Kernel.ConstantIndexPart).get.asInstanceOf[Array[Long]]) - fieldToOffset).indices

      if (applySpatialBlocking && deviation.take(executionDim).forall(x => SimplifyExpression.evalIntegral(x) == 0)) {
        SimplifyExpression.evalIntegral(deviation(executionDim)) match {
          case 0                                                        => VariableAccess("current")
          case x if 0L to offsetForSharedMemoryAccess contains x        => VariableAccess("infront" + x)
          case y if 0L to -offsetForSharedMemoryAccess by -1 contains y => VariableAccess("behind" + math.abs(y))
        }
      } else {
        new CUDA_SharedArrayAccess(VariableAccess(Kernel.KernelVariablePrefix + identifier, Some(IR_PointerDatatype(access.fieldSelection.field.resolveDeclType))), (access.index - fieldToOffset).indices.take(executionDim).reverse, new MultiIndex(sharedArrayStrides))
      }
  })
}

object GatherLocalIVs extends QuietDefaultStrategy("Gathering local InternalVariable nodes") {
  var ivAccesses = mutable.HashMap[String, iv.InternalVariable]()

  this += new Transformation("Searching", {
    case iv : iv.InternalVariable =>
      ivAccesses.put(iv.prettyprint, iv)
      iv
  }, false)
}

object ReplacingLocalIVs extends QuietDefaultStrategy("Replacing local InternalVariable nodes") {
  var ivAccesses = mutable.HashMap[String, iv.InternalVariable]()

  this += new Transformation("Searching", {
    case iv : iv.InternalVariable =>
      val ivAccess = ivAccesses.find(_._2 == iv).get // TODO: improve performance
      VariableAccess(ivAccess._1, Some(ivAccess._2.resolveDatatype))
  })
}

object ReplacingLocalIVArrays extends QuietDefaultStrategy("Replacing local ArrayAccess nodes with special vector datatype") {
  def checkAccess(ivArray : ArrayAccess) : Boolean = {
    var result = false

    (ivArray.base, ivArray.index) match {
      case (ivAccess : VariableAccess, i : IR_IntegerConstant) =>
        result = ivAccess.datatype.contains(IR_SpecialDatatype("Vec3i"))
      case _                                                   =>
    }

    result
  }

  this += new Transformation("Searching", {
    case ivArray : ArrayAccess if checkAccess(ivArray) =>
      val iv = ivArray.base.asInstanceOf[VariableAccess]
      val i = ivArray.index.asInstanceOf[IR_IntegerConstant]
      VariableAccess(iv.name + '_' + i.v, Some(IR_SpecialDatatype("double")))
  })
}

object ReplacingLoopVariables extends QuietDefaultStrategy("Replacing loop variables with generated kernel variables") {
  var loopVariables = ListBuffer[String]()

  this += new Transformation("Searching", {
    case v @ VariableAccess(name @ n, maybeDatatype @ d) if loopVariables.contains(name) =>
      var newName = Kernel.KernelVariablePrefix + Kernel.KernelGlobalIndexPrefix + dimToString(loopVariables.indexOf(name))

      if (v.hasAnnotation(Kernel.CUDASharedMemoryAccess)) {
        newName = Kernel.KernelVariablePrefix + "local_" + dimToString(loopVariables.indexOf(name))
      }

      VariableAccess(newName, Some(IR_IntegerDatatype))
    case s @ IR_StringLiteral(v @ value) if loopVariables.contains(v)                    =>
      var newName = Kernel.KernelVariablePrefix + Kernel.KernelGlobalIndexPrefix + dimToString(loopVariables.indexOf(v))

      if (s.hasAnnotation(Kernel.CUDASharedMemoryAccess)) {
        newName = Kernel.KernelVariablePrefix + "local_" + dimToString(loopVariables.indexOf(v))
      }

      VariableAccess(newName, Some(IR_IntegerDatatype))
  })
}

object AnnotatingLoopVariablesForSharedMemoryAccess extends QuietDefaultStrategy("Annotate loop variables for shared memory access") {
  var loopVariables = ListBuffer[String]()
  var accessName = ""

  this += new Transformation("Searching", {
    case v @ VariableAccess(name @ n, maybeDatatype @ d) if loopVariables.contains(name) =>
      v.annotate(Kernel.CUDASharedMemoryAccess, accessName)
      v
    case s @ IR_StringLiteral(v @ value) if loopVariables.contains(v)                    =>
      s.annotate(Kernel.CUDASharedMemoryAccess, accessName)
      s
  })
}

object ReplacingLoopVariablesInWrapper extends QuietDefaultStrategy("Replacing loop variables in wrapper with provided bounds expressions") {
  var loopVariables = ListBuffer[String]()
  var bounds = ListBuffer[IR_Expression]()

  this += new Transformation("Searching", {
    case IR_StringLiteral(v @ value) if loopVariables.contains(v) =>
      bounds(loopVariables.indexOf(v))

    case VariableAccess(n, Some(IR_IntegerDatatype)) if loopVariables.contains(n) =>
      bounds(loopVariables.indexOf(n))
  })
}
