package exastencils.cuda

import exastencils.core._
import exastencils.data._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.datastructures.ir.BinaryOperators.{apply => _}
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.prettyprinting._
import exastencils.util._

import scala.collection._
import scala.collection.mutable._
import scala.language.postfixOps

case class KernelFunctions() extends FunctionCollection("KernelFunctions/KernelFunctions",
  ListBuffer("cmath", "algorithm"), // provide math functions like sin, etc. as well as commonly used functions like min/max by default
  ListBuffer("Globals/Globals.h")) {

  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"
  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"
  if (Knowledge.experimental_cuda_enabled) {
    externalDependencies += "cuda.h"
    externalDependencies += "cuda_runtime.h"
  }

  var kernelCollection = ListBuffer[ExpKernel]()
  var requiredRedKernels = mutable.HashSet[String]()
  var counterMap = mutable.HashMap[String, Int]()

  def getIdentifier(fctName : String) : String = {
    val cnt = counterMap.getOrElse(fctName, -1) + 1
    counterMap.update(fctName, cnt)
    s"${fctName}_k${String.format("%03d", cnt : java.lang.Integer)}"
  }

  def addKernel(kernel : Kernel) = {
    //kernelCollection += kernel
  }

  def addKernel(kernel : ExpKernel) = {
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
      val writer = PrettyprintingManager.getPrinter(s"${baseName}_$fileName.cu")
      writer.addInternalDependency(s"$baseName.h")

      writer <<< f.prettyprint(PrintEnvironment.CUDA)
      writer <<< ""
    }
  }

  def addDefaultReductionKernel(op : String) = {
    val opAsIdent = BinaryOperators.opAsIdent(op)
    val kernelName = "DefaultReductionKernel" + opAsIdent
    val wrapperName = kernelName + "_wrapper"

    // kernel function
    {
      def data = VariableAccess("data", Some(PointerDatatype(RealDatatype)))
      def numElements = VariableAccess("numElements", Some(IntegerDatatype /*FIXME: size_t*/ ))
      def stride = VariableAccess("stride", Some(IntegerDatatype /*FIXME: size_t*/ ))
      def it = Duplicate(LoopOverDimensions.defItForDim(0))

      var fctBody = ListBuffer[Statement]()

      // add index calculation
      // FIXME: datatype for VariableAccess
      fctBody += new VariableDeclarationStatement(it,
        MemberAccess(VariableAccess("blockIdx", None), it.name) *
          MemberAccess(VariableAccess("blockDim", None), it.name) +
          MemberAccess(VariableAccess("threadIdx", None), it.name))
      fctBody += new AssignmentStatement(it, 2 * stride, "*=")

      // add index bounds conditions
      fctBody += new ConditionStatement(
        OrOrExpression(LowerExpression(it, 0), GreaterEqualExpression(it, numElements)),
        ReturnStatement())

      // add values with stride
      fctBody += new ConditionStatement(
        LowerExpression(it + stride, numElements),
        AssignmentStatement(ArrayAccess(data, it), BinaryOperators.CreateExpression(op, ArrayAccess(data, it), ArrayAccess(data, it + stride))))

      // compile final kernel function
      var fct = FunctionStatement(
        UnitDatatype,
        kernelName,
        ListBuffer(data, numElements, stride),
        fctBody,
        allowInlining = false, allowFortranInterface = false, "__global__")
      fct.annotate("deviceOnly")
      functions += fct
    }

    // wrapper function
    {
      def numElements = VariableAccess("numElements", Some(SpecialDatatype("size_t") /*FIXME*/ ))
      def stride = VariableAccess("stride", Some(SpecialDatatype("size_t") /*FIXME*/ ))
      def data = VariableAccess("data", Some(PointerDatatype(RealDatatype)))
      def ret = VariableAccess("ret", Some(RealDatatype))

      def blockSize = Knowledge.experimental_cuda_reductionBlockSize

      var fctBody = ListBuffer[Statement]()

      // compile loop body
      def blocks = VariableAccess("blocks", Some(SpecialDatatype("size_t")))
      var loopBody = ListBuffer[Statement]()
      loopBody += new VariableDeclarationStatement(blocks, (numElements + (blockSize * stride - 1)) / (blockSize * stride))
      loopBody += new ConditionStatement(EqEqExpression(0, blocks), AssignmentStatement(blocks, 1))
      loopBody += new CUDA_FunctionCallExpression(kernelName, ListBuffer[Expression](data, numElements, stride),
        Array[Expression](blocks * blockSize /*FIXME: avoid x*BS/BS */ ), Array[Expression](blockSize))

      fctBody += ForLoopStatement(
        new VariableDeclarationStatement(stride, 1),
        LowerExpression(stride, numElements),
        AssignmentStatement(stride, 2, "*="),
        loopBody)

      fctBody += new VariableDeclarationStatement(ret)
      fctBody += new CUDA_Memcpy(AddressofExpression(ret), data, SizeOfExpression(RealDatatype), "cudaMemcpyDeviceToHost")

      fctBody += new ReturnStatement(Some(ret))

      // compile final wrapper function
      functions += FunctionStatement(
        RealDatatype, // TODO: support other types
        wrapperName,
        ListBuffer(data, VariableAccess("numElements", Some(IntegerDatatype /*FIXME: size_t*/ ))),
        fctBody,
        allowInlining = false, allowFortranInterface = false,
        "extern \"C\"")
    }
  }
}

object Kernel {
  def wrapperPostfix = "_wrapper"
}

case class Kernel(var identifier : String,
    var passThroughArgs : ListBuffer[VariableAccess],
    var numDimensions : Int,
    var indices : IndexRange,
    var body : ListBuffer[Statement],
    var reduction : Option[Reduction] = None,
    var condition : Option[Expression] = None) extends Node {

  import Kernel._

  var evaluatedAccesses = false
  var fieldAccesses = mutable.HashMap[String, LinearizedFieldAccess]()
  var ivAccesses = mutable.HashMap[String, iv.InternalVariable]()

  def getKernelFctName : String = identifier
  def getWrapperFctName : String = identifier + wrapperPostfix

  def evalFieldAccesses() = {
    if (!evaluatedAccesses) {
      GatherLocalLinearizedFieldAccess.fieldAccesses.clear
      GatherLocalLinearizedFieldAccess.applyStandalone(Scope(body))
      fieldAccesses = GatherLocalLinearizedFieldAccess.fieldAccesses

      GatherLocalIVs.ivAccesses.clear
      GatherLocalIVs.applyStandalone(Scope(body))
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

  def compileKernelBody : ListBuffer[Statement] = {
    evalFieldAccesses() // ensure that field accesses have been mapped

    var statements = ListBuffer[Statement]()

    // add index calculation
    val minIndices : Array[Long] = LoopOverDimensions.evalMinIndex(indices.begin, numDimensions, printWarnings = true)
    statements ++= (0 until numDimensions).map(dim => {
      val it = dimToString(dim)
      // FIXME: datatype for VariableAccess
      VariableDeclarationStatement(IntegerDatatype, it,
        Some(MemberAccess(VariableAccess("blockIdx", None), it) *
          MemberAccess(VariableAccess("blockDim", None), it) +
          MemberAccess(VariableAccess("threadIdx", None), it) +
          minIndices(dim)))
    })

    // add index bounds conditions
    statements ++= (0 until numDimensions).map(dim => {
      def it = Duplicate(LoopOverDimensions.defItForDim(dim))
      new ConditionStatement(
        OrOrExpression(LowerExpression(it, s"begin_$dim"), GreaterEqualExpression(it, s"end_$dim")),
        ReturnStatement())
    })

    // add other conditions if required
    if (condition.isDefined)
      statements += new ConditionStatement(NegationExpression(condition.get), ReturnStatement())

    // add actual body after replacing field and iv accesses
    ReplacingLocalLinearizedFieldAccess.fieldAccesses = fieldAccesses
    ReplacingLocalLinearizedFieldAccess.applyStandalone(Scope(body))
    ReplacingLocalIVs.ivAccesses = ivAccesses
    ReplacingLocalIVs.applyStandalone(Scope(body))

    statements ++= body

    statements
  }

  def compileWrapperFunction : FunctionStatement = {
    evalFieldAccesses() // ensure that field accesses have been mapped

    // compile arguments for device function call
    var callArgs = ListBuffer[Expression]()
    for (dim <- 0 until numDimensions) {
      callArgs += indices.begin(dim)
      callArgs += indices.end(dim)
    }
    for (fieldAccess <- fieldAccesses) {
      val fieldSelection = fieldAccess._2.fieldSelection
      callArgs += iv.FieldDeviceData(fieldSelection.field, fieldSelection.level, fieldSelection.slot)
    }
    for (ivAccess <- ivAccesses) {
      val access = Duplicate(ivAccess._2)
      // Hack for Vec3 -> TODO: split Vec3 iv's into separate real iv's
      access.resolveDataType match {
        case SpecialDatatype("Vec3") => callArgs += FunctionCallExpression("make_double3", (0 until 3).map(dim => ArrayAccess(ivAccess._2, dim) : Expression).to[ListBuffer])
        case _ => callArgs += ivAccess._2
      }
    }
    for (variableAccess <- passThroughArgs) {
      callArgs += Duplicate(variableAccess)
    }

    // evaluate required thread counts
    var numThreadsPerDim = (
      LoopOverDimensions.evalMaxIndex(indices.end, numDimensions, printWarnings = true),
      LoopOverDimensions.evalMinIndex(indices.begin, numDimensions, printWarnings = true)).zipped.map(_ - _)

    if (null == numThreadsPerDim || numThreadsPerDim.product <= 0) {
      Logger.warn("Could not evaluate required number of threads for kernel " + identifier)
      numThreadsPerDim = (0 until numDimensions).map(dim => 0 : Long).toArray // TODO: replace 0 with sth more suitable
    }

    var body = ListBuffer[Statement]()
    if (reduction.isDefined) {
      def bufSize = numThreadsPerDim.product
      def bufAccess = iv.ReductionDeviceData(bufSize)
      body += CUDA_Memset(bufAccess, 0, bufSize, reduction.get.target.dType.get)
      body += new CUDA_FunctionCallExpression(getKernelFctName, callArgs, numThreadsPerDim)
      body += ReturnStatement(Some(FunctionCallExpression(s"DefaultReductionKernel${BinaryOperators.opAsIdent(reduction.get.op)}_wrapper",
        ListBuffer[Expression](bufAccess, bufSize))))

      StateManager.findFirst[KernelFunctions]().get.requiredRedKernels += reduction.get.op // request reduction kernel and wrapper
    } else {
      body += new CUDA_FunctionCallExpression(getKernelFctName, callArgs, numThreadsPerDim)
    }

    FunctionStatement(
      if (reduction.isDefined) reduction.get.target.dType.get else UnitDatatype,
      getWrapperFctName,
      Duplicate(passThroughArgs),
      body,
      allowInlining = false, allowFortranInterface = false,
      "extern \"C\"")
  }

  def compileKernelFunction : FunctionStatement = {
    evalFieldAccesses() // ensure that field accesses have been mapped

    // compile parameters for device function
    var fctParams = ListBuffer[VariableAccess]()
    for (dim <- 0 until numDimensions) {
      fctParams += VariableAccess(s"begin_$dim", Some(IntegerDatatype))
      fctParams += VariableAccess(s"end_$dim", Some(IntegerDatatype))
    }
    for (fieldAccess <- fieldAccesses) {
      val fieldSelection = fieldAccess._2.fieldSelection
      fctParams += VariableAccess(fieldAccess._1, Some(PointerDatatype(fieldSelection.field.resolveDeclType)))
    }
    for (ivAccess <- ivAccesses) {
      var access = VariableAccess(ivAccess._1, Some(ivAccess._2.resolveDataType))
      access.dType match {
        case Some(SpecialDatatype("Vec3")) => access.dType = Some(SpecialDatatype("double3"))
        case _ =>
      }
      fctParams += access
    }
    for (variableAccess <- passThroughArgs) {
      fctParams += Duplicate(variableAccess)
    }

    val fct = FunctionStatement(
      UnitDatatype, getKernelFctName, fctParams,
      compileKernelBody,
      allowInlining = false, allowFortranInterface = false, "__global__")

    fct.annotate("deviceOnly")

    fct
  }
}

object ExpKernel {
  def wrapperPostfix = "_wrapper"
  val KernelVariablePrefix = "_cu_"
  val KernelGlobalIndexPrefix = "global_"
  val CUDASharedMemoryAccess = "CUDASharedMemoryAccess"
}

case class ExpKernel(var identifier : String,
    var dimensionality : Int,
    var passThroughArgs : ListBuffer[VariableAccess],
    var loopVariables : ListBuffer[String],
    var lowerBounds : ListBuffer[Expression],
    var upperBounds : ListBuffer[Expression],
    var stepSize : ListBuffer[Expression],
    var body : ListBuffer[Statement],
    var reduction : Option[Reduction] = None,
    var loopVariableExtrema : mutable.Map[String, (Long, Long)] = mutable.Map[String, (Long, Long)]()) extends Node {

  import ExpKernel._

  var originalDimensionality = dimensionality
  var firstNnotParallelDim = loopVariables.size - dimensionality

  // properties required for shared memory analysis and shared memory allocation
  var fieldsForSharedMemory = mutable.Map[String, FieldAccessLike]()
  var fieldToOffset = mutable.Map[String, MultiIndex]()
  var fieldAccessesForSharedMemory = mutable.Map[String, List[FieldAccessLike]]()
  var fieldToRadius = mutable.HashMap[String, MultiIndex]()
  var fieldToSharedArraySize = mutable.Map[String, Array[Long]]()
  var fieldToNumberOfPointsInStencil = mutable.Map[String, Array[Long]]()

  var evaluatedAccesses = false
  var linearizedFieldAccesses = mutable.HashMap[String, LinearizedFieldAccess]()
  var ivAccesses = mutable.HashMap[String, iv.InternalVariable]()

  var evaluatedIndexBounds = false
  var minIndices = Array[Long]()
  var maxIndices = Array[Long]()

  var evaluatedExecutionConfiguration = false
  var requiredThreadsPerDim = Array[Long]()
  var numThreadsPerBlock = Array[Long]()
  var numBlocksPerDim = Array[Long]()

  def getKernelFctName : String = identifier
  def getWrapperFctName : String = identifier + wrapperPostfix

  init()

  /**
   * Initializes some of the properties and starts the shared memory analysis if specified.
   */
  def init() : Unit = {
    evalIndexBounds()
    evalExecutionConfiguration()

    // call this function already in constructor to work on DirectFieldAccesses where indices are not yet linearized.
    // shared memory is currently just working for Smoother
    if (Knowledge.experimental_cuda_useSharedMemory && identifier.contains("Smoother")) {
      evaluateAccessesForSharedMemory()
    }
  }

  /**
   * Perform shared memory analysis and collect all FieldAccesses that are worth to store in shared memory.
   */
  def evaluateAccessesForSharedMemory() : Unit = {
    // 1. get all field accesses in the body and ask for amount of shared memory
    GatherLocalFieldAccessLikeForSharedMemory.loopVariables.clear
    GatherLocalFieldAccessLikeForSharedMemory.loopVariables = loopVariables.drop(firstNnotParallelDim)
    GatherLocalFieldAccessLikeForSharedMemory.fieldAccesses.clear
    GatherLocalFieldAccessLikeForSharedMemory.fieldIndicesConstantPart.clear
    GatherLocalFieldAccessLikeForSharedMemory.maximalFieldDim = math.min(dimensionality, Platform.hw_cuda_maxNumDimsBlock)
    GatherLocalFieldAccessLikeForSharedMemory.applyStandalone(new Scope(body))
    fieldAccessesForSharedMemory = GatherLocalFieldAccessLikeForSharedMemory.fieldAccesses
    val fieldIndicesConstantPart = GatherLocalFieldAccessLikeForSharedMemory.fieldIndicesConstantPart
    var availableSharedMemory = if (Knowledge.experimental_cuda_favorL1CacheOverSharedMemory) Platform.hw_cuda_cacheMemory.toLong else Platform.hw_cuda_sharedMemory.toLong

    // 2. Perform shared memory analysis
    // the more field accesses to a field the more important it is to store this field in shared memory
    fieldAccessesForSharedMemory = fieldAccessesForSharedMemory.filter(fa => fa._2.nonEmpty)
    fieldAccessesForSharedMemory = mutable.ListMap(fieldAccessesForSharedMemory.toSeq.sortWith(_._2.size > _._2.size) : _*)
    fieldAccessesForSharedMemory.foreach(fa => {

      if (Knowledge.experimental_cuda_applySpatialBlocking && dimensionality == Platform.hw_cuda_maxNumDimsBlock) {
        availableSharedMemory = analyzeFieldAccessSpatialBlocking(fa._1, fa._2, fieldIndicesConstantPart(fa._1), availableSharedMemory)
      } else {
        availableSharedMemory = analyzeFieldAccess(fa._1, fa._2, fieldIndicesConstantPart(fa._1), availableSharedMemory)
      }
    })

    // 3. update map to contain only the field accesses which passed the shared memory analysis in step 2
    fieldAccessesForSharedMemory.retain((k, v) => fieldsForSharedMemory.contains(k))
  }

  def analyzeFieldAccess(fieldName : String, fieldAccesses : List[FieldAccessLike],
      fieldIndicesConstantPart : List[Array[Long]], availableSharedMemory : Long) : Long = {
    var smemAmount = availableSharedMemory

    // 2.1 colllect some basic informations required for further calculations
    var requiredMemoryInByte = 0L
    val fieldOffset = fieldAccesses.head.fieldSelection.fieldLayout.referenceOffset
    val baseIndex = (loopVariables.take(fieldOffset.length), fieldOffset).zipped.map((x, y) => new AdditionExpression(new VariableAccess(x), y)).toArray[Expression]

    // 2.2 calculate negative and positive deviation from the basic field index
    val leftDeviationFromBaseIndex = fieldIndicesConstantPart.foldLeft(Array.fill(dimensionality)(0L))((acc, m) => (acc, m, fieldOffset).zipped.map((x, y, z) => {
      math.min(SimplifyExpression.evalIntegral(x), SimplifyExpression.evalIntegral(SubtractionExpression(y, z)))
    })).map(x => math.abs(x))

    val rightDeviationFromBaseIndex = fieldIndicesConstantPart.foldLeft(Array.fill(dimensionality)(0L))((acc, m) => (acc, m, fieldOffset).zipped.map((x, y, z) => {
      math.max(SimplifyExpression.evalIntegral(x), SimplifyExpression.evalIntegral(SubtractionExpression(y, z)))
    }))

    // 2.3 calculate the required amount of shared memory
    val numDupLayersLeft = fieldAccesses.head.fieldSelection.fieldLayout.layoutsPerDim.map(x => x.numDupLayersLeft)
    val numDupLayersRight = fieldAccesses.head.fieldSelection.fieldLayout.layoutsPerDim.map(x => x.numDupLayersRight)
    val numInnerLayers = fieldAccesses.head.fieldSelection.fieldLayout.layoutsPerDim.map(x => x.numInnerLayers)
    val numPointsInStencil = (numDupLayersLeft, numInnerLayers, numDupLayersRight).zipped.map((x, y, z) => x + y + z)
    val numPointsInStencilPerThreadBlock = (numPointsInStencil, numThreadsPerBlock).zipped.map((x, y) => math.min(x, y))
    val sharedArraySize = (numPointsInStencilPerThreadBlock, leftDeviationFromBaseIndex.map(_ * 2)).zipped.map(_ + _)
    requiredMemoryInByte = sharedArraySize.product * (fieldAccesses.head.fieldSelection.fieldLayout.datatype match {
      case RealDatatype => if (Knowledge.useDblPrecision) 8 else 4
      case DoubleDatatype => 8
      case FloatDatatype => 4
      case _ => -1
    })

    // 2.4 consider this field for shared memory if all conditions are met
    if (fieldAccesses.size > 1 && leftDeviationFromBaseIndex.deep == rightDeviationFromBaseIndex.deep && requiredMemoryInByte < availableSharedMemory) {
      smemAmount -= requiredMemoryInByte
      val access = new DirectFieldAccess(fieldAccesses.head.fieldSelection, new MultiIndex(baseIndex))
      access.annotate(LinearizeFieldAccesses.NO_LINEARIZATION)
      fieldsForSharedMemory.put(fieldName, access)
      fieldToOffset.put(fieldName, Duplicate(fieldOffset))
      fieldToRadius(fieldName) = MultiIndex(leftDeviationFromBaseIndex.map(x => IntegerConstant(x)))
      fieldToSharedArraySize(fieldName) = sharedArraySize
      fieldToNumberOfPointsInStencil(fieldName) = numPointsInStencilPerThreadBlock
    } else {
      fieldAccesses.foreach(a => a.removeAnnotation(LinearizeFieldAccesses.NO_LINEARIZATION))
    }

    smemAmount
  }

  def analyzeFieldAccessSpatialBlocking(fieldName : String, fieldAccesses : List[FieldAccessLike],
      fieldIndicesConstantPart : List[Array[Long]], availableSharedMemory : Long) : Long = {
    var smemAmount = availableSharedMemory
    val fieldLayout = fieldAccesses.head.fieldSelection.fieldLayout

    if (fieldLayout.numDimsData == Platform.hw_cuda_maxNumDimsBlock) {
      // 2.1 colllect some basic informations required for further calculations
      val smemDimensionality = fieldLayout.numDimsData - 1
      var requiredMemoryInByte = 0L
      val fieldOffset = fieldLayout.referenceOffset
      val baseIndex = (loopVariables.take(fieldOffset.length), fieldOffset).zipped.map((x, y) => new AdditionExpression(new VariableAccess(x), y)).toArray[Expression]

      // 2.2 calculate negative and positive deviation from the basic field index
      val leftDeviationFromBaseIndex = fieldIndicesConstantPart.foldLeft(Array.fill(smemDimensionality)(0L))((acc, m) => (acc, m, fieldOffset).zipped.map((x, y, z) => {
        math.min(SimplifyExpression.evalIntegral(x), SimplifyExpression.evalIntegral(SubtractionExpression(y, z)))
      })).map(x => math.abs(x))

      val rightDeviationFromBaseIndex = fieldIndicesConstantPart.foldLeft(Array.fill(smemDimensionality)(0L))((acc, m) => (acc, m, fieldOffset).zipped.map((x, y, z) => {
        math.max(SimplifyExpression.evalIntegral(x), SimplifyExpression.evalIntegral(SubtractionExpression(y, z)))
      }))

      // 2.3 calculate the required amount of shared memory
      val numDupLayersLeft = fieldLayout.layoutsPerDim.map(x => x.numDupLayersLeft)
      val numDupLayersRight = fieldLayout.layoutsPerDim.map(x => x.numDupLayersRight)
      val numInnerLayers = fieldLayout.layoutsPerDim.map(x => x.numInnerLayers)
      val numPointsInStencil = (numDupLayersLeft, numInnerLayers, numDupLayersRight).zipped.map((x, y, z) => x + y + z)
      val numPointsInStencilPerThreadBlock = (numPointsInStencil.take(smemDimensionality), numThreadsPerBlock).zipped.map((x, y) => math.min(x, y))
      val sharedArraySize = (numPointsInStencilPerThreadBlock, leftDeviationFromBaseIndex.map(_ * 2)).zipped.map(_ + _)
      requiredMemoryInByte = sharedArraySize.product * (fieldLayout.datatype match {
        case RealDatatype => if (Knowledge.useDblPrecision) 8 else 4
        case DoubleDatatype => 8
        case FloatDatatype => 4
        case _ => -1
      })

      // 2.4 consider this field for shared memory if all conditions are met
      if (fieldAccesses.size > 1 && leftDeviationFromBaseIndex.deep == rightDeviationFromBaseIndex.deep && requiredMemoryInByte < availableSharedMemory) {
        smemAmount -= requiredMemoryInByte
        val access = new DirectFieldAccess(fieldAccesses.head.fieldSelection, new MultiIndex(baseIndex))
        access.annotate(LinearizeFieldAccesses.NO_LINEARIZATION)
        fieldsForSharedMemory.put(fieldName, access)
        fieldToOffset.put(fieldName, Duplicate(fieldOffset))
        fieldToRadius(fieldName) = MultiIndex(leftDeviationFromBaseIndex.map(x => IntegerConstant(x)))
        fieldToSharedArraySize(fieldName) = sharedArraySize
        fieldToNumberOfPointsInStencil(fieldName) = numPointsInStencilPerThreadBlock
      } else {
        fieldAccesses.foreach(a => a.removeAnnotation(LinearizeFieldAccesses.NO_LINEARIZATION))
      }
    } else {
      fieldAccesses.foreach(a => a.removeAnnotation(LinearizeFieldAccesses.NO_LINEARIZATION))
    }

    smemAmount
  }

  /**
   * Check the accesses in the loop to create valid function calls.
   */
  def evalAccesses() = {
    if (!evaluatedAccesses) {
      GatherLocalLinearizedFieldAccess.fieldAccesses.clear
      GatherLocalLinearizedFieldAccess.applyStandalone(new Scope(body))
      linearizedFieldAccesses = GatherLocalLinearizedFieldAccess.fieldAccesses

      GatherLocalIVs.ivAccesses.clear
      GatherLocalIVs.applyStandalone(new Scope(body))
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
          case _ =>
            Logger.warn(s"Start index for dimension $dim (${lowerBounds(dim)}) could not be evaluated")
            0
        }).toArray.reverse

      maxIndices = (loopVariables.size - 1 to 0 by -1).map(dim =>
        loopVariableExtrema.get(loopVariables(dim)) match {
          case Some((min : Long, max : Long)) => max
          case _ =>
            Logger.warn(s"Start index for dimension $dim (${upperBounds(dim)}) could not be evaluated")
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
      requiredThreadsPerDim = (maxIndices.drop(firstNnotParallelDim), minIndices.drop(firstNnotParallelDim)).zipped.map(_ - _)

      if (null == requiredThreadsPerDim || requiredThreadsPerDim.product <= 0) {
        Logger.warn("Could not evaluate required number of threads for kernel " + identifier)
        requiredThreadsPerDim = (0 until dimensionality).map(dim => 0 : Long).toArray // TODO: replace 0 with sth more suitable
      }

      // distribute threads along threads in blocks and blocks in grid
      // NVIDIA GeForce Titan Black has CUDA compute capability 3.5
      // maximum number of threads per block = 1024
      // number of threads per block should be integer multiple of warp size (32)
      var executionDimensionality = math.min(dimensionality, Platform.hw_cuda_maxNumDimsBlock)

      if (Knowledge.experimental_cuda_applySpatialBlocking && dimensionality == Platform.hw_cuda_maxNumDimsBlock) {
        executionDimensionality = dimensionality - 1
      }

      executionDimensionality match {
        case 1 => numThreadsPerBlock = Array[Long](512)
        case 2 => numThreadsPerBlock = Array[Long](16, 16)
        case _ => numThreadsPerBlock = Array[Long](8, 8, 8)
      }

      numBlocksPerDim = (0 until executionDimensionality).map(dim => {
        (requiredThreadsPerDim(dim) + numThreadsPerBlock(dim) - 1) / numThreadsPerBlock(dim)
      }).toArray

      evaluatedExecutionConfiguration = true
    }
  }

  def addSmemStmts(statements: ListBuffer[ Statement ], conditionAccess: VariableAccess): Unit = {
    val sharedMemoryStatements = ListBuffer[ Statement ]()

    // For every field allocate shared memory and add load operations
    fieldsForSharedMemory.foreach(a => {
      // 1. Annotate the loop variables appearing in the shared memory accesses to guarantee the right substitution later
      AnnotatingLoopVariablesForSharedMemoryAccess.loopVariables = loopVariables
      AnnotatingLoopVariablesForSharedMemoryAccess.accessName = a._1
      AnnotatingLoopVariablesForSharedMemoryAccess.applyStandalone(new Scope(fieldAccessesForSharedMemory(a._1).map(x => new ExpressionStatement(x))))

      val localPrefix = KernelVariablePrefix + a._1 + "_"

      // 2. Add local Thread ID calculation for indexing shared memory
      statements ++= (0 until dimensionality).map(dim => {
        val it = dimToString(dim)
        val variableName = localPrefix + it
        VariableDeclarationStatement(IntegerDatatype, variableName,
          Some(MemberAccess(VariableAccess("threadIdx", Some(SpecialDatatype("dim3"))), it) +
            fieldToRadius(a._1).indices.reverse(dim)))
      })

      val localThreadId = (0 until dimensionality).map(dim => {
        VariableAccess(localPrefix + dimToString(dim), Some(IntegerDatatype))
      }).toArray[ Expression ]
      val globalThreadId = (0 until dimensionality).map(dim => {
        VariableAccess(KernelVariablePrefix + KernelGlobalIndexPrefix + dimToString(dim), Some(IntegerDatatype))
      }).toArray[ Expression ]

      // 3. Add shared memory declarations
      val sharedArrayStrides = new MultiIndex(fieldToSharedArraySize(a._1))
      statements += new CUDA_SharedArray(KernelVariablePrefix + a._1, a._2.fieldSelection.fieldLayout.datatype, fieldToSharedArraySize(a._1).reverse)

      // 4. Load from global memory into shared memory
      sharedMemoryStatements += AssignmentStatement(new CUDA_SharedArrayAccess(KernelVariablePrefix + a._1, localThreadId.reverse, sharedArrayStrides), new DirectFieldAccess(a._2.fieldSelection, a._2.index).linearize)

      // 5. Add load operations as ConditionStatement to avoid index out of bounds exceptions in global memory
      // and sync threads afterwards conditionForSharedMemto guarantee that every thread has the same memory state
      sharedMemoryStatements ++= (0 until dimensionality).map(dim => {
        val it = dimToString(dim)

        // 5.1 Check if current thread resides on the left border in any dimension
        val condition = OrOrExpression(LowerExpression(MemberAccess(VariableAccess("threadIdx", Some(SpecialDatatype("dim3"))), it), fieldToRadius(a._1)(dim)), EqEqExpression(globalThreadId(dim), s"${KernelVariablePrefix}begin_$dim"))
        val conditionBody = ListBuffer[ Statement ]()

        // 5.2 Calculate the offset from the left to the right border of the actual field
        val localFieldOffsetName: String = "localFieldOffset"
        conditionBody += VariableDeclarationStatement(IntegerDatatype, localFieldOffsetName, Some(
          new CUDA_MinimumExpression(
            SubtractionExpression(MemberAccess(VariableAccess("blockDim", Some(SpecialDatatype("dim3"))), it),
              MemberAccess(VariableAccess("threadIdx", Some(SpecialDatatype("dim3"))), it)),
            SubtractionExpression(s"${KernelVariablePrefix}end_$dim", globalThreadId(dim)))))
        val localFieldOffset = VariableAccess(localFieldOffsetName, Some(IntegerDatatype))

        // 5.3 Calculate the indices for writing into the shared memory and loading from the global memory
        val localLeftIndex = Duplicate(localThreadId)
        val localRightIndex = Duplicate(localThreadId)
        localLeftIndex(dim) = SubtractionExpression(localLeftIndex(dim), fieldToRadius(a._1)(dim))
        localRightIndex(dim) = new AdditionExpression(localRightIndex(dim), localFieldOffset)
        val globalLeftIndex = new MultiIndex(Duplicate(globalThreadId)) + fieldToOffset(a._1)
        val globalRightIndex = new MultiIndex(Duplicate(globalThreadId)) + fieldToOffset(a._1)
        globalLeftIndex(dim) = SubtractionExpression(globalLeftIndex(dim), fieldToRadius(a._1)(dim))
        globalRightIndex(dim) = new AdditionExpression(globalRightIndex(dim), localFieldOffset)

        // 5.4 Thread residing on left border should load left neighbor and the right neighbor of the point residing
        // on the right border of the actual field
        conditionBody += AssignmentStatement(new CUDA_SharedArrayAccess(KernelVariablePrefix + a._1, localLeftIndex.reverse, sharedArrayStrides), new DirectFieldAccess(a._2.fieldSelection, globalLeftIndex).linearize)
        conditionBody += AssignmentStatement(new CUDA_SharedArrayAccess(KernelVariablePrefix + a._1, localRightIndex.reverse, sharedArrayStrides), new DirectFieldAccess(a._2.fieldSelection, globalRightIndex).linearize)
        new ConditionStatement(condition, conditionBody)
      })

      // 6. Add whole shared memory initialization wrapped in a ConditionStatement to the body
      statements += new ConditionStatement(conditionAccess, sharedMemoryStatements)
    })

    // This may not be part of the ConditionStatement to avoid dead locks if some thread do not fulfill the condition
    statements += new CUDA_SyncThreads()
  }

  def addSmemStmtsSpatialBlocking(statements: ListBuffer[ Statement ], conditionAccess: VariableAccess): Unit = {
    val sharedMemoryStatements = ListBuffer[ Statement ]()

    // For every field allocate shared memory and add load operations
    fieldsForSharedMemory.foreach(a => {
      // 1. Annotate the loop variables appearing in the shared memory accesses to guarantee the right substitution later
      AnnotatingLoopVariablesForSharedMemoryAccess.loopVariables = loopVariables
      AnnotatingLoopVariablesForSharedMemoryAccess.accessName = a._1
      AnnotatingLoopVariablesForSharedMemoryAccess.applyStandalone(new Scope(fieldAccessesForSharedMemory(a._1).map(x => new ExpressionStatement(x))))
      val smemDim = dimensionality - 1

      val localPrefix = KernelVariablePrefix + a._1 + "_"

      // 2. Add local Thread ID calculation for indexing shared memory
      statements ++= (0 until smemDim).map(dim => {
        val it = dimToString(dim)
        val variableName = localPrefix + it
        VariableDeclarationStatement(IntegerDatatype, variableName,
          Some(MemberAccess(VariableAccess("threadIdx", Some(SpecialDatatype("dim3"))), it) +
            fieldToRadius(a._1).indices.reverse(dim)))
      })

      val localThreadId = (0 until smemDim).map(dim => {
        VariableAccess(localPrefix + dimToString(dim), Some(IntegerDatatype))
      }).toArray[ Expression ]
      val globalThreadId = (0 until dimensionality-1).map(dim => {
        VariableAccess(KernelVariablePrefix + KernelGlobalIndexPrefix + dimToString(dim), Some(IntegerDatatype))
      }).toArray[ Expression ]

      // 3. Add shared memory declarations
      val sharedArrayStrides = new MultiIndex(fieldToSharedArraySize(a._1))
      statements += new CUDA_SharedArray(KernelVariablePrefix + a._1, a._2.fieldSelection.fieldLayout.datatype, fieldToSharedArraySize(a._1).reverse)

      // 4. Load from global memory into shared memory
      sharedMemoryStatements += AssignmentStatement(new CUDA_SharedArrayAccess(KernelVariablePrefix + a._1, localThreadId.reverse, sharedArrayStrides), new DirectFieldAccess(a._2.fieldSelection, a._2.index).linearize)

      // 5. Add load operations as ConditionStatement to avoid index out of bounds exceptions in global memory
      // and sync threads afterwards conditionForSharedMemto guarantee that every thread has the same memory state
      sharedMemoryStatements ++= (0 until smemDim).map(dim => {
        val it = dimToString(dim)

        // 5.1 Check if current thread resides on the left border in any dimension
        val condition = OrOrExpression(LowerExpression(MemberAccess(VariableAccess("threadIdx", Some(SpecialDatatype("dim3"))), it), fieldToRadius(a._1)(dim)), EqEqExpression(globalThreadId(dim), s"${KernelVariablePrefix}begin_$dim"))
        val conditionBody = ListBuffer[ Statement ]()

        // 5.2 Calculate the offset from the left to the right border of the actual field
        val localFieldOffsetName: String = "localFieldOffset"
        conditionBody += VariableDeclarationStatement(IntegerDatatype, localFieldOffsetName, Some(
          new CUDA_MinimumExpression(
            SubtractionExpression(MemberAccess(VariableAccess("blockDim", Some(SpecialDatatype("dim3"))), it),
              MemberAccess(VariableAccess("threadIdx", Some(SpecialDatatype("dim3"))), it)),
            SubtractionExpression(s"${KernelVariablePrefix}end_$dim", globalThreadId(dim)))))
        val localFieldOffset = VariableAccess(localFieldOffsetName, Some(IntegerDatatype))

        // 5.3 Calculate the indices for writing into the shared memory and loading from the global memory
        val localLeftIndex = Duplicate(localThreadId)
        val localRightIndex = Duplicate(localThreadId)
        localLeftIndex(dim) = SubtractionExpression(localLeftIndex(dim), fieldToRadius(a._1)(dim))
        localRightIndex(dim) = new AdditionExpression(localRightIndex(dim), localFieldOffset)
        val globalLeftIndex = new MultiIndex(Duplicate(globalThreadId)) + fieldToOffset(a._1)
        val globalRightIndex = new MultiIndex(Duplicate(globalThreadId)) + fieldToOffset(a._1)
        globalLeftIndex(dim) = SubtractionExpression(globalLeftIndex(dim), fieldToRadius(a._1)(dim))
        globalRightIndex(dim) = new AdditionExpression(globalRightIndex(dim), localFieldOffset)

        // 5.4 Thread residing on left border should load left neighbor and the right neighbor of the point residing
        // on the right border of the actual field
        conditionBody += AssignmentStatement(new CUDA_SharedArrayAccess(KernelVariablePrefix + a._1, localLeftIndex.reverse, sharedArrayStrides), new DirectFieldAccess(a._2.fieldSelection, globalLeftIndex).linearize)
        conditionBody += AssignmentStatement(new CUDA_SharedArrayAccess(KernelVariablePrefix + a._1, localRightIndex.reverse, sharedArrayStrides), new DirectFieldAccess(a._2.fieldSelection, globalRightIndex).linearize)
        new ConditionStatement(condition, conditionBody)
      })

      // 6. Add whole shared memory initialization wrapped in a ConditionStatement to the body
      statements += new ConditionStatement(conditionAccess, sharedMemoryStatements)
    })

    // This may not be part of the ConditionStatement to avoid dead locks if some thread do not fulfill the condition
    statements += new CUDA_SyncThreads()
  }

  def compileKernelBody : ListBuffer[Statement] = {
    evalIndexBounds() // ensure that minimal and maximal indices are set correctly
    evalAccesses() // ensure that field accesses have been mapped
    evalExecutionConfiguration() // ensure that execution configuration is already calculated

    var statements = ListBuffer[Statement]()

    // add CUDA global Thread ID (x,y,z) calculation for a dim3 execution configuration
    // global thread id x = blockIdx.x *blockDim.x + threadIdx.x + offset1;
    // global thread id y = blockIdx.y *blockDim.y + threadIdx.y + offset2;
    // global thread id z = blockIdx.z *blockDim.z + threadIdx.z + offset3;
    statements ++= (0 until dimensionality).map(dim => {
      val it = dimToString(dim)
      val variableName = KernelVariablePrefix + KernelGlobalIndexPrefix + it
      VariableDeclarationStatement(IntegerDatatype, variableName,
        Some(MemberAccess(VariableAccess("blockIdx", Some(SpecialDatatype("dim3"))), it) *
          MemberAccess(VariableAccess("blockDim", Some(SpecialDatatype("dim3"))), it) +
          MemberAccess(VariableAccess("threadIdx", Some(SpecialDatatype("dim3"))), it) +
          minIndices(dim)))
    })

    // add dimension index start and end point
    // add index bounds conditions
    val conditionParts = (0 until dimensionality).map(dim => {
      val variableAccess = VariableAccess(KernelVariablePrefix + KernelGlobalIndexPrefix + dimToString(dim), Some(IntegerDatatype))
      AndAndExpression(
        GreaterEqualExpression(variableAccess, s"${KernelVariablePrefix}begin_$dim"), LowerExpression(variableAccess, s"${KernelVariablePrefix}end_$dim"))
    })

    val condition = VariableDeclarationStatement(BooleanDatatype, KernelVariablePrefix + "condition",
      Some(conditionParts.reduceLeft[AndAndExpression] { (acc, n) =>
        AndAndExpression(acc, n)
      }))
    val conditionAccess = VariableAccess(KernelVariablePrefix + "condition", Some(BooleanDatatype))
    statements += condition

    if (Knowledge.experimental_cuda_useSharedMemory && identifier.contains("Smoother")) {
      if (Knowledge.experimental_cuda_applySpatialBlocking && dimensionality == Platform.hw_cuda_maxNumDimsBlock) {
        addSmemStmtsSpatialBlocking(statements, conditionAccess)
      } else {
        addSmemStmts(statements, conditionAccess)
      }
    }

    statements += new ConditionStatement(conditionAccess, body)
    body = statements

    // add actual body after replacing field and iv accesses
    // replace fieldaccesses in body with shared memory accesses

    if (Knowledge.experimental_cuda_useSharedMemory) {
      ReplacingLocalFieldAccessLikeForSharedMemory.fieldToOffset.clear()
      ReplacingLocalFieldAccessLikeForSharedMemory.fieldToOffset = fieldToOffset
      ReplacingLocalFieldAccessLikeForSharedMemory.offsetForSharedMemoryAccess.clear()
      ReplacingLocalFieldAccessLikeForSharedMemory.offsetForSharedMemoryAccess = fieldToRadius
      ReplacingLocalFieldAccessLikeForSharedMemory.sharedArrayStrides = fieldToSharedArraySize
      ReplacingLocalFieldAccessLikeForSharedMemory.applyStandalone(new Scope(body))
    }

    ReplacingLocalLinearizedFieldAccess.applyStandalone(new Scope(body))
    ReplacingLocalIVs.ivAccesses = ivAccesses
    ReplacingLocalIVs.applyStandalone(new Scope(body))
    ReplacingLocalIVArrays.applyStandalone(new Scope(body))
    ReplacingLoopVariables.loopVariables = loopVariables.drop(firstNnotParallelDim)
    ReplacingLoopVariables.applyStandalone(new Scope(body))

    body
  }

  def compileWrapperFunction : FunctionStatement = {
    evalIndexBounds() // ensure that minimal and maximal indices are set correctly
    evalAccesses() // ensure that field accesses have been mapped
    evalExecutionConfiguration() // ensure that execution configuration is already calculated

    // substitute loop variable in bounds with appropriate fix values to get valid code in wrapper function
    ReplacingLoopVariablesInWrapper.loopVariables.clear
    ReplacingLoopVariablesInWrapper.loopVariables = loopVariables.drop(firstNnotParallelDim)
    ReplacingLoopVariablesInWrapper.bounds = Duplicate(lowerBounds.drop(firstNnotParallelDim))
    val lowerArgs = Duplicate(lowerBounds.drop(firstNnotParallelDim))
    ReplacingLoopVariablesInWrapper.applyStandalone(lowerArgs)

    val upperArgs = Duplicate(upperBounds.drop(firstNnotParallelDim))
    ReplacingLoopVariablesInWrapper.bounds = Duplicate(upperBounds.drop(firstNnotParallelDim))
    ReplacingLoopVariablesInWrapper.applyStandalone(upperArgs)

    // compile arguments for device function call
    var callArgs = ListBuffer[Expression]()

    for (dim <- 0 until dimensionality) {
      callArgs += lowerArgs(dim)
      callArgs += upperArgs(dim)
    }

    for (fieldAccess <- linearizedFieldAccesses) {
      val fieldSelection = fieldAccess._2.fieldSelection
      callArgs += iv.FieldDeviceData(fieldSelection.field, fieldSelection.level, fieldSelection.slot)
    }

    if (Knowledge.experimental_cuda_useSharedMemory) {
      for (fieldAccess <- fieldsForSharedMemory) {
        val fieldSelection = fieldAccess._2.fieldSelection
        callArgs += iv.FieldDeviceData(fieldSelection.field, fieldSelection.level, fieldSelection.slot)
      }
    }

    for (ivAccess <- ivAccesses) {
      val access = Duplicate(ivAccess._2)
      // Hack for Vec3 -> TODO: split Vec3 iv's into separate real iv's
      access.resolveDataType match {
        case SpecialDatatype("Vec3") => callArgs += FunctionCallExpression("make_double3", (0 until 3).map(dim => ArrayAccess(ivAccess._2, dim) : Expression).to[ListBuffer])
        case SpecialDatatype("Vec3i") => callArgs ++= (0 until 3).map(dim => ArrayAccess(ivAccess._2, dim) : Expression).to[ListBuffer]
        case _ => callArgs += ivAccess._2
      }
    }

    for (variableAccess <- passThroughArgs) {
      callArgs += Duplicate(variableAccess)
    }

    var body = ListBuffer[Statement]()

    if (reduction.isDefined) {
      def bufSize = requiredThreadsPerDim.product
      def bufAccess = iv.ReductionDeviceData(bufSize)
      body += CUDA_Memset(bufAccess, 0, bufSize, reduction.get.target.dType.get)
      body += new CUDA_FunctionCallExperimentalExpression(getKernelFctName, callArgs, numThreadsPerBlock, numBlocksPerDim)
      body += ReturnStatement(Some(FunctionCallExpression(s"DefaultReductionKernel${BinaryOperators.opAsIdent(reduction.get.op)}_wrapper",
        ListBuffer[Expression](bufAccess, bufSize))))

      StateManager.findFirst[KernelFunctions]().get.requiredRedKernels += reduction.get.op // request reduction kernel and wrapper
    } else {
      body += new CUDA_FunctionCallExperimentalExpression(getKernelFctName, callArgs, numThreadsPerBlock, numBlocksPerDim)
    }

    FunctionStatement(
      if (reduction.isDefined) reduction.get.target.dType.get else UnitDatatype,
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
    var fctParams = ListBuffer[VariableAccess]()

    for (dim <- 0 until dimensionality) {
      fctParams += VariableAccess(s"${KernelVariablePrefix}begin_$dim", Some(IntegerDatatype))
      fctParams += VariableAccess(s"${KernelVariablePrefix}end_$dim", Some(IntegerDatatype))
    }

    for (fieldAccess <- linearizedFieldAccesses) {
      val fieldSelection = fieldAccess._2.fieldSelection
      fctParams += VariableAccess(fieldAccess._1, Some(PointerDatatype(fieldSelection.field.resolveDeclType)))
    }

    for (fieldAccess <- fieldsForSharedMemory) {
      val fieldSelection = fieldAccess._2.fieldSelection
      fctParams += VariableAccess(fieldAccess._1, Some(PointerDatatype(fieldSelection.field.resolveDeclType)))
    }

    for (ivAccess <- ivAccesses) {
      var access = VariableAccess(ivAccess._1, Some(ivAccess._2.resolveDataType))
      val datatype = ivAccess._2.resolveDataType

      datatype match {
        case SpecialDatatype("Vec3") =>
          access.dType = Some(SpecialDatatype("double3"))
          fctParams += access
        case SpecialDatatype("Vec3i") =>
          fctParams ++= (0 until 3).map(dim => VariableAccess(ivAccess._1 + '_' + dim, Some(SpecialDatatype("int")))).to[ListBuffer]
        case _ => fctParams += VariableAccess(ivAccess._1, Some(datatype))
      }
    }

    for (variableAccess <- passThroughArgs) {
      fctParams += Duplicate(variableAccess)
    }

    val fct = FunctionStatement(
      UnitDatatype, getKernelFctName, fctParams,
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
        case SlotAccess(_, offset) => identifier += s"_o$offset"
        case IntegerConstant(slot) => identifier += s"_s$slot"
        case _ => identifier += s"_s${access.fieldSelection.slot.prettyprint}"
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
        case SlotAccess(_, offset) => identifier += s"_o$offset"
        case IntegerConstant(slot) => identifier += s"_s$slot"
        case _ => identifier += s"_s${access.fieldSelection.slot.prettyprint}"
      }
    }

    VariableAccess(identifier, Some(PointerDatatype(field.resolveDeclType)))
  }

  this += new Transformation("Searching", {
    case access : LinearizedFieldAccess =>
      val identifier = extractIdentifier(access)
      ArrayAccess(identifier, access.index)
  })
}

object GatherLocalFieldAccessLikeForSharedMemory extends QuietDefaultStrategy("Gathering local FieldAccessLike nodes for shared memory") {
  var loopVariables = ListBuffer[String]()
  var fieldAccesses = new mutable.HashMap[String, List[FieldAccessLike]].withDefaultValue(Nil)
  var fieldIndicesConstantPart = new mutable.HashMap[String, List[Array[Long]]].withDefaultValue(Nil)
  var maximalFieldDim = Platform.hw_cuda_maxNumDimsBlock

  def mapFieldAccesses(access : FieldAccessLike) = {
    val field = access.fieldSelection.field
    var identifier = field.codeName

    if (field.numSlots > 1) {
      access.fieldSelection.slot match {
        case SlotAccess(_, offset) => identifier += s"_o$offset"
        case IntegerConstant(slot) => identifier += s"_s$slot"
        case _ => identifier += s"_s${access.fieldSelection.slot.prettyprint}"
      }
    }

    // Evaluate indices. Should be of the form "variable + offset". Ignore all other fields.
    var suitableForSharedMemory = field.fieldLayout.numDimsData <= Platform.hw_cuda_maxNumDimsBlock
    val accessIndices = access.index.indices
    val indexConstantPart = Array.fill[Long](accessIndices.length)(0)

    accessIndices.indices.foreach(i => {
      accessIndices(i) match {
        case AdditionExpression(ListBuffer(va @ VariableAccess(name : String, _), IntegerConstant(v : Long))) =>
          suitableForSharedMemory &= loopVariables.contains(name)
          indexConstantPart(i) = v
        case va @ VariableAccess(name : String, _) =>
          suitableForSharedMemory &= loopVariables.contains(name)
        case IntegerConstant(v : Long) =>
          indexConstantPart(i) = v
        case _ =>
          suitableForSharedMemory = false
      }
    })

    if (suitableForSharedMemory) {
      access.annotate(LinearizeFieldAccesses.NO_LINEARIZATION)
      fieldAccesses(identifier) ::= access
      fieldIndicesConstantPart(identifier) ::= indexConstantPart
    }
  }

  this += new Transformation("Searching", {
    case access : FieldAccessLike if access.fieldSelection.fieldLayout.numDimsData <= maximalFieldDim =>
      mapFieldAccesses(access)
      access
  }, true)
}

object ReplacingLocalFieldAccessLikeForSharedMemory extends QuietDefaultStrategy("Replacing local FieldAccessLike nodes for shared memory") {
  var fieldToOffset = mutable.Map[String, MultiIndex]()
  var offsetForSharedMemoryAccess = mutable.Map[String, MultiIndex]()
  var sharedArrayStrides = mutable.Map[String, Array[Long]]()

  def extractIdentifier(access : FieldAccessLike) = {
    val field = access.fieldSelection.field
    var identifier = field.codeName

    // TODO: array fields
    if (field.numSlots > 1) {
      access.fieldSelection.slot match {
        case SlotAccess(_, offset) => identifier += s"_o$offset"
        case IntegerConstant(slot) => identifier += s"_s$slot"
        case _ => identifier += s"_s${access.fieldSelection.slot.prettyprint}"
      }
    }

    identifier
  }

  this += new Transformation("Searching", {
    case access : FieldAccessLike =>
      val identifier = extractIdentifier(access)
      new CUDA_SharedArrayAccess(VariableAccess(ExpKernel.KernelVariablePrefix + identifier, Some(PointerDatatype(access.fieldSelection.field.resolveDeclType))), (access.index - fieldToOffset(identifier)).indices.reverse, new MultiIndex(sharedArrayStrides(identifier)))
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
      VariableAccess(ivAccess._1, Some(ivAccess._2.resolveDataType))
  })
}

object ReplacingLocalIVArrays extends QuietDefaultStrategy("Replacing local ArrayAccess nodes with special vector datatype") {
  def checkAccess(ivArray : ArrayAccess) : Boolean = {
    var result = false

    (ivArray.base, ivArray.index) match {
      case (ivAccess : VariableAccess, i : IntegerConstant) =>
        result = ivAccess.dType.contains(SpecialDatatype("Vec3i"))
      case _ =>
    }

    result
  }

  this += new Transformation("Searching", {
    case ivArray : ArrayAccess if checkAccess(ivArray) =>
      val iv = ivArray.base.asInstanceOf[VariableAccess]
      val i = ivArray.index.asInstanceOf[IntegerConstant]
      VariableAccess(iv.name + '_' + i.v, Some(SpecialDatatype("double")))
  })
}

object ReplacingLoopVariables extends QuietDefaultStrategy("Replacing loop variables with generated kernel variables") {
  var loopVariables = ListBuffer[String]()

  this += new Transformation("Searching", {
    case v @ VariableAccess(name @ n, maybeDatatype @ d) if loopVariables.contains(name) =>
      var newName = ExpKernel.KernelVariablePrefix + ExpKernel.KernelGlobalIndexPrefix + dimToString(loopVariables.indexOf(name))

      if (v.hasAnnotation(ExpKernel.CUDASharedMemoryAccess)) {
        newName = ExpKernel.KernelVariablePrefix + v.getAnnotation(ExpKernel.CUDASharedMemoryAccess).get + "_" + dimToString(loopVariables.indexOf(name))
      }

      VariableAccess(newName, Some(IntegerDatatype))
    case s @ StringLiteral(v @ value) if loopVariables.contains(v) =>
      var newName = ExpKernel.KernelVariablePrefix + ExpKernel.KernelGlobalIndexPrefix + dimToString(loopVariables.indexOf(v))

      if (s.hasAnnotation(ExpKernel.CUDASharedMemoryAccess)) {
        newName = ExpKernel.KernelVariablePrefix + s.getAnnotation(ExpKernel.CUDASharedMemoryAccess).get + "_" + dimToString(loopVariables.indexOf(v))
      }

      VariableAccess(newName, Some(IntegerDatatype))
  })
}

object AnnotatingLoopVariablesForSharedMemoryAccess extends QuietDefaultStrategy("Annotate loop variables for shared memory access") {
  var loopVariables = ListBuffer[String]()
  var accessName = ""

  this += new Transformation("Searching", {
    case v @ VariableAccess(name @ n, maybeDatatype @ d) if loopVariables.contains(name) =>
      v.annotate(ExpKernel.CUDASharedMemoryAccess, accessName)
      v
    case s @ StringLiteral(v @ value) if loopVariables.contains(v) =>
      s.annotate(ExpKernel.CUDASharedMemoryAccess, accessName)
      s
  })
}

object ReplacingLoopVariablesInWrapper extends QuietDefaultStrategy("Replacing loop variables in wrapper with provided bounds expressions") {
  var loopVariables = ListBuffer[String]()
  var bounds = ListBuffer[Expression]()

  this += new Transformation("Searching", {
    case StringLiteral(v @ value) if loopVariables.contains(v) =>
      bounds(loopVariables.indexOf(v))

    case VariableAccess(n, Some(IntegerDatatype)) if loopVariables.contains(n) =>
      bounds(loopVariables.indexOf(n))
  })
}
