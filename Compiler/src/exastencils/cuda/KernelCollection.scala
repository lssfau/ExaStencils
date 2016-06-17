package exastencils.cuda

import exastencils.core._
import exastencils.data._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.prettyprinting._

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
  val CUDASharedMemoryAccess = "CUDASharedMemoryAccess"
}

case class ExpKernel(var identifier : String,
    var passThroughArgs : ListBuffer[VariableAccess],
    var loopVariables : ListBuffer[String],
    var lowerBounds : ListBuffer[Expression],
    var upperBounds : ListBuffer[Expression],
    var stepSize : ListBuffer[Expression],
    var body : ListBuffer[Statement],
    var reduction : Option[Reduction] = None,
    var loopVariableExtrema : mutable.Map[String, (Long, Long)] = mutable.Map[String, (Long, Long)]()) extends Node {

  import ExpKernel._

  var originalDimensionality = loopVariables.size
  var dimensionality = loopVariables.size

  var evaluatedAccesses = false
  var fieldAccesses = mutable.Map[String, List[FieldAccessLike]]()
  var fieldToOffset = mutable.Map[String, MultiIndex]()
  var offsetForSharedMemoryAccess = mutable.Map[String, MultiIndex]()
  var accessesForSharedMemory = mutable.Map[String, FieldAccessLike]()

  var linearizedFieldAccesses = mutable.HashMap[String, LinearizedFieldAccess]()
  var ivAccesses = mutable.HashMap[String, iv.InternalVariable]()

  var evaluatedIndexBounds = false
  var minIndices = Array[Long]()
  var maxIndices = Array[Long]()
  var requiredThreadsPerDim = Array[Long]()
  var numThreadsPerBlock = Array[Long]()
  var numBlocksPerDim = Array[Long]()
  var evaluatedExecutionConfiguration = false

  def getKernelFctName : String = identifier
  def getWrapperFctName : String = identifier + wrapperPostfix

  init()

  def init() : Unit = {
    // 1. get read and write field accesses in body
    GatherLocalFieldAccessLike.fieldAccesses.clear
    GatherLocalFieldAccessLike.applyStandalone(new Scope(body))
    fieldAccesses = GatherLocalFieldAccessLike.fieldAccesses

    // 2. evaluate accesses worth to load in shared memory
    // every field accessed more than once is potentially worth to load in shared memory
    fieldAccesses.foreach(fa => {
      if (fa._2.size > 1) {
        accessesForSharedMemory.put(fa._1, fa._2.head)
      } else {
        fa._2.foreach(a => a.removeAnnotation(LinearizeFieldAccesses.NO_LINEARIZATION))
      }
    })
    fieldAccesses = fieldAccesses.filter(fa => fa._2.size > 1)

    // 3. get offset from every field
    accessesForSharedMemory.foreach(a => {
      val offset = Array.fill[Long](a._2.fieldSelection.fieldLayout.referenceOffset.indices.length)(1)

      fieldToOffset.put(a._1, Duplicate(a._2.fieldSelection.fieldLayout.referenceOffset))
      offsetForSharedMemoryAccess.put(a._1, new MultiIndex(offset))
    })
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
      minIndices = (originalDimensionality - 1 to 0 by -1).map(dim =>
        loopVariableExtrema.get(loopVariables(dim)) match {
          case Some((min : Long, max : Long)) => min
          case _ =>
            Logger.warn(s"Start index for dimension $dim (${lowerBounds(dim)}) could not be evaluated")
            0
        }).toArray.reverse.take(dimensionality)

      maxIndices = (originalDimensionality - 1 to 0 by -1).map(dim =>
        loopVariableExtrema.get(loopVariables(dim)) match {
          case Some((min : Long, max : Long)) => max
          case _ =>
            Logger.warn(s"Start index for dimension $dim (${upperBounds(dim)}) could not be evaluated")
            0
        }).toArray.reverse.take(dimensionality)

      evaluatedIndexBounds = true
    }
  }

  def evalExecutionConfiguration() = {
    if (!evaluatedExecutionConfiguration) {
      requiredThreadsPerDim = (maxIndices, minIndices).zipped.map(_ - _)

      if (null == requiredThreadsPerDim || requiredThreadsPerDim.product <= 0) {
        Logger.warn("Could not evaluate required number of threads for kernel " + identifier)
        requiredThreadsPerDim = (0 until dimensionality).map(dim => 0 : Long).toArray // TODO: replace 0 with sth more suitable
      }

      // distribute threads along threads in blocks and blocks in grid
      // NVIDIA GeForce Titan Black has CUDA compute capability 3.5
      // maximum number of threads per block = 1024
      // number of threads per block should be integer multiple of warp size (32)
      dimensionality match {
        case 1 => numThreadsPerBlock = Array[Long](512)
        case 2 => numThreadsPerBlock = Array[Long](32, 16)
        case _ => numThreadsPerBlock = Array[Long](8, 8, 8)
      }

      numBlocksPerDim = (0 until dimensionality).map(dim => {
        (requiredThreadsPerDim(dim) + numThreadsPerBlock(dim) - 1) / numThreadsPerBlock(dim)
      }).toArray

      evaluatedExecutionConfiguration = true
    }
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
      val variableName = KernelVariablePrefix + it
      VariableDeclarationStatement(IntegerDatatype, variableName,
        Some(MemberAccess(VariableAccess("blockIdx", Some(SpecialDatatype("dim3"))), it) *
          MemberAccess(VariableAccess("blockDim", Some(SpecialDatatype("dim3"))), it) +
          MemberAccess(VariableAccess("threadIdx", Some(SpecialDatatype("dim3"))), it) +
          minIndices(dim)))
    })

    // add dimension index start and end point
    // add index bounds conditions
    val conditionParts = (0 until dimensionality).map(dim => {
      val variableAccess = VariableAccess(KernelVariablePrefix + dimToString(dim), Some(IntegerDatatype))
      AndAndExpression(GreaterEqualExpression(variableAccess, s"${KernelVariablePrefix}begin_$dim"), LowerExpression(variableAccess, s"${KernelVariablePrefix}end_$dim"))
    })

    val condition = VariableDeclarationStatement(BooleanDatatype, KernelVariablePrefix + "condition",
      Some(conditionParts.reduceLeft[AndAndExpression] { (acc, n) =>
        AndAndExpression(acc, n)
      }))
    val conditionAccess = VariableAccess(KernelVariablePrefix + "condition", Some(BooleanDatatype))
    statements += condition

    if (Knowledge.experimental_cuda_useSharedMemory) {
      accessesForSharedMemory.foreach(a => {
        statements += new CUDA_SharedArray(KernelVariablePrefix + a._1, DoubleDatatype, numThreadsPerBlock.map(x => x + 2))
      })

      val localIndices = ListBuffer[Expression]() ++ (0 until dimensionality).map(dim => MemberAccess(VariableAccess("threadIdx", Some(SpecialDatatype("dim3"))), dimToString(dim)))
      val globalIndices = ListBuffer[Expression]() ++ (0 until dimensionality).map(dim => VariableAccess(KernelVariablePrefix + dimToString(dim)))
      var sharedMemoryStatements = ListBuffer[Statement]()
      accessesForSharedMemory.foreach(a => {
        sharedMemoryStatements += AssignmentStatement(new CUDA_SharedArrayAccess(KernelVariablePrefix + a._1, localIndices.reverse), new DirectFieldAccess(a._2.fieldSelection, MultiIndex(globalIndices.toArray[Expression]) + fieldToOffset(a._1)).linearize)
      })

      statements += new ConditionStatement(conditionAccess, sharedMemoryStatements)
      statements += new CUDA_SyncThreads()

      AnnotatingLoopVariablesForSharedMemoryAccess.loopVariables = loopVariables
      AnnotatingLoopVariablesForSharedMemoryAccess.applyStandalone(new Scope(fieldAccesses.values.flatten.map(x => new ExpressionStatement(x)).toList))
    }

    statements += new ConditionStatement(conditionAccess, body)
    body = statements

    // add actual body after replacing field and iv accesses
    // replace fieldaccesses in body with shared memory accesses
    ReplacingLocalFieldAccessLike.fieldToOffset.clear()
    ReplacingLocalFieldAccessLike.fieldToOffset = fieldToOffset
    ReplacingLocalFieldAccessLike.offsetForSharedMemoryAccess.clear()
    ReplacingLocalFieldAccessLike.offsetForSharedMemoryAccess = offsetForSharedMemoryAccess
    ReplacingLocalFieldAccessLike.applyStandalone(new Scope(body))
    ReplacingLocalLinearizedFieldAccess.applyStandalone(new Scope(body))
    ReplacingLocalIVs.ivAccesses = ivAccesses
    ReplacingLocalIVs.applyStandalone(new Scope(body))
    ReplacingLocalIVArrays.applyStandalone(new Scope(body))
    ReplacingLoopVariables.loopVariables = loopVariables
    ReplacingLoopVariables.applyStandalone(new Scope(body))

    body
  }

  def compileWrapperFunction : FunctionStatement = {
    evalIndexBounds() // ensure that minimal and maximal indices are set correctly
    evalAccesses() // ensure that field accesses have been mapped
    evalExecutionConfiguration() // ensure that execution configuration is already calculated

    // substitute loop variable in bounds with appropriate fix values to get valid code in wrapper function
    ReplacingLoopVariablesInWrapper.loopVariables.clear
    ReplacingLoopVariablesInWrapper.loopVariables = loopVariables
    ReplacingLoopVariablesInWrapper.bounds = Duplicate(lowerBounds)
    val lowerArgs = Duplicate(lowerBounds)
    ReplacingLoopVariablesInWrapper.applyStandalone(lowerArgs)

    val upperArgs = Duplicate(upperBounds)
    ReplacingLoopVariablesInWrapper.bounds = Duplicate(upperBounds)
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

    for (fieldAccess <- accessesForSharedMemory) {
      val fieldSelection = fieldAccess._2.fieldSelection
      callArgs += iv.FieldDeviceData(fieldSelection.field, fieldSelection.level, fieldSelection.slot)
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

    for (fieldAccess <- accessesForSharedMemory) {
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
          fctParams ++= (0 until 3).map(dim => VariableAccess(ivAccess._1 + '_' + dim, Some(SpecialDatatype("double")))).to[ListBuffer]
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

object GatherLocalFieldAccessLike extends QuietDefaultStrategy("Gathering local FieldAccessLike nodes") {
  var fieldAccesses = new mutable.HashMap[String, List[FieldAccessLike]].withDefaultValue(Nil)

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

    access.annotate(LinearizeFieldAccesses.NO_LINEARIZATION)
    fieldAccesses(identifier) ::= access
  }

  this += new Transformation("Searching", {
    case access : FieldAccessLike =>
      mapFieldAccesses(access)
      access
  }, true)
}

object ReplacingLocalFieldAccessLike extends QuietDefaultStrategy("Replacing local FieldAccessLike nodes") {
  var fieldToOffset = mutable.Map[String, MultiIndex]()
  var offsetForSharedMemoryAccess = mutable.Map[String, MultiIndex]()

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
      new CUDA_SharedArrayAccess(VariableAccess(ExpKernel.KernelVariablePrefix + identifier, Some(PointerDatatype(access.fieldSelection.field.resolveDeclType))), (access.index - fieldToOffset(identifier) + offsetForSharedMemoryAccess(identifier)).indices.reverse)
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
      if (v.hasAnnotation(ExpKernel.CUDASharedMemoryAccess)) {
        MemberAccess(VariableAccess("threadIdx", Some(SpecialDatatype("dim3"))), dimToString(loopVariables.indexOf(name)))
      } else {
        val newName = ExpKernel.KernelVariablePrefix + dimToString(loopVariables.indexOf(name))
        VariableAccess(newName, Some(IntegerDatatype))
      }
    case s @ StringLiteral(v @ value) if loopVariables.contains(v) =>
      if (s.hasAnnotation(ExpKernel.CUDASharedMemoryAccess)) {
        MemberAccess(VariableAccess("threadIdx", Some(SpecialDatatype("dim3"))), dimToString(loopVariables.indexOf(v)))
      } else {
        val newName = ExpKernel.KernelVariablePrefix + dimToString(loopVariables.indexOf(v))
        VariableAccess(newName, Some(IntegerDatatype))
      }
  })
}

object AnnotatingLoopVariablesForSharedMemoryAccess extends QuietDefaultStrategy("Annotate loop variables for shared memory access") {
  var loopVariables = ListBuffer[String]()

  this += new Transformation("Searching", {
    case v @ VariableAccess(name @ n, maybeDatatype @ d) if loopVariables.contains(name) =>
      v.annotate(ExpKernel.CUDASharedMemoryAccess)
      v
    case s @ StringLiteral(v @ value) if loopVariables.contains(v) =>
      s.annotate(ExpKernel.CUDASharedMemoryAccess)
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
