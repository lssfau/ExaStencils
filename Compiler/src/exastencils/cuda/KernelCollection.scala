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

import scala.collection.mutable.{ HashMap, HashSet, ListBuffer }

case class KernelFunctions() extends FunctionCollection("KernelFunctions/KernelFunctions",
  ListBuffer("cmath", "algorithm"), // provide math functions like sin, etc. as well as commonly used functions like min/max by default
  ListBuffer("Globals/Globals.h", "MultiGrid/MultiGrid.h")) {

  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"
  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"
  if (Knowledge.experimental_cuda_enabled) {
    externalDependencies += "cuda.h"
    externalDependencies += "cuda_runtime.h"
  }

  var kernelCollection = ListBuffer[ExpKernel]()
  var requiredRedKernels = HashSet[String]()
  var counterMap = HashMap[String, Int]()

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

  def convertToFunctions = {
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
  var fieldAccesses = HashMap[String, LinearizedFieldAccess]()
  var ivAccesses = HashMap[String, iv.InternalVariable]()

  def getKernelFctName : String = identifier
  def getWrapperFctName : String = identifier + wrapperPostfix

  def evalFieldAccesses = {
    if (!evaluatedAccesses) {
      GatherLocalLinearizedFieldAccess.fieldAccesses.clear
      GatherLocalLinearizedFieldAccess.applyStandalone(Scope(body))
      fieldAccesses = GatherLocalLinearizedFieldAccess.fieldAccesses

      GatherLocalIVs.ivAccesses.clear
      GatherLocalIVs.applyStandalone(Scope(body))
      ivAccesses = GatherLocalIVs.ivAccesses

      // postprocess iv's -> generate parameter names
      var cnt = 0
      val processedIVs = HashMap[String, iv.InternalVariable]()
      for (ivAccess <- ivAccesses) {
        processedIVs.put(ivAccess._2.resolveName + "_" + cnt, ivAccess._2)
        cnt += 1
      }
      ivAccesses = processedIVs

      evaluatedAccesses = true
    }
  }

  def compileKernelBody : ListBuffer[Statement] = {
    evalFieldAccesses // ensure that field accesses have been mapped

    var statements = ListBuffer[Statement]()

    // add index calculation
    val minIndices = LoopOverDimensions.evalMinIndex(indices.begin, numDimensions, printWarnings = true)
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
    evalFieldAccesses // ensure that field accesses have been mapped

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
    evalFieldAccesses // ensure that field accesses have been mapped

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
}

case class ExpKernel(var identifier : String,
    var passThroughArgs : ListBuffer[VariableAccess],
    var loopVariables : ListBuffer[String],
    var lowerBounds : ListBuffer[Expression],
    var upperBounds : ListBuffer[Expression],
    var increments : ListBuffer[Expression],
    var loopBody : ListBuffer[Statement],
    var loop : ForLoopStatement,
    var innerLoops : ListBuffer[ForLoopStatement],
    var reduction : Option[Reduction] = None) extends Node {

  import Kernel._

  val CudaMaxDimensionality = 3
  var evaluatedAccesses = false
  var evaluatedIndices = false
  var fieldAccesses = HashMap[String, LinearizedFieldAccess]()
  var ivAccesses = HashMap[String, iv.InternalVariable]()
  val executionConfigurationDimensionality = math.min(CudaMaxDimensionality, loopVariables.size)
  var minIndices = Array.fill[Expression](executionConfigurationDimensionality) { 0 }
  var maxIndices = Array.fill[Expression](executionConfigurationDimensionality) { 0 }
  var offsets = Array.fill[Expression](executionConfigurationDimensionality) { 0 }

  def getKernelFctName : String = identifier
  def getWrapperFctName : String = identifier + wrapperPostfix

  /**
   * Check the accesses in the loop to create valid function calls.
   */
  def evalAccesses = {
    if (!evaluatedAccesses) {
      GatherLocalLinearizedFieldAccess.fieldAccesses.clear
      GatherLocalLinearizedFieldAccess.applyStandalone(new Scope(loopBody))
      fieldAccesses = GatherLocalLinearizedFieldAccess.fieldAccesses

      GatherLocalIVs.ivAccesses.clear
      GatherLocalIVs.applyStandalone(new Scope(loopBody))
      ivAccesses = GatherLocalIVs.ivAccesses

      // postprocess iv's -> generate parameter names
      var cnt = 0
      val processedIVs = HashMap[String, iv.InternalVariable]()
      for (ivAccess <- ivAccesses) {
        processedIVs.put(ivAccess._2.resolveName + "_" + cnt, ivAccess._2)
        cnt += 1
      }
      ivAccesses = processedIVs

      evaluatedAccesses = true
    }
  }

  def evalIndices = {
    if (!evaluatedIndices) {
      (0 until executionConfigurationDimensionality).foreach(dim => {
        minIndices(dim) = lowerBounds(dim)
        maxIndices(dim) = upperBounds(dim)
        offsets(dim) = lowerBounds(dim)
      })

      evaluatedIndices = true
    }
  }

  def compileKernelBody : ListBuffer[Statement] = {
    evalAccesses // ensure that field accesses have been mapped
    evalIndices // calculate the minimal and maximal indices used in the loops

    var statements = ListBuffer[Statement]()

    // add CUDA global Thread ID (x,y,z) calculation for a dim3 execution configuration
    // global thread id x = blockIdx.x *blockDim.x + threadIdx.x + offset1;
    // global thread id y = blockIdx.y *blockDim.y + threadIdx.y + offset2;
    // global thread id z = blockIdx.z *blockDim.z + threadIdx.z + offset3;
    statements ++= (0 until executionConfigurationDimensionality).map(dim => {
      val it = dimToString(dim)
      VariableDeclarationStatement(IntegerDatatype, it,
        Some(MemberAccess(VariableAccess("blockIdx", Some(SpecialDatatype("dim3"))), it) *
          MemberAccess(VariableAccess("blockDim", Some(SpecialDatatype("dim3"))), it) +
          MemberAccess(VariableAccess("threadIdx", Some(SpecialDatatype("dim3"))), it) +
          offsets(dim)))
    })

    // add index bounds conditions
    statements ++= (0 until executionConfigurationDimensionality).map(dim => {
      def it = Duplicate(LoopOverDimensions.defItForDim(dim))
      new ConditionStatement(
        OrOrExpression(LowerExpression(it, s"begin_$dim"), GreaterEqualExpression(it, s"end_$dim")),
        ReturnStatement())
    })

    // add actual body after replacing field and iv accesses
    ReplacingLocalLinearizedFieldAccess.fieldAccesses = fieldAccesses
    ReplacingLocalLinearizedFieldAccess.applyStandalone(new Scope(loopBody))
    ReplacingLocalIVs.ivAccesses = ivAccesses
    ReplacingLocalIVs.applyStandalone(new Scope(loopBody))
    ReplacingLocalIVArrays.applyStandalone(new Scope(loopBody))
    statements ++= loopBody
    ReplacingArraySubscripts.loopVariables = loopVariables
    ReplacingArraySubscripts.applyStandalone(new Scope(statements))

    statements
  }

  def compileWrapperFunction : FunctionStatement = {
    evalAccesses // ensure that field accesses have been mapped
    evalIndices // calculate the minimal and maximal indices used in the loops

    // compile arguments for device function call
    var callArgs = ListBuffer[Expression]()
    for (dim <- 0 until executionConfigurationDimensionality) {
      // add dimension index start and end point
      callArgs += minIndices(dim)
      callArgs += maxIndices(dim)
    }

    for (fieldAccess <- fieldAccesses) {
      val fieldSelection = fieldAccess._2.fieldSelection
      callArgs += iv.FieldDeviceData(fieldSelection.field, fieldSelection.level, fieldSelection.slot)
    }
    for (ivAccess <- ivAccesses) {
      val access = Duplicate(ivAccess._2)
      // Hack for Vec3 -> TODO: split Vec3 iv's into separate real iv's
      access.resolveDataType match {
        case SpecialDatatype("Vec3") => callArgs ++= (0 until 3).map(dim => ArrayAccess(ivAccess._2, dim) : Expression).to[ListBuffer]
        case SpecialDatatype("Vec3i") => callArgs ++= (0 until 3).map(dim => ArrayAccess(ivAccess._2, dim) : Expression).to[ListBuffer]

        case _ => callArgs += ivAccess._2
      }
    }
    for (variableAccess <- passThroughArgs) {
      callArgs += Duplicate(variableAccess)
    }

    // evaluate required thread counts
    val minIndex = Array.fill[Long](executionConfigurationDimensionality) { 0 }
    val maxIndex = (0 until executionConfigurationDimensionality).map(dim => 1).toArray

    var numThreadsPerDim = (maxIndex, minIndex).zipped.map(_ - _)

    if (null == numThreadsPerDim || numThreadsPerDim.product <= 0) {
      Logger.warn("Could not evaluate required number of threads for kernel " + identifier)
      numThreadsPerDim = (0 until executionConfigurationDimensionality).map(dim => 0 : Long).toArray // TODO: replace 0 with sth more suitable
    }

    var body = ListBuffer[Statement]()
    body += new CUDA_FunctionCallExpression(getKernelFctName, callArgs, numThreadsPerDim)

    FunctionStatement(
      UnitDatatype,
      getWrapperFctName,
      Duplicate(passThroughArgs),
      body,
      allowInlining = false, allowFortranInterface = false,
      "extern \"C\"")
  }

  def compileKernelFunction : FunctionStatement = {
    evalAccesses // ensure that field accesses have been mapped

    // compile parameters for device function
    var fctParams = ListBuffer[VariableAccess]()

    for (dim <- 0 until executionConfigurationDimensionality) {
      fctParams += VariableAccess(s"begin_$dim", Some(IntegerDatatype))
      fctParams += VariableAccess(s"end_$dim", Some(IntegerDatatype))
    }

    for (fieldAccess <- fieldAccesses) {
      val fieldSelection = fieldAccess._2.fieldSelection
      fctParams += VariableAccess(fieldAccess._1, Some(PointerDatatype(fieldSelection.field.resolveDeclType)))
    }

    for (ivAccess <- ivAccesses) {
      val datatype = ivAccess._2.resolveDataType

      datatype match {
        case SpecialDatatype("Vec3") =>
          fctParams ++= (0 until 3).map(dim => VariableAccess(ivAccess._1 + '_' + dim, Some(SpecialDatatype("double")))).to[ListBuffer]
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
  var fieldAccesses = HashMap[String, LinearizedFieldAccess]()

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
  var fieldAccesses = HashMap[String, LinearizedFieldAccess]()

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

object GatherLocalIVs extends QuietDefaultStrategy("Gathering local InternalVariable nodes") {
  var ivAccesses = HashMap[String, iv.InternalVariable]()

  this += new Transformation("Searching", {
    case iv : iv.InternalVariable =>
      ivAccesses.put(iv.prettyprint, iv)
      iv
  }, false)
}

object ReplacingLocalIVs extends QuietDefaultStrategy("Replacing local InternalVariable nodes") {
  var ivAccesses = HashMap[String, iv.InternalVariable]()

  this += new Transformation("Searching", {
    case iv : iv.InternalVariable =>
      val ivAccess = ivAccesses.find(_._2 == iv).get // TODO: improve performance
      VariableAccess(ivAccess._1, Some(ivAccess._2.resolveDataType))
  })
}

object ReplacingLocalIVArrays extends QuietDefaultStrategy("Replacing local InternalVariable nodes") {

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

object ReplacingArraySubscripts extends QuietDefaultStrategy("Replacing array subscripts with dimensionality " +
  "depending indexing") {
  var loopVariables = ListBuffer[String]()

  this += new Transformation("Searching", {
    case VariableAccess(name @ n, maybeDatatype @ d) if loopVariables.contains(name) =>
      val newName = dimToString(loopVariables.indexOf(name))
      VariableAccess(newName, Some(IntegerDatatype))
  })
}
