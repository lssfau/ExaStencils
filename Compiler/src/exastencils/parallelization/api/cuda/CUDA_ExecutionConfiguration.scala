package exastencils.parallelization.api.cuda

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.config.Platform
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.prettyprinting.PpStream

trait CUDA_ExecutionConfiguration extends IR_Expression {
  def numThreadsPerBlock : Array[IR_Expression]
  def numBlocksPerDim : Array[IR_Expression]

  def stream : CUDA_Stream // associated stream
  def sharedMemPerBlock : IR_Expression // dynamically allocated shared mem in bytes, default: 0
}

object CUDA_ExecutionConfiguration {
  def defaultSharedMemPerBlock : IR_Expression = 0
}

case class CUDA_ExecutionConfigurationStatic(
    var stream : CUDA_Stream,
    var sharedMemPerBlock : IR_Expression = CUDA_ExecutionConfiguration.defaultSharedMemPerBlock
) extends CUDA_ExecutionConfiguration {

  override def datatype : IR_Datatype = IR_UnknownDatatype

  var numThreadsPerBlock : Array[IR_Expression] = Array()
  var numBlocksPerDim : Array[IR_Expression] = Array()

  // compute good fit between iteration space and CUDA block sizes specified as knowledge flags
  def computeGridAndBlockDimensions(
      executionDim : Int,
      requiredThreadsPerDim : Array[Long],
      stepSize : ListBuffer[IR_Expression]) = {

    var numThreads : Array[Long] = Array()
    var numBlocks : Array[Long] = Array()

    def getIntersectionWithIterationSpace(blockSizes : Array[Long]) : Array[Long] =
      blockSizes.zipWithIndex.map { case (blockSize, idx) => scala.math.min(blockSize, requiredThreadsPerDim(idx)) }

    // use user-defined block sizes as initial config and intersect iteration space with cuda block sizes
    var blockSizes = getIntersectionWithIterationSpace(Knowledge.cuda_blockSizeAsVec.take(executionDim))
    var done = false
    if (blockSizes.product >= Knowledge.cuda_minimalBlockSize) {
      // case 1: greater than min block size -> use intersection
      numThreads = blockSizes.map(identity)
      done = true
    }

    var prevTrimSize = 0L
    val resizeOrder = if (executionDim == 3) List(0, 2, 1) else 0 until executionDim
    while (blockSizes.product != prevTrimSize && !done) {
      prevTrimSize = blockSizes.product

      // case 2: intersected block is equivalent to iter space -> return
      if (blockSizes.zip(requiredThreadsPerDim.take(executionDim)).forall(tup => tup._1 == tup._2)) {
        numThreads = blockSizes.map(identity)
        done = true
      } else {
        // double block size in each dimension until block is large enough (or case 2 triggers)
        for (d <- resizeOrder) {
          // resize
          blockSizes(d) *= 2

          // optional: trim innermost dim to multiples of warp size
          if (d == 0 && blockSizes(d) > Platform.hw_cuda_warpSize && blockSizes(d) % Platform.hw_cuda_warpSize != 0)
            blockSizes(d) = blockSizes(d) - (blockSizes(d) % Platform.hw_cuda_warpSize) // subtract remainder

          // check if block sizes are within hardware capabilities
          blockSizes(d) = math.min(blockSizes(d), Platform.hw_cuda_maxBlockSizes(d))

          // intersect again
          blockSizes = getIntersectionWithIterationSpace(blockSizes.map(identity))

          // case 3: intersected block is large enough
          if (blockSizes.product >= Knowledge.cuda_minimalBlockSize) {
            numThreads = blockSizes.map(identity)
            done = true
          }
        }
      }
    }

    // shrink if max number of threads is exceeded
    while (blockSizes.product >= Platform.hw_cuda_maxNumThreads) {
      var shrink = true
      var d = 0
      while (shrink && d < resizeOrder.length) {
        val idx = resizeOrder.reverse(d)
        blockSizes(idx) /= 2
        blockSizes(idx) = math.max(1, blockSizes(idx))

        if (blockSizes.product < Platform.hw_cuda_maxNumThreads)
          shrink = false

        d = d + 1
      }
    }

    // adapt thread count for reduced dimensions
    if (Knowledge.cuda_foldBlockSizeForRedDimensionality)
      for (d <- executionDim until Knowledge.dimensionality)
        numThreads(0) *= Knowledge.cuda_blockSizeAsVec(d)

    numBlocks = (0 until executionDim).map(dim => {
      val inc = stepSize(dim) match {
        case IR_IntegerConstant(i) => i
        case _                     => 1
      }
      val nrThreads = (requiredThreadsPerDim(dim) + inc - 1) / inc
      (nrThreads + numThreads(dim) - 1) / numThreads(dim)
    }).toArray
  }

  override def prettyprint(out : PpStream) : Unit = {
    val numDims = numThreadsPerBlock.length
    if (numDims > 3) Logger.warn(s"${ numDims }D kernel found; this is currently unsupported by CUDA")

    out << "<<<"

    if (1 == numDims)
      out << numBlocksPerDim(0) << ", " << numThreadsPerBlock(0) // only one dimensions -> wrapping not necessary
    else
      out << s"dim3(" << numBlocksPerDim.take(numDims).mkString(",") << "), " << s"dim3(" << numThreadsPerBlock.take(numDims).mkString(",") << ")"

    if (sharedMemPerBlock != CUDA_ExecutionConfiguration.defaultSharedMemPerBlock || stream.useNonDefaultStreams)
      out << ", " << sharedMemPerBlock

    if (stream.useNonDefaultStreams)
      out << ", " << stream

    out << ">>>"
  }
}

case class CUDA_ExecutionConfigurationDynamic(
    var executionDim : Int,
    var lowerBounds : ListBuffer[IR_Expression],
    var upperBounds : ListBuffer[IR_Expression],
    var stepSize : ListBuffer[IR_Expression],
    var stream : CUDA_Stream,
    var sharedMemPerBlock : IR_Expression = CUDA_ExecutionConfiguration.defaultSharedMemPerBlock
) extends CUDA_ExecutionConfiguration {

  override def datatype : IR_Datatype = IR_UnknownDatatype

  def getNumBlocks = IR_FunctionCall(CUDA_ComputeGridDimConfiguration(executionDim, stepSize).name, (lowerBounds, upperBounds).zipped.map((a, b) => b - a) : _*)
  def getNumThreads = IR_FunctionCall(CUDA_ComputeBlockDimConfiguration(executionDim).name, (lowerBounds, upperBounds).zipped.map((a, b) => b - a) : _*)

  override def numBlocksPerDim : Array[IR_Expression] = (0 until executionDim).toArray.map(d => IR_ArrayAccess(getNumBlocks, d))
  override def numThreadsPerBlock : Array[IR_Expression] = (0 until executionDim).toArray.map(d => IR_ArrayAccess(getNumThreads, d))

  override def prettyprint(out : PpStream) : Unit = {
    out << "<<<" << getNumBlocks << ", " << getNumThreads

    if (sharedMemPerBlock != CUDA_ExecutionConfiguration.defaultSharedMemPerBlock || stream.useNonDefaultStreams)
      out << ", " << sharedMemPerBlock

    if (stream.useNonDefaultStreams)
      out << ", " << stream

    out << ">>>"
  }
}

sealed trait CUDA_ComputeExecutionConfigurationFunction extends IR_FuturePlainFunction {
  def executionDim : Int

  def dt = IR_SpecialDatatype("dim3")

  def getNumberOfThreads() = {
    val numThreadsPerBlock = Knowledge.cuda_blockSizeAsVec.take(executionDim)

    // adapt thread count for reduced dimensions
    if (Knowledge.cuda_foldBlockSizeForRedDimensionality)
      for (d <- executionDim until Knowledge.dimensionality)
        numThreadsPerBlock(0) *= Knowledge.cuda_blockSizeAsVec(d)

    numThreadsPerBlock
  }

  def wrappedGenerateFct() : IR_PlainFunction
  override def generateFct() : IR_PlainFunction = {
    val fct = wrappedGenerateFct()

    fct.functionQualifiers = "inline"
    fct.allowInlining = false
    fct.allowFortranInterface = false

    val cudaFcts = CUDA_KernelFunctions.get

    if (!cudaFcts.functions.exists(_.name == fct.name))
      cudaFcts.functions += fct

    fct
  }

  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint
}

sealed case class CUDA_ComputeBlockDimConfiguration(
    var executionDim : Int
) extends CUDA_ComputeExecutionConfigurationFunction {

  override def wrappedGenerateFct() : IR_PlainFunction = {
    var body = ListBuffer[IR_Statement]()

    body += IR_Return(IR_FunctionCall(dt.typeName, getNumberOfThreads().map(IR_IntegerConstant) : _*))

    IR_PlainFunction(name, dt, ListBuffer[IR_FunctionArgument](), body)
  }

  override def name : String = "getThreads"
}

sealed case class CUDA_ComputeGridDimConfiguration(
    var executionDim : Int,
    var stepSize : ListBuffer[IR_Expression]
) extends CUDA_ComputeExecutionConfigurationFunction {

  override def wrappedGenerateFct() : IR_PlainFunction = {
    val requiredThreadsPerDim = (0 until executionDim).map(d => IR_VariableAccess(s"numThreads_${ ('x' + d).toChar }", IR_IntegerDatatype))

    var body = ListBuffer[IR_Statement]()

    val numThreadsPerBlock = getNumberOfThreads()

    val numBlocksPerDim = (0 until executionDim).map(dim => {
      val inc = stepSize(dim) match {
        case IR_IntegerConstant(i) => i
        case _                     => 1
      }
      val nrThreads = (requiredThreadsPerDim(dim) + inc - 1) / inc
      (nrThreads + numThreadsPerBlock(dim) - 1) / numThreadsPerBlock(dim)
    }).toArray

    body += IR_Return(IR_FunctionCall(dt.typeName, numBlocksPerDim : _*))

    IR_PlainFunction(name, IR_SpecialDatatype("dim3"), requiredThreadsPerDim.map(IR_FunctionArgument(_)).to[ListBuffer], body)
  }

  private val stepSizeProd = IR_SimplifyExpression.simplifyIntegralExpr(stepSize.reduce(_ * _))
  private val postFix = stepSizeProd match {
    case IR_IntegerConstant(1) => ""
    case IR_IntegerConstant(v) => "s_" + v
    case _                     => "s_" + stepSize.map(_.prettyprint()).mkString("_")
  }
  override def name : String = s"getBlocks_$postFix"
}
