package exastencils.parallelization.api.cuda

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.logger._
import exastencils.prettyprinting._

/// CUDA_FunctionCall

object CUDA_FunctionCall {
  def apply(name : String, arguments : ListBuffer[IR_Expression], numThreadsPerDim : Array[Long]) =
    new CUDA_FunctionCall(name, arguments, numThreadsPerDim.map(n => n : IR_Expression))
  def apply(name : String, arguments : ListBuffer[IR_Expression], numThreadsPerDim : Array[Long], numBlocksPerDim : Array[Long]) =
    new CUDA_FunctionCall(name, arguments, numThreadsPerDim.map(n => n : IR_Expression), numBlocksPerDim.map(n => n : IR_Expression))

}

case class CUDA_FunctionCall(
    var name : String,
    var arguments : ListBuffer[IR_Expression],
    var numThreadsPerDim : Array[IR_Expression],
    var numBlocksPerDim : Array[IR_Expression] = Knowledge.cuda_blockSizeAsVec.map(n => n : IR_Expression)) extends IR_Expression {

  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = {
    val numDims = numThreadsPerDim.size
    if (numDims > 3) Logger.warn(s"${ numDims }D kernel found; this is currently unsupported by CUDA") // TODO: check relation to compute capability

    val numBlocks = (0 until numDims).map(dim => {
      (numThreadsPerDim(dim) + numBlocksPerDim(dim) - 1) / numBlocksPerDim(dim)
    }).toArray

    // TODO: simplify? check if integer ops are handled correctly

    out << name << "<<<"
    if (1 == numDims)
      out << numBlocks(0) << ", " << numBlocksPerDim(0) // only one dimensions -> wrapping not necessary
    else
      out << s"dim3(" <<< (numBlocks, ", ") << "), " << s"dim3(" <<< (numBlocksPerDim.take(numDims), ", ") << ")"

    out << ">>>" << '(' <<< (arguments, ", ") << ')'
  }
}

/// CUDA_FunctionCallExperimental

object CUDA_FunctionCallExperimental {
  def apply(name : String, arguments : ListBuffer[IR_Expression], numThreadsPerDim : Array[Long]) =
    new CUDA_FunctionCallExperimental(name, arguments, numThreadsPerDim.map(n => n : IR_Expression))
  def apply(name : String, arguments : ListBuffer[IR_Expression], numThreadsPerDim : Array[Long], numBlocksPerDim : Array[Long]) =
    new CUDA_FunctionCallExperimental(name, arguments, numThreadsPerDim.map(n => n : IR_Expression), numBlocksPerDim.map(n => n : IR_Expression))
}

// TODO: find out what this class is used for
case class CUDA_FunctionCallExperimental(
    var name : String,
    var arguments : ListBuffer[IR_Expression],
    var numThreadsPerDim : Array[IR_Expression],
    var numBlocksPerDim : Array[IR_Expression] = Knowledge.cuda_blockSizeAsVec.map(n => n : IR_Expression)) extends IR_Expression {

  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = {
    val numDims = numThreadsPerDim.size
    if (numDims > 3) Logger.warn(s"${ numDims }D kernel found; this is currently unsupported by CUDA")

    out << name << "<<<"
    if (1 == numDims)
      out << numBlocksPerDim(0) << ", " << numThreadsPerDim(0) // only one dimensions -> wrapping not necessary
    else
      out << s"dim3(" <<< (numBlocksPerDim.take(numDims), ", ") << "), " << s"dim3(" <<< (numThreadsPerDim.take(numDims), ", ") << ")"

    out << ">>>" << '(' <<< (arguments, ", ") << ')'
  }
}
