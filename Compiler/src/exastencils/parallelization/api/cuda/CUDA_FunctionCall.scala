package exastencils.parallelization.api.cuda

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.logger._
import exastencils.prettyprinting._

/// CUDA_FunctionCallExperimental

object CUDA_FunctionCall {
  def apply(name : String, arguments : ListBuffer[IR_Expression], numBlocks : Array[Long], numThreads : Array[Long]) =
    new CUDA_FunctionCall(name, arguments, numBlocks.map(n => n : IR_Expression), numThreads.map(n => n : IR_Expression))
}

case class CUDA_FunctionCall(
    var name : String,
    var arguments : ListBuffer[IR_Expression],
    var numBlocks : Array[IR_Expression],
    var numThreads : Array[IR_Expression]) extends IR_Expression {

  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = {
    val numDims = numThreads.size
    if (numDims > 3) Logger.warn(s"${ numDims }D kernel found; this is currently unsupported by CUDA")

    out << name << "<<<"
    if (1 == numDims)
      out << numBlocks(0) << ", " << numThreads(0) // only one dimensions -> wrapping not necessary
    else
      out << s"dim3(" <<< (numBlocks.take(numDims), ", ") << "), " << s"dim3(" <<< (numThreads.take(numDims), ", ") << ")"

    out << ">>>" << '(' <<< (arguments, ", ") << ')'
  }
}
