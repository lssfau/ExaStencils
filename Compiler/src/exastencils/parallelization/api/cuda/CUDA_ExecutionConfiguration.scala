package exastencils.parallelization.api.cuda

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

object CUDA_ExecutionConfiguration {
  def apply(numBlocks : Array[Long], numThreads : Array[Long], stream : IR_Expression) =
    new CUDA_ExecutionConfiguration(numBlocks.map(n => n : IR_Expression), numThreads.map(n => n : IR_Expression), stream)

  def defaultSharedMemPerBlock : IR_Expression = 0
}

case class CUDA_ExecutionConfiguration(
    var numBlocks : Array[IR_Expression],
    var numThreads : Array[IR_Expression],
    var stream : IR_Expression, // associated stream, default: 0
    var sharedMemPerBlock : IR_Expression = CUDA_ExecutionConfiguration.defaultSharedMemPerBlock // dynamically allocated shared mem in bytes, default: 0
) extends IR_Expression {

  override def datatype : IR_Datatype = IR_UnknownDatatype

  override def prettyprint(out : PpStream) : Unit = {
    val numDims = numThreads.size
    if (numDims > 3) Logger.warn(s"${ numDims }D kernel found; this is currently unsupported by CUDA")

    out << "<<<"

    if (1 == numDims)
      out << numBlocks(0) << ", " << numThreads(0) // only one dimensions -> wrapping not necessary
    else
      out << s"dim3(" <<< (numBlocks.take(numDims), ", ") << "), " << s"dim3(" <<< (numThreads.take(numDims), ", ") << ")"

    out << ", " << sharedMemPerBlock << ", " << stream

    out << ">>>"
  }
}
