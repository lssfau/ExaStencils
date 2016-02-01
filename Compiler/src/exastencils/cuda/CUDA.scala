package exastencils.cuda

import exastencils.datastructures.ir._
import exastencils.prettyprinting._

trait CUDA_Statement extends Statement

case class CUDA_Init() extends CUDA_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "cuInit(0);"
}

case class CUDA_Finalize() extends CUDA_Statement {
  override def prettyprint(out : PpStream) : Unit = { /* nothing to do here (yet) */ }
}
