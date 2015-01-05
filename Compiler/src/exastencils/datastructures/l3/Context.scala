package exastencils.datastructures.l3

import exastencils.math._

case class Context(
    val env : Environment,
    val tcb : TcbBlock,
    val stencils : StencilManager) {

}

class StencilManager {

  // add a stencil and return its identifier
  private val statements = new TcbBlock

  private var idCount = 0

  def add(s : GeneralStencil[Double]) : String = {

    // @todo: Implement code generation for stencils

    idCount += 1
    "__stencil_id%02d".format(idCount)
  }

}
