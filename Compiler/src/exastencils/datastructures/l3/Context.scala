package exastencils.datastructures.l3

case class Context(
    val env : Environment,
    val tcb : TcbBlock,
    val stencils : StencilManager,
    val fields : FieldManager) {

}

class FieldManager {
  private var idCounter = 0

  private def genId() = {
    idCounter += 1
    "__FieldId%02d".format(idCounter)
  }

  def add() = {
    genId()
  }
}