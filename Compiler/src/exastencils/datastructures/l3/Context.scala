package exastencils.datastructures.l3

case class Context(
    val env : Environment,
    val tcb : TcbBlock,
    val stencils : StencilManager,
    val fields : FieldManager) {

  def createNewScope() = {
    val newEnv = new Environment(Some(env))
    val newTcb = new TcbBlock()

    new Context(newEnv, newTcb, stencils, fields)
  }

}

class FieldManager {
  private var idCounter = 0

  private def genId() = {
    idCounter += 1
    "__field_id%02d".format(idCounter)
  }

  def add() = {
    genId()
  }
}

