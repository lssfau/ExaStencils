package exastencils.datastructures.l3

import exastencils.datastructures.l4
import scala.collection.mutable.ListBuffer

case class Context(
    val env : Environment,
    val tcb : TcbBlock,
    val stencils : StencilManager,
    val fields : FieldManager,
    val ids : IdManager = new IdManager) {

  def createNewScope() = {
    val newEnv = new Environment(Some(env))
    val newTcb = new TcbBlock()

    new Context(newEnv, newTcb, stencils, fields, ids)
  }

  def genId() = ids.genId
}

class IdManager {
  private var idCounter = 0

  def genId() = {
    idCounter += 1
    "__id%02d".format(idCounter)
  }
}

class FieldManager {
  private var idCounter = 0

  private val stms = ListBuffer[l4.SpecialStatement]()
  def statements = stms

  private def genId() = {
    idCounter += 1
    "__field_id%02d".format(idCounter)
  }

  def add(level : Int) = {
    val id = genId()

    val tcId = l4.LeveledIdentifier(id, l4.SingleLevelSpecification(level))
    stms += l4.FieldDeclarationStatement(tcId, "dfl_domain", "dfl_layout", None, 1)

    id
  }
}

/** Manages the code for all automatically generated stencils. */
class StencilManager {

  // add a stencil and return its identifier
  private val stms = ListBuffer[l4.SpecialStatement]()
  def statements = stms

  private var idCount = 0

  def genId() : String = {
    idCount += 1
    "__stencil_id%02d".format(idCount)
  }

  def add(s : ListStaticValue) : String = {
    val id = genId()

    stms += Stencil(s).toTc(id)

    id
  }

}
