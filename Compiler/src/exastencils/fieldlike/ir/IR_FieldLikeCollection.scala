package exastencils.fieldlike.ir

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_IntegerConstant
import exastencils.knowledge.ir.IR_LeveledKnowledgeCollection
import exastencils.logger.Logger

object IR_FieldLikeCollections {
  val collections = ListBuffer[IR_FieldLikeCollection[_ <: IR_FieldLike]]()

  def register(collection : IR_FieldLikeCollection[_ <: IR_FieldLike]) =
    collections += collection

  def getByIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[IR_FieldLike] = {
    for (coll <- collections) {
      if (coll.exists(identifier, level))
        return Some(coll.getByIdentifier(identifier, level, suppressError).get)
    }
    None
  }

  def clear() = collections.foreach(_.clear())
}

class IR_FieldLikeCollection[IR_FieldAbstraction <: IR_FieldLike : TypeTag] extends IR_LeveledKnowledgeCollection[IR_FieldAbstraction] {

  // special handling for level expressions
  def getByIdentifierLevExp(identifier : String, level : IR_Expression, suppressError : Boolean = false) : Option[IR_FieldAbstraction] = {
    level match {
      case IR_IntegerConstant(constLevel) => getByIdentifier(identifier, constLevel.toInt, suppressError)
      case _                              =>
        if (!suppressError) Logger.warn(s"Trying to find field $identifier on level ${ level.prettyprint } - non-constant levels are not supported")
        None
    }
  }

  // special function to find fields according to their layout
  @deprecated("to be removed", "04.10.16")
  def getByLayoutIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[IR_FieldAbstraction] = {
    val ret = objects.find(field => field.layout.name == identifier && field.level == level)
    if (!suppressError && ret.isEmpty) Logger.warn(s"External field with layout $identifier on level $level was not found")
    ret
  }

  def remove(oldObj : IR_FieldAbstraction) : Unit = objects -= oldObj
}
