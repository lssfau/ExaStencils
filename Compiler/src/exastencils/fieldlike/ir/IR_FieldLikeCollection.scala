package exastencils.fieldlike.ir

import scala.reflect.runtime.universe._
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_IntegerConstant
import exastencils.knowledge.ir.IR_LeveledKnowledgeCollection
import exastencils.logger.Logger

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
