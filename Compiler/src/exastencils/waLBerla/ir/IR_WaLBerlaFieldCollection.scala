package exastencils.waLBerla.ir

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_IntegerConstant
import exastencils.knowledge.ir.IR_LeveledKnowledgeCollection
import exastencils.logger.Logger

object IR_WaLBerlaFieldCollection extends IR_LeveledKnowledgeCollection[IR_WaLBerlaField] {
  exastencils.core.Duplicate.registerConstant(this)

  // special handling for leveled expressions
  def getByLayoutIdentifierLevExp(identifier : String, level : IR_Expression, suppressError : Boolean = false) : Option[IR_WaLBerlaField] = {
    level match {
      case IR_IntegerConstant(constLevel) => objects.find(field => field.layout.name == identifier && field.level == constLevel)
      case _                              =>
        if (!suppressError) Logger.warn(s"Trying to find field $identifier on level ${ level.prettyprint } - non-constant levels are not supported")
        None
    }
  }

  def remove(oldObj : IR_WaLBerlaField) : Unit = objects -= oldObj
}
