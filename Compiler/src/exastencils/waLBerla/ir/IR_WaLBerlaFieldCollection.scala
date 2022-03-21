package exastencils.waLBerla.ir

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerConstant
import exastencils.field.ir.IR_FieldAccessLike
import exastencils.fieldlike.ir.IR_FieldLikeCollection
import exastencils.logger.Logger

object IR_WaLBerlaFieldCollection extends IR_FieldLikeCollection[IR_WaLBerlaField] {
  exastencils.core.Duplicate.registerConstant(this)

  private def getByCallbackLevExp(
      identifier : String,
      level : IR_Expression,
      getById : (String, IR_Expression, Boolean) => Option[IR_WaLBerlaField],
      suppressError : Boolean = false) : Option[IR_WaLBerlaField] = {

    level match {
      case IR_IntegerConstant(constLevel) => getById(identifier, constLevel, suppressError)
      case _                              =>
        if (!suppressError) Logger.warn(s"Trying to find field $identifier on level ${ level.prettyprint } - non-constant levels are not supported")
        None
    }
  }

  def contains(access : IR_WaLBerlaFieldAccess) : Boolean =
    getByIdentifier(access.field.name, access.field.level, suppressError = true).isDefined

  def contains(access : IR_FieldAccessLike) : Boolean =
    getByIdentifier(access.field.name, access.field.level, suppressError = true).isDefined

  // special handling for leveled expressions
  def getByLayoutIdentifierLevExp(identifier : String, level : IR_Expression, suppressError : Boolean = false) : Option[IR_WaLBerlaField] = {
    def getByLayoutId(identifier : String, level : IR_Expression, suppressError : Boolean = false) : Option[IR_WaLBerlaField] =
      objects.find(field => field.layout.name == identifier && IR_IntegerConstant(field.level) == level)

    getByCallbackLevExp(identifier, level, getByLayoutId, suppressError)
  }
}
