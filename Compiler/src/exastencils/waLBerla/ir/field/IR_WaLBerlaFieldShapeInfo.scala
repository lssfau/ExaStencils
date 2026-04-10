package exastencils.waLBerla.ir.field

import exastencils.base.ir._
import exastencils.datastructures._

/// IR_WaLBerlaFieldShapeInfo

case class IR_WaLBerlaFieldShapeInfo(
    var getField : IR_IV_WaLBerlaGetField,
    var funcName : String
) extends IR_Expression with IR_SpecialExpandable {
  def datatype : IR_Datatype = IR_IntegerDatatype

  def expandSpecial() : IR_Expression = {
    IR_Cast(IR_IntegerDatatype, IR_MemberFunctionCallArrowWithDt(getField, funcName, IR_IntegerDatatype))
  }
}

/// IR_ResolveWaLBerlaFieldShapeInfo

object IR_ResolveWaLBerlaFieldShapeInfo extends DefaultStrategy("Resolve nodes for remote data transfer") {
  this += Transformation("..", {
    case shapeInfo : IR_WaLBerlaFieldShapeInfo => shapeInfo.expandSpecial()
  })
}