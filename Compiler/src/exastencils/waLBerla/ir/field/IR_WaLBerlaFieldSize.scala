package exastencils.waLBerla.ir.field

import exastencils.base.ir._
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockLoopVariable
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_UintType

case class IR_WaLBerlaFieldSize(var getField : IR_IV_WaLBerlaGetField, var sizeName : String, var declName : String) extends IR_WaLBerlaBlockLoopVariable {

  override def resolveName() : String = declName
  override def resolveDatatype() : IR_Datatype = IR_IntegerDatatype

  override def getDeclaration() : IR_VariableDeclaration = IR_VariableDeclaration(resolveDatatype(), resolveName(),
    IR_Cast(IR_IntegerDatatype, IR_MemberFunctionCallArrowWithDt(getField, sizeName, IR_SpecialDatatype(s"const ${WB_UintType.typeName}"))) : IR_Expression)
}
