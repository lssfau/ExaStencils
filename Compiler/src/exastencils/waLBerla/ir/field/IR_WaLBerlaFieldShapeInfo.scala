package exastencils.waLBerla.ir.field

import exastencils.base.ir._
import exastencils.field.ir.IR_HasVariableFieldSize
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockLoopVariable
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverLocalBlockArray

/// IR_WaLBerlaFieldShapeInfo

case class IR_WaLBerlaFieldShapeInfo(
    var field : IR_WaLBerlaField,
    var funcName : String,
) extends IR_WaLBerlaBlockLoopVariable with IR_HasVariableFieldSize {

  override def resolveName() : String = s"${field.codeName}_${funcName}"

  override def resolveDatatype() : IR_Datatype = IR_IntegerDatatype

  override def getDeclaration() : IR_VariableDeclaration = {
    val getField = IR_IV_WaLBerlaGetField(field, IR_IntegerConstant(0), onGPU = false, IR_WaLBerlaLoopOverLocalBlockArray.defIt)

    IR_VariableDeclaration(resolveDatatype(), resolveName(),
      IR_Cast(IR_IntegerDatatype, IR_MemberFunctionCallArrowWithDt(getField, funcName, IR_IntegerDatatype)))
  }
}