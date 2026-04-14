package exastencils.waLBerla.ir.field

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverLocalBlockArray
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverLocalBlocks
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaInterfaceMember

/// IR_IV_WaLBerlaGetFieldShapeInfo

abstract class IR_IV_WaLBerlaGetFieldShapeInfo extends IR_WaLBerlaInterfaceMember(true, true, false) {

  def field : IR_WaLBerlaField
  def level : IR_Expression
  def fragmentIdx : IR_Expression = IR_WaLBerlaLoopOverLocalBlocks.defIt


  private val levels = IR_WaLBerlaFieldCollection.getAllByIdentifier(field.name, suppressError = true).map(_.level)
  override def minLevel : Int = levels.min
  override def maxLevel : Int = levels.max

  override def isPrivate : Boolean = true

  override def resolveDatatype() : IR_Datatype = IR_IntegerDatatype

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess()

  override def resolveDefValue() = Some(0)

  protected def resolveAccess() : IR_Expression = resolveAccess(resolveMemberBaseAccess(), fragmentIdx, level, IR_NullExpression)
}

/// IR_WaLBerlaFieldShapeInfo

case class IR_WaLBerlaFieldShapeInfo(
    var field : IR_WaLBerlaField,
    var name : String,
) extends IR_IV_WaLBerlaGetFieldShapeInfo {

  var level : IR_Expression = field.level

  override def getCtor() : Option[IR_Statement] = {
    val getField = IR_IV_WaLBerlaGetField(field, IR_IntegerConstant(0), onGPU = false, fragmentIdx)

    Some(IR_WaLBerlaLoopOverLocalBlockArray(
      IR_Assignment(resolveAccess(), IR_Cast(IR_IntegerDatatype, IR_MemberFunctionCallArrowWithDt(getField, name, IR_IntegerDatatype)))))
  }
}