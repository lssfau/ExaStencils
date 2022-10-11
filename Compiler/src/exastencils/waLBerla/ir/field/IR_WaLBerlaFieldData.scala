package exastencils.waLBerla.ir.field

import scala.collection.mutable

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.field.ir.IR_FieldAccess
import exastencils.fieldlike.ir.IR_IV_AbstractFieldLikeData
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_FieldDatatype

object IR_IV_WaLBerlaFieldData {
  def apply(fAcc : IR_FieldAccess) : IR_IV_WaLBerlaFieldData = {
    val wbfield = IR_WaLBerlaFieldCollection.getByIdentifier(fAcc.name, fAcc.level, suppressError = true).get
    new IR_IV_WaLBerlaFieldData(wbfield, fAcc.slot, fAcc.fragIdx)
  }

  def apply(fAcc : IR_WaLBerlaFieldAccess) : IR_IV_WaLBerlaFieldData = new IR_IV_WaLBerlaFieldData(fAcc.target, fAcc.slot, fAcc.fragIdx)
}

case class IR_IV_WaLBerlaFieldData(
    var field : IR_WaLBerlaField,
    var slot : IR_Expression,
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_AbstractFieldLikeData(true, false, true, false, false) {

  var level : IR_Expression = field.level

  override def datatype : IR_SpecialDatatype = WB_FieldDatatype(field)

  override def resolveDatatype() : IR_Datatype = {
    if (field.numSlots > 1)
      IR_ArrayDatatype(IR_PointerDatatype(datatype), field.numSlots)
    else
      IR_PointerDatatype(datatype)
  }

  override def resolveDefValue() = Some(0)

  // don't use as global variables
  override def getCtor() : Option[IR_Statement] = None
  override def getDtor() : Option[IR_Statement] = None
  override def registerIV(declarations : mutable.HashMap[String, IR_VariableDeclaration], ctors : mutable.HashMap[String, IR_Statement], dtors : mutable.HashMap[String, IR_Statement]) : Unit = {}

  override def resolveName() : String = field.codeName
}