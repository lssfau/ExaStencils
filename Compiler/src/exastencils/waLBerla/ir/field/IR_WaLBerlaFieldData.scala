package exastencils.waLBerla.ir.field

import scala.collection.mutable

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.field.ir.IR_FieldAccess
import exastencils.prettyprinting.PpStream
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
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, true, false, false) {

  var level : IR_Expression = field.level

  override def datatype : IR_SpecialDatatype = WB_FieldDatatype(field)

  override def prettyprint(out : PpStream) : Unit =
    out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index, level, IR_NullExpression)

  override def usesFieldArrays : Boolean = !Knowledge.data_useFieldNamesAsIdx

  override def resolveDefValue() = Some(0)

  // don't use as global variables
  override def getCtor() : Option[IR_Statement] = None
  override def getDtor() : Option[IR_Statement] = None
  override def registerIV(declarations : mutable.HashMap[String, IR_VariableDeclaration], ctors : mutable.HashMap[String, IR_Statement], dtors : mutable.HashMap[String, IR_Statement]) : Unit = {}

  override def resolveAccess(baseAccess : IR_Expression, fragment : IR_Expression, domain : IR_Expression, field : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Expression = {
    var access = super.resolveAccess(baseAccess, fragment, domain, field, IR_NullExpression, neigh)
    if (this.field.numSlots > 1)
      access = IR_ArrayAccess(access, slot)
    access
  }

  override def resolveDatatype() : IR_Datatype = {
    if (field.numSlots > 1)
      IR_ArrayDatatype(IR_PointerDatatype(datatype), field.numSlots)
    else
      IR_PointerDatatype(datatype)
  }

  override def resolveName() : String = field.codeName

  def getData(defVal : Option[IR_Expression] = None) : IR_VariableDeclaration = IR_VariableDeclaration(resolveDatatype(), resolveName(), defVal)
}