package exastencils.waLBerla.ir

import scala.collection.mutable

import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_NullExpression
import exastencils.base.ir.IR_PointerDatatype
import exastencils.base.ir.IR_SpecialDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.baseExt.ir.IR_InternalVariable
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.field.ir.IR_FieldAccess
import exastencils.prettyprinting.PpStream

// TODO: potentially extend with slots

abstract class WB_IV_AbstractFieldData extends IR_InternalVariable(true, false, true, false, false) {

  import IR_WaLBerlaDatatypes._

  var field : IR_WaLBerlaField
  var fragmentIdx : IR_Expression
  var level : IR_Expression
  //var slot : IR_Expression

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index, level, IR_NullExpression)

  override def usesFieldArrays : Boolean = !Knowledge.data_useFieldNamesAsIdx

  override def resolveDefValue() = Some(0)

  /*
  override def wrapInLoops(body : IR_Statement) : IR_Statement = {
    var wrappedBody = body
    if (field.numSlots > 1)
      wrappedBody = IR_ForLoop(
        IR_VariableDeclaration(IR_IntegerDatatype, "slot", 0),
        IR_Lower("slot", field.numSlots),
        IR_PreIncrement("slot"),
        wrappedBody)
    super.wrapInLoops(wrappedBody)
  }
  */

  override def resolveAccess(baseAccess : IR_Expression, fragment : IR_Expression, domain : IR_Expression, field : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Expression = {
    var access = super.resolveAccess(baseAccess, fragment, domain, field, IR_NullExpression, neigh)
    /*
    if (this.field.numSlots > 1)
      access = IR_ArrayAccess(access, slot)
    */
    access
  }

  // don't use as global variables
  override def getCtor() : Option[IR_Statement] = None
  override def getDtor() : Option[IR_Statement] = None
  override def registerIV(declarations : mutable.HashMap[String, IR_VariableDeclaration], ctors : mutable.HashMap[String, IR_Statement], dtors : mutable.HashMap[String, IR_Statement]) : Unit = {}

  override def datatype : IR_SpecialDatatype = WB_FieldDatatype(field)

  override def resolveDatatype() : IR_Datatype = {
    if (field.numSlots > 1)
      IR_ArrayDatatype(IR_PointerDatatype(datatype), field.numSlots)
    else
      IR_PointerDatatype(datatype)
  }
}

object WB_IV_FieldData {
  def apply(fAcc : IR_FieldAccess) : WB_IV_FieldData = {
    val field = IR_WaLBerlaFieldCollection.getByIdentifier(fAcc.name, fAcc.level, suppressError = true).get
    new WB_IV_FieldData(field, fAcc.fragIdx)
  }

  def apply(fAcc : IR_WaLBerlaFieldAccess) : WB_IV_FieldData = new WB_IV_FieldData(fAcc.target, fAcc.fragIdx)
}

case class WB_IV_FieldData(
    override var field : IR_WaLBerlaField,
    //override var slot : IR_Expression,
    override var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends WB_IV_AbstractFieldData {

  override var level : IR_Expression = field.level

  // TODO mapping naming conventions: wb <-> exa
  override def resolveName() : String = field.name
    //resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index.toString, level.prettyprint, "")

  def getData(defVal : Option[IR_Expression] = None) : IR_VariableDeclaration = IR_VariableDeclaration(IR_PointerDatatype(datatype), resolveName(), defVal)
}