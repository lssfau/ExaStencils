package exastencils.waLBerla.ir

import scala.collection.mutable.HashMap

import exastencils.base.ir.IR_ArrayAccess
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ForLoop
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_Lower
import exastencils.base.ir.IR_NullExpression
import exastencils.base.ir.IR_PointerDatatype
import exastencils.base.ir.IR_PreIncrement
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.baseExt.ir.IR_InternalVariable
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.prettyprinting.PpStream

abstract class WB_IV_AbstractFieldData extends IR_InternalVariable(true, false, true, true, false) {

  import IR_WaLBerlaDatatypes._

  var field : IR_WaLBerlaField
  var fragmentIdx : IR_Expression
  var level : IR_Expression
  var slot : IR_Expression

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index, level, IR_NullExpression)

  override def usesFieldArrays : Boolean = !Knowledge.data_useFieldNamesAsIdx

  override def resolveDefValue() = Some(0)

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

  override def resolveAccess(baseAccess : IR_Expression, fragment : IR_Expression, domain : IR_Expression, field : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Expression = {
    var access = super.resolveAccess(baseAccess, fragment, domain, field, level, neigh)
    if (this.field.numSlots > 1)
      access = IR_ArrayAccess(access, slot)
    access
  }

  // don't use as global variables
  override def getCtor() : Option[IR_Statement] = None
  override def getDtor() : Option[IR_Statement] = None
  override def registerIV(declarations : HashMap[String, IR_VariableDeclaration], ctors : HashMap[String, IR_Statement], dtors : HashMap[String, IR_Statement]) = {}

  override def datatype = WB_FieldDatatype(field)

  override def resolveDatatype() = {
    if (field.numSlots > 1)
      IR_ArrayDatatype(IR_PointerDatatype(datatype), field.numSlots)
    else
      IR_PointerDatatype(datatype)
  }
}

case class WB_IV_FieldData(
    override var field : IR_WaLBerlaField,
    override var slot : IR_Expression,
    override var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends WB_IV_AbstractFieldData {

  override var level : IR_Expression = field.level

  // TODO mapping naming conventions: wb <-> exa
  override def resolveName() = field.name
    //resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index.toString, level.prettyprint, "")

  def getData(defVal : Option[IR_Expression] = None) : IR_VariableDeclaration = IR_VariableDeclaration(IR_PointerDatatype(datatype), resolveName(), defVal)
}