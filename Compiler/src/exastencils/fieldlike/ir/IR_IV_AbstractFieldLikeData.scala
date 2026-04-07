package exastencils.fieldlike.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.prettyprinting.PpStream

object IR_IV_AbstractFieldLikeData {
  def apply(field : IR_FieldLike, slot : IR_Expression, fragmentIdx : IR_Expression) = field.getFieldData(slot, fragmentIdx)
}

// TODO: try to remove "IR_IV_FieldData" occurrences

abstract class IR_IV_AbstractFieldLikeData(
    canBePerFragment : Boolean,
    canBePerDomain : Boolean,
    canBePerField : Boolean,
    canBePerLevel : Boolean,
    canBePerNeighbor : Boolean
) extends IR_InternalVariable(canBePerFragment, canBePerDomain, canBePerField, canBePerLevel, canBePerNeighbor) {

  def field : IR_FieldLike
  var level : IR_Expression
  var slot : IR_Expression
  var fragmentIdx : IR_Expression

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index, level, IR_NullExpression)

  override def usesFieldArrays : Boolean = !Knowledge.data_useFieldNamesAsIdx

  override def resolveDatatype() = {
    if (field.numSlots > 1)
      IR_ArrayDatatype(IR_PointerDatatype(field.resolveDeclType), field.numSlots)
    else
      IR_PointerDatatype(field.resolveDeclType)
  }

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
}
