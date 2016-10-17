package exastencils.field.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._

/// IR_IV_FieldFlag

abstract class IR_IV_FieldFlag extends IR_InternalVariable(true, false, true, true, false) {
  var field : IR_Field
  var slot : IR_Expression
  var fragmentIdx : IR_Expression

  override def resolveAccess(baseAccess : IR_Expression, fragment : IR_Expression, domain : IR_Expression, field : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Expression = {
    val access = if (this.field.numSlots > 1) IR_ArrayAccess(baseAccess, slot) else baseAccess
    super.resolveAccess(access, fragment, domain, field, level, neigh)
  }

  override def resolveDatatype = {
    if (field.numSlots > 1)
      IR_ArrayDatatype(IR_BooleanDatatype, field.numSlots)
    else
      IR_BooleanDatatype
  }

  override def wrapInLoops(body : IR_Statement) : IR_Statement = {
    var wrappedBody = super.wrapInLoops(body)
    if (field.numSlots > 1)
      wrappedBody = IR_ForLoop(
        IR_VariableDeclaration(IR_IntegerDatatype, "slot", 0),
        IR_Lower("slot", field.numSlots),
        IR_PreIncrement("slot"),
        wrappedBody)
    wrappedBody
  }

  override def getCtor() : Option[IR_Statement] = {
    val origSlot = slot
    slot = "slot"
    val ret = Some(wrapInLoops(IR_Assignment(resolveAccess(resolveName, IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt), resolveDefValue.get)))
    slot = origSlot
    ret
  }
}
