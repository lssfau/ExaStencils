//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

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
    val access = super.resolveAccess(baseAccess, fragment, domain, field, level, neigh)
    if (this.field.numSlots > 1) IR_ArrayAccess(access, slot) else access
  }

  override def resolveDatatype() = {
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
    val ret = Some(wrapInLoops(IR_Assignment(resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt), resolveDefValue().get)))
    slot = origSlot
    ret
  }
}
