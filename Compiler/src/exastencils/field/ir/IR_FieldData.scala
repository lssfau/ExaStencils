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

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.prettyprinting._

/// IR_IV_AbstractFieldData

abstract class IR_IV_AbstractFieldData extends IR_InternalVariable(true, false, true, true, false) {
  var field : IR_Field
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

  override def getCtor() : Option[IR_Statement] = {
    val origSlot = slot
    slot = "slot"
    val ret = Some(wrapInLoops(IR_Assignment(resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt), resolveDefValue().get)))
    slot = origSlot
    ret
  }

  override def getDtor() : Option[IR_Statement] = {
    val origSlot = slot
    slot = "slot"
    val access = resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt)

    val ret = Some(wrapInLoops(
      IR_IfCondition(access,
        ListBuffer[IR_Statement](
          IR_ArrayFree(access),
          IR_Assignment(access, 0)))))
    slot = origSlot
    ret
  }

  override def resolveAccess(baseAccess : IR_Expression, fragment : IR_Expression, domain : IR_Expression, field : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Expression = {
    var access = super.resolveAccess(baseAccess, fragment, domain, field, level, neigh)
    if (this.field.numSlots > 1)
      access = IR_ArrayAccess(access, slot)
    access
  }
}

/// IR_IV_FieldDataBasePtr

case class IR_IV_FieldDataBasePtr(override var field : IR_Field, override var slot : IR_Expression, override var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_AbstractFieldData {
  override var level : IR_Expression = field.level

  override def resolveName() = (if (1 == field.numSlots) s"fieldData" else "slottedFieldData") +
    resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index.toString, level.prettyprint, "") +
    "_base"
}

/// IR_IV_FieldData

case class IR_IV_FieldData(override var field : IR_Field, override var slot : IR_Expression, override var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_AbstractFieldData {
  def basePtr = IR_IV_FieldDataBasePtr(field, slot, fragmentIdx)

  override var level : IR_Expression = field.level

  override def resolveName() = (if (1 == field.numSlots) s"fieldData" else "slottedFieldData") +
    resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index.toString, level.prettyprint, "")

  override def getDtor() : Option[IR_Statement] = {
    if (Knowledge.data_alignFieldPointers) {
      val origSlot = slot
      slot = "slot"
      val access = resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt)
      val ret = Some(wrapInLoops(IR_Assignment(access, 0)))
      slot = origSlot
      ret
    } else {
      super.getDtor()
    }
  }

  override def registerIV(declarations : HashMap[String, IR_VariableDeclaration], ctors : HashMap[String, IR_Statement], dtors : HashMap[String, IR_Statement]) = {
    declarations += (resolveName -> getDeclaration)
    ctors += (resolveName -> getCtor().get)
    dtors += (resolveName -> getDtor().get)

    if (Knowledge.data_alignFieldPointers)
      basePtr.registerIV(declarations, ctors, dtors)
  }
}
