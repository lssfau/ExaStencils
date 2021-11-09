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
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.parallelization.ir.IR_ParallelizationInfo
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_StackCollector

/// IR_IV_ActiveSlot

case class IR_IV_ActiveSlot(var field : IR_FieldLike, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, true, true, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index, field.level, IR_NullExpression)

  override def usesFieldArrays : Boolean = !Knowledge.data_useFieldNamesAsIdx

  override def resolveName() = s"currentSlot" + resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index.toString, field.level.toString, "")
  override def resolveDatatype() = "int"
  override def resolveDefValue() = Some(IR_IntegerConstant(0))
}

/// IR_SlotAccess

case class IR_SlotAccess(var slot : IR_IV_ActiveSlot, var offset : Int) extends IR_Expression with IR_SpecialExpandable {
  // ensure: 0 <= offset < slot.field.numSlots
  offset %= slot.field.numSlots
  if (offset < 0)
    offset += slot.field.numSlots

  override def datatype = IR_UnitDatatype

  // offset is always positive -> Mod is safe
  def expandSpecial() = (slot + offset) Mod slot.field.numSlots
}

/// IR_AdvanceSlot

case class IR_AdvanceSlot(var slot : IR_IV_ActiveSlot, var step : Int = 1) extends IR_Statement with IR_SpecialExpandable {
  if (step < 1)
    Logger.error("advance slot with a negative step size is not supported")
  // slot never contains negative values (currently)
  def expandSpecial() = IR_Assignment(slot, (slot + step) Mod slot.field.numSlots)
}

/// IR_ResolveSlotOperations

object IR_ResolveSlotOperations extends DefaultStrategy("Resolve slot operations") {
  var collector = new IR_StackCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve", {
    case slotAccess : IR_SlotAccess => slotAccess.expandSpecial()

    case advanceSlot : IR_AdvanceSlot =>
      // check if already inside a fragment loop - if not wrap the expanded statement
      if (collector.stack.map {
        case _ : IR_LoopOverFragments                                                                             => true
        case IR_ForLoop(IR_VariableDeclaration(_, it, _, _), _, _, _, _) if IR_LoopOverFragments.defIt.name == it => true
        case _                                                                                                    => false
      }.fold(false)((a, b) => a || b))
        advanceSlot.expandSpecial()
      else
        IR_LoopOverFragments(advanceSlot.expandSpecial(), IR_ParallelizationInfo(potentiallyParallel = true))
  })
}
