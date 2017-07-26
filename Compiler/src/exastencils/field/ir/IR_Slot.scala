package exastencils.field.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.parallelization.ir.IR_ParallelizationInfo
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_StackCollector

/// IR_IV_ActiveSlot

case class IR_IV_ActiveSlot(var field : IR_Field, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, true, true, false) {
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

case class IR_AdvanceSlot(var slot : IR_IV_ActiveSlot) extends IR_Statement with IR_SpecialExpandable {
  // slot never contains negative values (currently)
  def expandSpecial() = IR_Assignment(slot, (slot + 1) Mod slot.field.numSlots)
}

/// IR_ResolveSlotOperations

object IR_ResolveSlotOperations extends DefaultStrategy("Resolve slot operations") {
  var collector = new IR_StackCollector
  this.register(collector)

  this += new Transformation("Resolve", {
    case slotAccess : IR_SlotAccess => slotAccess.expandSpecial()

    case advanceSlot : IR_AdvanceSlot =>
      // check if already inside a fragment loop - if not wrap the expanded statement
      if (collector.stack.map {
        case _ : IR_LoopOverFragments                                                                     => true
        case IR_ForLoop(IR_VariableDeclaration(_, it, _), _, _, _, _) if IR_LoopOverFragments.defIt == it => true
        case _                                                                                            => false
      }.fold(false)((a, b) => a || b))
        advanceSlot.expandSpecial()
      else
        IR_LoopOverFragments(advanceSlot.expandSpecial(), IR_ParallelizationInfo.PotentiallyParallel())
  })
}
