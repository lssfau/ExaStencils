package exastencils.field.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.core._
import exastencils.datastructures._
import exastencils.logger.Logger

@deprecated("to be deleted", "12.10.17")
object L4_IntroduceSlots extends DefaultStrategy("Add slots to fields and field accesses") {
  this += new Transformation("Introduce slots", {
    // target assignments to fields
    case assignment @ L4_Assignment(lhs : L4_FieldAccess, rhs, op, cond) =>
      // TODO: support conditions
      if (cond.isDefined) Logger.warn("Found potentially slotted assignment with condition - resolution is currently unsupported")

      // check if the same field is accessed in the rhs of the assignment
      if (cond.isEmpty && StateManager.findFirst({ access : L4_FieldAccess => access.target == lhs.target }, L4_ExpressionStatement(rhs)).isDefined) {
        // destination field is used in update expression -> introduce slots
        // TODO: find better strategy to determine number of slots required
        lhs.target.numSlots = 2

        // pre-store advance statements before altering lhs
        val advance = L4_AdvanceSlot(Duplicate(lhs))

        // handle lhs
        lhs.slot = L4_NextSlot

        // handle rhs
        L4_SetActiveSlotForFieldAccess.targetField = lhs.target
        L4_SetActiveSlotForFieldAccess.applyStandalone(L4_ExpressionStatement(rhs))

        // return original statement plus advance statement
        ListBuffer(assignment, advance)
      } else {
        // no slots necessary -> return assignment unchanged
        assignment
      }
  })

  /// L4_SetNextSlotOnFieldAccess

  object L4_SetActiveSlotForFieldAccess extends QuietDefaultStrategy("Set active slot modifier for field accesses") {
    var targetField : L4_Field = _

    this += new Transformation("Set modifier", {
      case access : L4_FieldAccess if targetField == access.target =>
        access.slot = L4_ActiveSlot
        access
    })
  }

}
