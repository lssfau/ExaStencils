package exastencils.field.l4

import exastencils.base.l4.L4_Assignment
import exastencils.baseExt.l4._
import exastencils.core.Duplicate
import exastencils.datastructures._

object L4_AddLoopsToFieldAssignments extends DefaultStrategy("Add loop statements around field assignments") {
  this += new Transformation("Add loops", {
    case assignment @ L4_Assignment(lhs : L4_FieldAccess, rhs, op, cond) =>
      val loop = L4_LoopOverField(Duplicate(lhs), assignment)
      loop.condition = cond

      // TODO: add real switch
      if (false)
        L4_LoopOverFragments(loop)
      else
        loop
  }, false /* recursion must be switched of due to wrapping mechanism */)
}
