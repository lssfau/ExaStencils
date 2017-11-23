package exastencils.solver.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.config.Knowledge
import exastencils.field.l3._

/// L3_GenerateSmootherHint

object L3_GenerateSmootherHint {
  def apply(loopBase : Option[L3_Access], solveFor : Option[List[L3_Access]]) = new L3_GenerateSmootherHint(loopBase, solveFor.getOrElse(List()).to[ListBuffer])
}

case class L3_GenerateSmootherHint(var loopBase : Option[L3_Access], var solveFor : ListBuffer[L3_Access]) extends L3_Node {
  loopBase = loopBase.map(access => {
    val unresolved = access.asInstanceOf[L3_UnresolvedAccess]
    L3_FutureFieldAccess(unresolved.name, Knowledge.maxLevel/*FIXME*/ , unresolved.slot.getOrElse(L3_ActiveSlot), unresolved.offset)
  })
  solveFor.transform(access => {
    val unresolved = access.asInstanceOf[L3_UnresolvedAccess]
    L3_FutureFieldAccess(unresolved.name, Knowledge.maxLevel/*FIXME*/ , unresolved.slot.getOrElse(L3_ActiveSlot), unresolved.offset)
  })
}
