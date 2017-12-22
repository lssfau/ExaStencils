package exastencils.operator.l1

import exastencils.base.ProgressLocation
import exastencils.knowledge.l1.L1_LeveledKnowledgeAccess
import exastencils.operator.l2.L2_StencilAccess
import exastencils.prettyprinting.PpStream

/// L1_StencilAccess

case class L1_StencilAccess(var target : L1_Stencil) extends L1_LeveledKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << name << '@' << level
  override def progress = ProgressLocation(L2_StencilAccess(target.getProgressedObj(), None, None))
}
