package exastencils.solver.l1

import exastencils.base.ProgressLocation
import exastencils.datastructures._
import exastencils.knowledge.l1.L1_LeveledKnowledgeAccess
import exastencils.prettyprinting.PpStream
import exastencils.solver.l2.L2_EquationAccess

/// L1_EquationAccess

object L1_EquationAccess {
  def apply(access : L1_FutureEquationAccess) =
    new L1_EquationAccess(L1_EquationCollection.getByIdentifier(access.name, access.level).get)
}

case class L1_EquationAccess(var target : L1_NamedEquation) extends L1_LeveledKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << target.name << '@' << target.level
  override def progress = ProgressLocation(L2_EquationAccess(target.getProgressedObj(), None))
}

/// L1_ResolveEquationAccesses

object L1_ResolveEquationAccesses extends DefaultStrategy("Resolve accesses to equations") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L1_FutureEquationAccess if L1_EquationCollection.exists(access.name, access.level) =>
      access.toEquationAccess
  })
}
