package exastencils.solver.l2

import exastencils.base.l2._
import exastencils.datastructures._
import exastencils.knowledge.l2.L2_LeveledKnowledgeAccess
import exastencils.prettyprinting.PpStream
import exastencils.solver.l3.L3_EquationAccess

/// L2_EquationAccess

object L2_EquationAccess {
  def apply(access : L2_FutureEquationAccess) =
    new L2_EquationAccess(L2_EquationCollection.getByIdentifier(access.name, access.level).get, access.offset)
}

case class L2_EquationAccess(
    var target : L2_NamedEquation,
    var offset : Option[L2_ConstIndex] = None) extends L2_LeveledKnowledgeAccess with L2_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    out << target.name << '@' << target.level
    if (offset.isDefined) out << '@' << offset.get
  }

  override def offsetWith(newOffset : L2_ConstIndex) = {
    if (offset.isEmpty)
      offset = Some(newOffset)
    else
      offset = Some(offset.get + newOffset)
  }

  override def progress = L3_EquationAccess(target.getProgressedObj(), L2_ProgressOption(offset)(_.progress))
}

/// L2_ResolveEquationAccesses

object L2_ResolveEquationAccesses extends DefaultStrategy("Resolve accesses to equations") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L2_FutureEquationAccess if L2_EquationCollection.exists(access.name, access.level) =>
      access.toEquationAccess
  })
}
