package exastencils.solver.l3

import exastencils.base.l3._
import exastencils.datastructures._
import exastencils.knowledge.l3.L3_LeveledKnowledgeAccess
import exastencils.prettyprinting.PpStream
import exastencils.solver.l4.L4_EquationAccess

/// L3_EquationAccess

object L3_EquationAccess {
  def apply(access : L3_FutureEquationAccess) =
    new L3_EquationAccess(L3_EquationCollection.getByIdentifier(access.name, access.level).get, access.offset)
}

case class L3_EquationAccess(
    var target : L3_NamedEquation,
    var offset : Option[L3_ConstIndex] = None) extends L3_LeveledKnowledgeAccess with L3_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    out << target.name << '@' << target.level
    if (offset.isDefined) out << '@' << offset.get
  }

  override def offsetWith(newOffset : L3_ConstIndex) = {
    if (offset.isEmpty)
      offset = Some(newOffset)
    else
      offset = Some(offset.get + newOffset)
  }

  override def progress = L4_EquationAccess(target.getProgressedObj(), L3_ProgressOption(offset)(_.progress))
}

/// L3_ResolveEquationAccesses

object L3_ResolveEquationAccesses extends DefaultStrategy("Resolve accesses to equations") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L3_FutureEquationAccess if L3_EquationCollection.exists(access.name, access.level) =>
      access.toEquationAccess
  })
}
