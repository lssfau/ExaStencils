package exastencils.solver.l4

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.knowledge.l4.L4_LeveledKnowledgeAccess
import exastencils.prettyprinting.PpStream
import exastencils.solver.ir.IR_EquationAccess

/// L4_EquationAccess

object L4_EquationAccess {
  def apply(access : L4_FutureEquationAccess) =
    new L4_EquationAccess(L4_EquationCollection.getByIdentifier(access.name, access.level).get, access.offset)
}

case class L4_EquationAccess(
    var target : L4_NamedEquation,
    var offset : Option[L4_ConstIndex] = None) extends L4_LeveledKnowledgeAccess with L4_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    out << target.name << '@' << target.level
    if (offset.isDefined) out << '@' << offset.get
  }

  override def offsetWith(newOffset : L4_ConstIndex) = {
    if (offset.isEmpty)
      offset = Some(newOffset)
    else
      offset = Some(offset.get + newOffset)
  }

  override def progress = ProgressLocation(IR_EquationAccess(target.getProgressedObj(), L4_ProgressOption(offset)(_.progress)))
}

/// L4_ResolveEquationAccesses

object L4_ResolveEquationAccesses extends DefaultStrategy("Resolve accesses to equations") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L4_FutureEquationAccess if L4_EquationCollection.exists(access.name, access.level) =>
      access.toEquationAccess
  })
}
