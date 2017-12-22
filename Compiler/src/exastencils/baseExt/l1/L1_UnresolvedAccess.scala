package exastencils.baseExt.l1

import exastencils.base.ProgressLocation
import exastencils.base.l1._
import exastencils.baseExt.l2.L2_UnresolvedAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L1_UnresolvedAccess

object L1_UnresolvedAccess {
  def apply(name : String) = new L1_UnresolvedAccess(name, None)
}

case class L1_UnresolvedAccess(
    var name : String,
    var level : Option[L1_AccessLevelSpecification]) extends L1_Access {

  def prettyprint(out : PpStream) = {
    out << name
    if (level.isDefined) out << '@' << level.get
  }

  override def progress = ProgressLocation {
    Logger.warn(s"Progressing unresolved access on L1: $name" + (if (level.isDefined) s"@${ level.get }" else ""))
    L2_UnresolvedAccess(name, L1_ProgressOption(level)(_.progress))
  }
}
