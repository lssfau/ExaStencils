package exastencils.baseExt.l2

import exastencils.base.l2._
import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

case class L2_UnresolvedAccess(var name : String, var level : Option[L2_AccessLevelSpecification]) extends L2_Access {
  override def prettyprint(out : PpStream) = {
    out << name
    if (level.isDefined) out << '@' << level.get
  }

  def resolveLevel : Int = {
    level.get match {
      case l : L2_SingleLevel => l.level
      case _                  => Logger.error(s"Trying to get level for access $name, but level is $level")
    }
  }

  override def progress = {
    Logger.warn(s"Trying to progress unresolved l2 Access $name")
    L3_UnresolvedAccess(name, L2_ProgressOption(level)(_.progress))
  }
}
