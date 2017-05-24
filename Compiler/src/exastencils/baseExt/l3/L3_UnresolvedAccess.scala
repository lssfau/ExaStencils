package exastencils.baseExt.l3

import exastencils.base.l3._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

case class L3_UnresolvedAccess(var name : String, var level : Option[L3_AccessLevelSpecification]) extends L3_Access {
  override def prettyprint(out : PpStream) = {
    out << name
    if (level.isDefined) out << '@' << level.get
  }

  def resolveLevel : Int = {
    level.get match {
      case l : L3_SingleLevel => l.level
      case _                  => Logger.error(s"Trying to get level for access $name, but level is $level")
    }
  }

  override def progress = {
    Logger.warn(s"Trying to progress unresolved L3 Access $name")
    L4_UnresolvedAccess(name, None, L3_ProgressOption(level)(_.progress), None, None, None)
  }
}
