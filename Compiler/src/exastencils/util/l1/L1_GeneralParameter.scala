package exastencils.util.l1

import exastencils.config.Knowledge
import exastencils.core.UniversalSetter
import exastencils.logger.Logger

trait L1_GeneralParameter {
  def name : String
  def value : Any

  def printVal() = {
    value match {
      case c : Char   => s"'$c'"
      case s : String => '"' + s + '"'
      case other      => other
    }
  }

  def set() = {
    // TODO: other collections required?
    try {
      UniversalSetter(Knowledge, name, value)
    } catch {
      case _ : java.lang.NoSuchFieldException     => Logger.error(s"Trying to set parameter Knowledge.${ name } to ${ value } but this parameter is undefined")
      case _ : java.lang.IllegalArgumentException => Logger.error(s"Trying to set parameter Knowledge.${ name } to ${ value } but data types are incompatible")
    }
  }
}