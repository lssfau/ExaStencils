//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.util.ir

import exastencils.config.Knowledge
import exastencils.core.UniversalSetter
import exastencils.logger.Logger

trait IR_GeneralParameter {
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
