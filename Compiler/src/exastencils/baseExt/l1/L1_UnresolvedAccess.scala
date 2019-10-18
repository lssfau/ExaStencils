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
