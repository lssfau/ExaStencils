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

package exastencils.base.l3

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.prettyprinting._

/// L3_Return

case class L3_Return(var expr : Option[L3_Expression]) extends L3_Statement {
  override def prettyprint(out : PpStream) = {
    out << "return"
    if (expr.isDefined) out << ' ' << expr.get.prettyprint()
  }

  override def progress = ProgressLocation(L4_Return(L3_ProgressOption(expr)(_.progress)))
}
