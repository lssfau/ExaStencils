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

package exastencils.base.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir._
import exastencils.prettyprinting._

/// L4_Return

case class L4_Return(var expr : Option[L4_Expression]) extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    out << "return"
    if (expr.isDefined) out << ' ' << expr.get.prettyprint()
  }

  override def progress = ProgressLocation(IR_Return(L4_ProgressOption(expr)(_.progress)))
}
