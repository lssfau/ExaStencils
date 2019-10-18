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
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L3_Assignment

object L3_Assignment {
  def apply(dest : L3_Access, src : L3_Expression) = new L3_Assignment(dest, src, "=", None)
}

// TODO: use specialized compound assignment and eliminate op member
case class L3_Assignment(var dest : L3_Access, var src : L3_Expression, var op : String, var condition : Option[L3_Expression]) extends L3_Statement {
  override def prettyprint(out : PpStream) = {
    out << dest << ' ' << op << ' ' << src
    if (condition.isDefined) out << " where " << condition.get
  }

  override def progress = ProgressLocation(L4_Assignment(dest.progress, src.progress, op, L3_ProgressOption(condition)(_.progress)))
}

/// L3_CompoundAssignment

case class L3_CompoundAssignment(var dest : L3_Expression, var src : L3_Expression, var op : L3_BinaryOperators.BinaryOperators, var condition : Option[L3_Expression]) extends L3_Statement {
  Logger.warn("Not fully incorporated yet")
  override def prettyprint(out : PpStream) = {
    out << dest << ' ' << op << "=" << ' ' << src
    if (condition.isDefined) out << " where " << condition.get
  }

  override def progress = ProgressLocation(L4_CompoundAssignment(dest.progress, src.progress, L3_BinaryOperators.progress(op), L3_ProgressOption(condition)(_.progress)))
}
