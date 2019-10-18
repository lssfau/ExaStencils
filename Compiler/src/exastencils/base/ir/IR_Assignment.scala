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

package exastencils.base.ir

import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// IR_Assignment

// TODO: use specialized compound assignment and eliminate op member
case class IR_Assignment(var dest : IR_Expression, var src : IR_Expression, var op : String = "=") extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << dest << ' ' << op << ' ' << src << ';'
}

/// IR_CompoundAssignment

case class IR_CompoundAssignment(var dest : IR_Expression, var src : IR_Expression, var op : IR_BinaryOperators.BinaryOperators) extends IR_Statement {
  Logger.warn("Not fully incorporated yet")
  override def prettyprint(out : PpStream) : Unit = out << dest << ' ' << op << "=" << ' ' << src << ';'
}
