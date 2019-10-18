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

/// L3_Statement

trait L3_Statement extends L3_Node with L3_Progressable with PrettyPrintable {
  def progress : L4_Statement
}

/// L3_StatementWrapper

// exists to provide convenient way to wrap for applying transformations
case class L3_StatementWrapper(stmt : L3_Statement) extends L3_Node

/// L3_NullStatement

case object L3_NullStatement extends L3_Statement {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = {}
  override def progress = ProgressLocation(L4_NullStatement)
}

/// L3_ExpressionStatement

case class L3_ExpressionStatement(var expression : L3_Expression) extends L3_Statement {
  override def prettyprint(out : PpStream) : Unit = out << expression
  override def progress = ProgressLocation(L4_ExpressionStatement(expression.progress))
}
