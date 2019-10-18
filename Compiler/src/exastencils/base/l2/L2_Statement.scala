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

package exastencils.base.l2

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.prettyprinting._

/// L2_Statement

trait L2_Statement extends L2_Node with L2_Progressable with PrettyPrintable {
  def progress : L3_Statement
}

/// L2_StatementWrapper

// exists to provide convenient way to wrap for applying transformations
case class L2_StatementWrapper(stmt : L2_Statement) extends L2_Node

/// L2_NullStatement

case object L2_NullStatement extends L2_Statement {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = {}
  override def progress = ProgressLocation(L3_NullStatement)
}

/// L2_ExpressionStatement

case class L2_ExpressionStatement(var expression : L2_Expression) extends L2_Statement {
  override def prettyprint(out : PpStream) : Unit = out << expression
  override def progress = ProgressLocation(L3_ExpressionStatement(expression.progress))
}
