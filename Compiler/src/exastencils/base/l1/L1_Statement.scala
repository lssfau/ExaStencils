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

package exastencils.base.l1

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.prettyprinting._

/// L1_Statement

trait L1_Statement extends L1_Node with L1_Progressable with PrettyPrintable {
  def progress : L2_Statement
}

/// L1_StatementWrapper

// exists to provide convenient way to wrap for applying transformations
case class L1_StatementWrapper(stmt : L1_Statement) extends L1_Node

/// L1_NullStatement

case object L1_NullStatement extends L1_Statement {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = {}
  override def progress = ProgressLocation(L2_NullStatement)
}

/// L1_ExpressionStatement

case class L1_ExpressionStatement(var expression : L1_Expression) extends L1_Statement {
  override def prettyprint(out : PpStream) : Unit = out << expression
  override def progress = ProgressLocation(L2_ExpressionStatement(expression.progress))
}
