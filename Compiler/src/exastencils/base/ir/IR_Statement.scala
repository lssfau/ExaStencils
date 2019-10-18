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

import exastencils.prettyprinting._

/// IR_Statement

trait IR_Statement extends IR_Node with PrettyPrintable

/// IR_ScopedStatement

// used as a marker to simplify unnecessary nesting of scopes
trait IR_ScopedStatement extends IR_Statement

/// IR_StatementWrapper

// exists to provide convenient way to wrap for applying transformations
case class IR_StatementWrapper(stmt : IR_Statement) extends IR_Node

/// IR_NullStatement

case object IR_NullStatement extends IR_Statement {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = {}
}

/// IR_ExpressionStatement

case class IR_ExpressionStatement(var expression : IR_Expression) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << expression << ';'
}
