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

import scala.collection.mutable.ListBuffer

import exastencils.prettyprinting.PpStream

/// IR_FunctionCall

object IR_FunctionCall {
  def apply(function : IR_FunctionReference, args : IR_Expression*) = new IR_FunctionCall(function, args.to[ListBuffer])

  @deprecated("Used for backwards compatibility - to be removed", "22.09.16")
  def apply(functionName : String, args : IR_Expression*)
  = new IR_FunctionCall(IR_UnresolvedFunctionReference(functionName, IR_UnitDatatype), args.to[ListBuffer])
  @deprecated("Used for backwards compatibility - to be removed", "22.09.16")
  def apply(functionName : String, args : ListBuffer[IR_Expression])
  = new IR_FunctionCall(IR_UnresolvedFunctionReference(functionName, IR_UnitDatatype), args)
}

case class IR_FunctionCall(var function : IR_FunctionReference, var arguments : ListBuffer[IR_Expression]) extends IR_Expression {
  def name = function.name
  override def datatype = function.returnType
  override def prettyprint(out : PpStream) = out << function << '(' <<< (arguments, ", ") << ')'
}
