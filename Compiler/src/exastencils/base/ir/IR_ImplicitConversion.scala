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

object IR_ImplicitConversion {

  import scala.language.implicitConversions

  // constants

  implicit def NumberToIntegerConstant(n : Int) : IR_IntegerConstant = IR_IntegerConstant(n)
  implicit def NumberToIntegerConstant(n : Long) : IR_IntegerConstant = IR_IntegerConstant(n)
  implicit def NumberToFloatConstant(n : Float) : IR_RealConstant = IR_RealConstant(n)
  implicit def NumberToFloatConstant(n : Double) : IR_RealConstant = IR_RealConstant(n)

  implicit def BooleanToBooleanConstant(b : Boolean) : IR_BooleanConstant = IR_BooleanConstant(b)

  implicit def StringToStringLiteral(s : String) : IR_StringLiteral = IR_StringLiteral(s)

  // expression -> statement

  implicit def ExpressionToExpressionStatement(e : IR_Expression) : IR_Statement = IR_ExpressionStatement(e)

  // datatype

  implicit def StringToDatatype(s : String) : IR_Datatype = IR_SpecialDatatype(s)

  // deprecated

  @deprecated("should be removed completely; please, don't use it in new code", "15.09.2016")
  implicit def StringToStatement(s : String) : IR_Statement = IR_ExpressionStatement(IR_StringLiteral(s))
}
