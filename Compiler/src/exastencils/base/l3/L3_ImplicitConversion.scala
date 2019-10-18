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

object L3_ImplicitConversion {

  import scala.language.implicitConversions

  // constants

  implicit def NumberToIntegerConstant(n : Int) : L3_IntegerConstant = L3_IntegerConstant(n)
  implicit def NumberToIntegerConstant(n : Long) : L3_IntegerConstant = L3_IntegerConstant(n)
  implicit def NumberToFloatConstant(n : Float) : L3_RealConstant = L3_RealConstant(n)
  implicit def NumberToFloatConstant(n : Double) : L3_RealConstant = L3_RealConstant(n)

  implicit def BooleanToBooleanConstant(b : Boolean) : L3_BooleanConstant = L3_BooleanConstant(b)

  implicit def StringToStringLiteral(s : String) : L3_StringLiteral = L3_StringLiteral(s)

  // expression -> statement

  implicit def ExpressionToExpressionStatement(e : L3_Expression) : L3_Statement = L3_ExpressionStatement(e)

  // datatype

  implicit def StringToDatatype(s : String) : L3_Datatype = L3_SpecialDatatype(s)
}
