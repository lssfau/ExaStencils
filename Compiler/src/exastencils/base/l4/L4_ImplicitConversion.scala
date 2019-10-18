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

object L4_ImplicitConversion {

  import scala.language.implicitConversions

  // constants

  implicit def NumberToIntegerConstant(n : Int) : L4_IntegerConstant = L4_IntegerConstant(n)
  implicit def NumberToIntegerConstant(n : Long) : L4_IntegerConstant = L4_IntegerConstant(n)
  implicit def NumberToFloatConstant(n : Float) : L4_RealConstant = L4_RealConstant(n)
  implicit def NumberToFloatConstant(n : Double) : L4_RealConstant = L4_RealConstant(n)

  implicit def BooleanToBooleanConstant(b : Boolean) : L4_BooleanConstant = L4_BooleanConstant(b)

  implicit def StringToStringLiteral(s : String) : L4_StringLiteral = L4_StringLiteral(s)

  // expression -> statement

  implicit def ExpressionToExpressionStatement(e : L4_Expression) : L4_Statement = L4_ExpressionStatement(e)

  // datatype

  implicit def StringToDatatype(s : String) : L4_Datatype = L4_SpecialDatatype(s)
}
