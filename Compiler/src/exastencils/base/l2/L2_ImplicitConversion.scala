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

object L2_ImplicitConversion {

  import scala.language.implicitConversions

  // constants

  implicit def NumberToIntegerConstant(n : Int) : L2_IntegerConstant = L2_IntegerConstant(n)
  implicit def NumberToIntegerConstant(n : Long) : L2_IntegerConstant = L2_IntegerConstant(n)
  implicit def NumberToFloatConstant(n : Float) : L2_RealConstant = L2_RealConstant(n)
  implicit def NumberToFloatConstant(n : Double) : L2_RealConstant = L2_RealConstant(n)

  implicit def BooleanToBooleanConstant(b : Boolean) : L2_BooleanConstant = L2_BooleanConstant(b)

  implicit def StringToStringLiteral(s : String) : L2_StringLiteral = L2_StringLiteral(s)

  // expression -> statement

  implicit def ExpressionToExpressionStatement(e : L2_Expression) : L2_Statement = L2_ExpressionStatement(e)

  // datatype

  implicit def StringToDatatype(s : String) : L2_Datatype = L2_SpecialDatatype(s)
}
