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

object L1_ImplicitConversion {

  import scala.language.implicitConversions

  // constants

  implicit def NumberToIntegerConstant(n : Int) : L1_IntegerConstant = L1_IntegerConstant(n)
  implicit def NumberToIntegerConstant(n : Long) : L1_IntegerConstant = L1_IntegerConstant(n)
  implicit def NumberToFloatConstant(n : Float) : L1_RealConstant = L1_RealConstant(n)
  implicit def NumberToFloatConstant(n : Double) : L1_RealConstant = L1_RealConstant(n)

  implicit def BooleanToBooleanConstant(b : Boolean) : L1_BooleanConstant = L1_BooleanConstant(b)

  implicit def StringToStringLiteral(s : String) : L1_StringLiteral = L1_StringLiteral(s)

  // expression -> statement

  implicit def ExpressionToExpressionStatement(e : L1_Expression) : L1_Statement = L1_ExpressionStatement(e)

  // datatype

  implicit def StringToDatatype(s : String) : L1_Datatype = L1_SpecialDatatype(s)
}
