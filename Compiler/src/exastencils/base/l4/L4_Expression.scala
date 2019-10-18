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

import exastencils.base.ProgressLocation
import exastencils.base.ir._
import exastencils.prettyprinting._

/// L4_Expression

trait L4_Expression extends L4_Node with L4_Progressable with PrettyPrintable {
  def progress : IR_Expression

  def +(other : L4_Expression) = L4_Addition(this, other)
  def -(other : L4_Expression) = L4_Subtraction(this, other)
  def *(other : L4_Expression) = L4_Multiplication(this, other)
  def /(other : L4_Expression) = L4_Division(this, other)

  def Pow(other : L4_Expression) = L4_Power(this, other)
  def Mod(other : L4_Expression) = L4_Modulo(this, other)
  def Modulo(other : L4_Expression) = L4_Modulo(this, other)

  // Scala does not allow .{+,-,*,/} and fails with Dot{+,-,*,/}
  def :*(other : L4_Expression) = L4_ElementwiseMultiplication(this, other)
  def :/(other : L4_Expression) = L4_ElementwiseDivision(this, other)
  def :^(other : L4_Expression) = L4_ElementwisePower(this, other)
  def DotPow(other : L4_Expression) = L4_ElementwisePower(this, other)
  def DotMod(other : L4_Expression) = L4_ElementwiseModulo(this, other)
  def DotModulo(other : L4_Expression) = L4_ElementwiseModulo(this, other)

  def And(other : L4_Expression) = L4_AndAnd(this, other)
  def AndAnd(other : L4_Expression) = L4_AndAnd(this, other)
  def Or(other : L4_Expression) = L4_OrOr(this, other)
  def OrOr(other : L4_Expression) = L4_OrOr(this, other)

  def EqEq(other : L4_Expression) = L4_EqEq(this, other)
  def Neq(other : L4_Expression) = L4_Neq(this, other)
  def <(other : L4_Expression) = L4_Lower(this, other)
  def <=(other : L4_Expression) = L4_LowerEqual(this, other)
  def >(other : L4_Expression) = L4_Greater(this, other)
  def >=(other : L4_Expression) = L4_GreaterEqual(this, other)
}

/// L4_NullExpression

case object L4_NullExpression extends L4_Expression {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = {}
  override def progress = ProgressLocation(IR_NullExpression)
}
