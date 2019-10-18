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

/// L2_Expression

trait L2_Expression extends L2_Node with L2_Progressable with PrettyPrintable {
  def progress : L3_Expression

  def +(other : L2_Expression) = L2_Addition(this, other)
  def -(other : L2_Expression) = L2_Subtraction(this, other)
  def *(other : L2_Expression) = L2_Multiplication(this, other)
  def /(other : L2_Expression) = L2_Division(this, other)

  def Pow(other : L2_Expression) = L2_Power(this, other)
  def Mod(other : L2_Expression) = L2_Modulo(this, other)
  def Modulo(other : L2_Expression) = L2_Modulo(this, other)

  // Scala does not allow .{+,-,*,/} and fails with Dot{+,-,*,/}
  def :+(other : L2_Expression) = L2_ElementwiseAddition(this, other)
  def :-(other : L2_Expression) = L2_ElementwiseSubtraction(this, other)
  def :*(other : L2_Expression) = L2_ElementwiseMultiplication(this, other)
  def :/(other : L2_Expression) = L2_ElementwiseDivision(this, other)

  def DotPow(other : L2_Expression) = L2_ElementwisePower(this, other)
  def DotMod(other : L2_Expression) = L2_ElementwiseModulo(this, other)
  def DotModulo(other : L2_Expression) = L2_ElementwiseModulo(this, other)

  def And(other : L2_Expression) = L2_AndAnd(this, other)
  def AndAnd(other : L2_Expression) = L2_AndAnd(this, other)
  def Or(other : L2_Expression) = L2_OrOr(this, other)
  def OrOr(other : L2_Expression) = L2_OrOr(this, other)

  def EqEq(other : L2_Expression) = L2_EqEq(this, other)
  def Neq(other : L2_Expression) = L2_Neq(this, other)
  def <(other : L2_Expression) = L2_Lower(this, other)
  def <=(other : L2_Expression) = L2_LowerEqual(this, other)
  def >(other : L2_Expression) = L2_Greater(this, other)
  def >=(other : L2_Expression) = L2_GreaterEqual(this, other)
}

/// L2_NullExpression

case object L2_NullExpression extends L2_Expression {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = {}
  override def progress = ProgressLocation(L3_NullExpression)
}
