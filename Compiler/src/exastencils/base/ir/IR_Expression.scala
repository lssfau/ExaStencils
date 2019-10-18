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

/// IR_Expression

trait IR_Expression extends IR_Node with PrettyPrintable {
  def datatype : IR_Datatype

  def +(other : IR_Expression) = IR_Addition(this, other)
  def -(other : IR_Expression) = IR_Subtraction(this, other)
  def *(other : IR_Expression) = IR_Multiplication(this, other)
  def /(other : IR_Expression) = IR_Division(this, other)

  def Pow(other : IR_Expression) = IR_Power(this, other)
  def Mod(other : IR_Expression) = IR_Modulo(this, other)
  def Modulo(other : IR_Expression) = IR_Modulo(this, other)

  // Scala does not allow .{+,-,*,/} and fails with Dot{+,-,*,/}
  def :+(other : IR_Expression) = IR_ElementwiseAddition(this, other)
  def :-(other : IR_Expression) = IR_ElementwiseSubtraction(this, other)
  def :*(other : IR_Expression) = IR_ElementwiseMultiplication(this, other)
  def :/(other : IR_Expression) = IR_ElementwiseDivision(this, other)

  def DotPow(other : IR_Expression) = IR_ElementwisePower(this, other)
  def DotMod(other : IR_Expression) = IR_ElementwiseModulo(this, other)
  def DotModulo(other : IR_Expression) = IR_ElementwiseModulo(this, other)

  def And(other : IR_Expression) = IR_AndAnd(this, other)
  def AndAnd(other : IR_Expression) = IR_AndAnd(this, other)
  def Or(other : IR_Expression) = IR_OrOr(this, other)
  def OrOr(other : IR_Expression) = IR_OrOr(this, other)

  def EqEq(other : IR_Expression) = IR_EqEq(this, other)
  def Neq(other : IR_Expression) = IR_Neq(this, other)
  def <(other : IR_Expression) = IR_Lower(this, other)
  def <=(other : IR_Expression) = IR_LowerEqual(this, other)
  def >(other : IR_Expression) = IR_Greater(this, other)
  def >=(other : IR_Expression) = IR_GreaterEqual(this, other)

  def <<(other : IR_Expression) = IR_LeftShift(this, other)
}

/// IR_NullExpression

case object IR_NullExpression extends IR_Expression {
  exastencils.core.Duplicate.registerConstant(this)
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = {}
}
