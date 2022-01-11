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

object IR_UnaryOperators extends Enumeration {
  exastencils.core.Duplicate.registerConstant(this)

  type UnaryOperators = Value

  val Negative = Value("-")
  val Not = Value("!")

  val AddressOf = Value("&")

  def createExpression(op : String, exp : IR_Expression) : IR_Expression = createExpression(withName(op), exp)
  def createExpression(op : Value, exp : IR_Expression) : IR_Expression = op match {
    case Negative => IR_Negative(exp)
    case Not      => IR_Negation(exp)
  }
}

/// arithmetic operations

case class IR_Negative(var left : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << "(-" << left << ')'
}

/// logical operations

case class IR_Negation(var left : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << '!' << '(' << left << ')'
}

/// increment and decrement operations

case class IR_PreDecrement(var left : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << "(--" << left << ')'
}

case class IR_PostDecrement(var left : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "--)"
}

case class IR_PreIncrement(var left : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << "(++" << left << ')'
}

case class IR_PostIncrement(var left : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << '(' << left << "++)"
}

/// other operations

case class IR_AddressOf(var left : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << "(&(" << left << "))"
}

// bitwise operations

case class IR_BitwiseNot(var left : IR_Expression) extends IR_Expression {
  override def datatype = left.datatype
  override def prettyprint(out : PpStream) : Unit = out << "(~(" << left << "))"
}
