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

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.prettyprinting._

/// L1_UnaryOperators

object L1_UnaryOperators extends Enumeration {
  exastencils.core.Duplicate.registerConstant(this)

  type UnaryOperators = Value
  val Negative = Value("-")
  val Not = Value("!")

  def createExpression(op : String, exp : L1_Expression) : L1_Expression = createExpression(withName(op), exp)
  def createExpression(op : Value, exp : L1_Expression) : L1_Expression = op match {
    case Negative => L1_Negative(exp)
    case Not      => L1_Negation(exp)
  }
}

/// arithmetic operations

case class L1_Negative(var left : L1_Expression) extends L1_Expression {
  override def prettyprint(out : PpStream) : Unit = out << "(-" << left << ')'
  override def progress = ProgressLocation(L2_Negative(left.progress))
}

/// logical operations

case class L1_Negation(var left : L1_Expression) extends L1_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '!' << '(' << left << ')'
  override def progress = ProgressLocation(L2_Negation(left.progress))
}
