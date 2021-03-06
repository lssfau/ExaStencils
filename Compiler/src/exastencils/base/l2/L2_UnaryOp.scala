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

/// L2_UnaryOperators

object L2_UnaryOperators extends Enumeration {
  exastencils.core.Duplicate.registerConstant(this)

  type UnaryOperators = Value
  val Negative = Value("-")
  val Not = Value("!")

  def createExpression(op : String, exp : L2_Expression) : L2_Expression = createExpression(withName(op), exp)
  def createExpression(op : Value, exp : L2_Expression) : L2_Expression = op match {
    case Negative => L2_Negative(exp)
    case Not      => L2_Negation(exp)
  }
}

/// arithmetic operations

case class L2_Negative(var left : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << "(-" << left << ')'
  override def progress = ProgressLocation(L3_Negative(left.progress))
}

/// logical operations

case class L2_Negation(var left : L2_Expression) extends L2_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '!' << '(' << left << ')'
  override def progress = ProgressLocation(L3_Negation(left.progress))
}
