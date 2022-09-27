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
import exastencils.datastructures._
import exastencils.prettyprinting._

/// L2_ConstantExpression

trait L2_ConstantExpression extends L2_Expression

/// L2_Number
trait L2_Number extends L2_ConstantExpression {
  def value : AnyVal
}

/// L2_StringLiteral

case class L2_StringLiteral(var value : String) extends L2_ConstantExpression {
  override def prettyprint(out : PpStream) : Unit = out << value
  override def toString : String = value
  override def progress = ProgressLocation(L3_StringLiteral(value))
}

/// L2_StringConstant

case class L2_StringConstant(var value : String) extends L2_ConstantExpression {
  // swap quotation characters for second parse
  def q = if (value.contains("\"")) "\'" else "\""
  override def prettyprint(out : PpStream) : Unit = out << q << value << q
  override def progress = ProgressLocation(L3_StringConstant(value))
}

/// L2_IntegerConstant
case class L2_IntegerConstant(var v : Long) extends L2_Number {
  override def prettyprint(out : PpStream) : Unit = out << v
  override def value = v
  override def progress = ProgressLocation(L3_IntegerConstant(value))
}

/// L2_RealConstant
case class L2_RealConstant(var v : Double) extends L2_Number {
  override def prettyprint(out : PpStream) : Unit = {
    out << value // this uses value.toString(), which is Locale-independent and the string can be parsed without a loss of precision later
  }
  override def value = v
  override def progress = ProgressLocation(L3_RealConstant(value))
}

/// L2_BooleanConstant

case class L2_BooleanConstant(var value : Boolean) extends L2_ConstantExpression {
  override def prettyprint(out : PpStream) : Unit = out << value
  override def progress = ProgressLocation(L3_BooleanConstant(value))
}

// L2_ConvertStringConstantsToLiterals

object L2_ConvertStringConstantsToLiterals extends QuietDefaultStrategy("Convert string constants to literals") {
  this += new Transformation("Convert", {
    case const : L2_StringConstant => L2_StringLiteral(const.value)
  })
}

object L2_ConvertStringLiteralsToConstants extends QuietDefaultStrategy("Convert string literals to constants") {
  this += new Transformation("Convert", {
    case const : L2_StringLiteral => L2_StringConstant(const.value)
  })
}
