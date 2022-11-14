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

package exastencils.base.l3

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.prettyprinting._

/// L3_ConstantExpression

trait L3_ConstantExpression extends L3_Expression

/// L3_Number
trait L3_Number extends L3_ConstantExpression {
  def value : AnyVal
}

/// L3_StringLiteral

case class L3_StringLiteral(var value : String) extends L3_ConstantExpression {
  override def prettyprint(out : PpStream) : Unit = out << value
  override def toString : String = value
  override def progress = ProgressLocation(L4_StringLiteral(value))
}

/// L3_StringConstant

case class L3_StringConstant(var value : String) extends L3_ConstantExpression {
  // swap quotation characters for second parse
  def q = if (value.contains("\"")) "\'" else "\""
  override def prettyprint(out : PpStream) : Unit = out << q << value << q
  override def progress = ProgressLocation(L4_StringConstant(value))
}

/// L3_IntegerConstant
case class L3_IntegerConstant(var v : Long) extends L3_Number {
  override def prettyprint(out : PpStream) : Unit = out << v
  override def value = v
  override def progress = ProgressLocation(L4_IntegerConstant(value))
}

/// L3_RealConstant
case class L3_RealConstant(var v : Double) extends L3_Number {
  override def prettyprint(out : PpStream) : Unit = {
    out << value // this uses value.toString(), which is Locale-independent and the string can be parsed without a loss of precision later
  }
  override def value = v
  override def progress = ProgressLocation(L4_RealConstant(value))
}

/// L3_BooleanConstant

case class L3_BooleanConstant(var value : Boolean) extends L3_ConstantExpression {
  override def prettyprint(out : PpStream) : Unit = out << value
  override def progress = ProgressLocation(L4_BooleanConstant(value))
}

// L3_ConvertStringConstantsToLiterals

object L3_ConvertStringConstantsToLiterals extends QuietDefaultStrategy("Convert string constants to literals") {
  this += new Transformation("Convert", {
    case const : L3_StringConstant => L3_StringLiteral(const.value)
  })
}

object L3_ConvertStringLiteralsToConstants extends QuietDefaultStrategy("Convert string literals to constants") {
  this += new Transformation("Convert", {
    case const : L3_StringLiteral => L3_StringConstant(const.value)
  })
}
