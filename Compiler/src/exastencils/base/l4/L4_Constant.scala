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
import exastencils.datastructures._
import exastencils.prettyprinting._

/// L4_ConstantExpression

trait L4_ConstantExpression extends L4_Expression

/// L4_Number
trait L4_Number extends L4_ConstantExpression {
  def value : AnyVal
}

/// L4_StringLiteral

case class L4_StringLiteral(var value : String) extends L4_ConstantExpression {
  override def prettyprint(out : PpStream) : Unit = out << value
  override def toString : String = value
  override def progress = ProgressLocation(IR_StringLiteral(value))
}

/// L4_StringConstant

case class L4_StringConstant(var value : String) extends L4_ConstantExpression {
  // swap quotation characters for second parse
  def q = if (value.contains("\"")) "\'" else "\""
  override def prettyprint(out : PpStream) : Unit = out << q << value << q
  override def progress = ProgressLocation(IR_StringConstant(value))
}

/// L4_IntegerConstant
case class L4_IntegerConstant(var v : Long) extends L4_Number {
  override def prettyprint(out : PpStream) : Unit = out << v
  override def value = v
  override def progress = ProgressLocation(IR_IntegerConstant(value))
}

/// L4_RealConstant
case class L4_RealConstant(var v : Double) extends L4_Number {
  override def prettyprint(out : PpStream) : Unit = {
    out << value // this uses value.toString(), which is Locale-independent and the string can be parsed without a loss of precision later
  }
  override def value = v
  override def progress = ProgressLocation(IR_RealConstant(value))
}

/// L4_BooleanConstant

case class L4_BooleanConstant(var value : Boolean) extends L4_ConstantExpression {
  override def prettyprint(out : PpStream) : Unit = out << value
  override def progress = ProgressLocation(IR_BooleanConstant(value))
}

// L4_ConvertStringConstantsToLiterals

object L4_ConvertStringConstantsToLiterals extends QuietDefaultStrategy("Convert string constants to literals") {
  this += new Transformation("Convert", {
    case const : L4_StringConstant => L4_StringLiteral(const.value)
  })
}

object L4_ConvertStringLiteralsToConstants extends QuietDefaultStrategy("Convert string literals to constants") {
  this += new Transformation("Convert", {
    case const : L4_StringLiteral => L4_StringConstant(const.value)
  })
}
