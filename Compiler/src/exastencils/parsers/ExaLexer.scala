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

package exastencils.parsers

import scala.util.parsing.combinator.lexical.StdLexical

/**
  * Defines a basic standard lexical parser common to all Layers
  */
class ExaLexer extends StdLexical {
  override def token : Parser[Token] = floatingToken | super.token

  def floatingToken : Parser[Token] =
    rep1(digit) ~ optFraction ~ optExponent ^^ {
      case intPart ~ frac ~ exp => NumericLit((intPart mkString "") :: frac :: exp :: Nil mkString "")
    }
  def chr(c : Char) = elem("", ch => ch == c)
  def optFraction = opt(fraction) ^^ {
    case None           => ""
    case Some(fraction) => fraction
  }
  def fraction = '.' ~ rep(digit) ^^ {
    case dot ~ ff => dot :: (ff mkString "") :: Nil mkString ""
  }
  def optExponent = opt(exponent) ^^ {
    case None           => ""
    case Some(exponent) => exponent
  }
  def exponent = (chr('e') | chr('E')) ~ optSign ~ rep1(digit) ^^ {
    case e ~ optSign ~ exp => e :: optSign :: (exp mkString "") :: Nil mkString ""
  }
  def optSign = opt(sign) ^^ {
    case None       => ""
    case Some(sign) => sign
  }
  def sign = chr('+') | chr('-')

  reserved += ("true", "false", "import")
}
