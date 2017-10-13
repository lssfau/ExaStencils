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
