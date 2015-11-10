package exastencils.parsers

import scala.util.parsing.combinator.lexical.StdLexical

/**
  * Defines a basic standard lexical parser common to all Layers
  */
class ExaLexer extends StdLexical {
  // general stuff
  delimiters += ("=", "(", ")", "{", "}", ":", "+", "-", "*", "/", "^", "**", "%", ".+", ".-", ".*", "./", ".**", ".%", "!", "+=", "-=", "*=", "/=", "|", "[", "]", ",", "<", ">", "<=", ">=", "@", "=>", "!=", "==", "&&", "||")

  reserved += ("true", "false")

  override def token : Parser[Token] = floatingToken | super.token

  def floatingToken : Parser[Token] =
    /*optSign ~*/ rep1(digit) ~ optFraction ~ optExponent ^^
      {
        case /*optSign ~*/ intPart ~ frac ~ exp => NumericLit(
          /*optSign ::*/ (intPart mkString "") :: frac :: exp :: Nil mkString "")
      }
  def chr(c : Char) = elem("", ch => ch == c)
  def sign = chr('+') | chr('-')
  def optSign = opt(sign) ^^ {
    case None       => ""
    case Some(sign) => sign
  }
  def fraction = '.' ~ rep(digit) ^^ {
    case dot ~ ff => dot :: (ff mkString "") :: Nil mkString ""
  }
  def optFraction = opt(fraction) ^^ {
    case None           => ""
    case Some(fraction) => fraction
  }
  def exponent = (chr('e') | chr('E')) ~ optSign ~ rep1(digit) ^^ {
    case e ~ optSign ~ exp => e :: optSign :: (exp mkString "") :: Nil mkString ""
  }
  def optExponent = opt(exponent) ^^ {
    case None           => ""
    case Some(exponent) => exponent
  }
}
