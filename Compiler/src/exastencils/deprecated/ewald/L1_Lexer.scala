package exastencils.deprecated.ewald

import scala.util.parsing.combinator.lexical.StdLexical

object L1_Lexer extends StdLexical {

  case class Delimiter(chars : String) extends Token {
    override def toString = chars
  }

  delimiters += ("[", "]", "(", ")") // brackets
  delimiters += ("=", "+", "-", "*", "/", "**", "^^") // operators
  delimiters += (".", ":", ",", ";", "_", "^") // separators
  delimiters += ("\u2202", "\\partial") // partial differential
  delimiters += ("\u0394", "\u2206", "\\Delta") // Capital delta (Laplace-operator)
  delimiters += ("\u00D7", "\\times") // multiplication sign

  reserved += ("Domain", "Equation") // section names
  reserved += ("x", "U") // reserved variables

  override def identChar = letter
  override def processIdent(name : String) = {
    if (reserved contains name) Keyword(name)
    else if (delimiters contains name) Delimiter(name)
    else Identifier(name)
  }
  override def token : Parser[Token] = super.token
}
