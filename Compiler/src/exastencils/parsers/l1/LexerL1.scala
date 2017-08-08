package exastencils.parsers.l1

import exastencils.base.l1._
import scala.util.parsing.combinator.lexical.StdLexical

object L1_Lexer extends StdLexical {

  //case class Delimiter(chars : String) extends Token {
  //  override def toString = chars
  //}
  // delimiters += ("\u2202", "\\partial", "\u0394", "\\Delta",  "_", "=",  "+", "-", "*", "^", "**", "^^", "/", ",", ".", ";", ":", "[", "]", "(", ")", "\\times", "\u00D7")
  delimiters += ("[","]", "(",")", "{","}")                     // brackets
  delimiters += ("=", "+", "-", "*", "/", "**", "^^")  // simple operators
  delimiters += (".", ":",  ",", ";", "_", "^")        // separators

  delimiters ++= ReservedSigns.fieldsToStringSet
  /*
  delimiters += ("\u2202", "\\partial")                // partial differential
  delimiters += ("\u0394", "\u2206", "\\Delta")        // Capital delta (Laplace-operator)
  delimiters += ("\u00D7", "\\times")                  // multiplication sign
  delimiters += ("\u2227", "\\land")                   // logical and
  delimiters += ("\u2228", "\\lor")                    // logical or
  delimiters += ("\u2229", "\\cap")                    // intersection
  delimiters += ("\u222A", "\\cup")                    // union
  delimiters += ("\u2208", "\\in")                     // element of
  delimiters += ("\u2209", "\\notin")                  // not element of
  delimiters += ("\u2264", "\\leq")                    // less than or equal to
  delimiters += ("\u2265", "\\geq")                    // greater than or equal to

  reserved += ("Domain","Equation","Options")  // section names
  reserved += ("\u03a9", "\\Omega")  // capital letter omega
  reserved += ("e","n","U","x")      // reserved variables
  reserved += (ReservedNames.errorOrder)
  reserved += (ReservedNames.gridPoints)
  reserved += (ReservedNames.solution)
  reserved += (ReservedNames.vector)
  */
  reserved ++= ReservedNames.fieldsToStringSet()

  override def identChar = letter

  override def processIdent (name : String) =
  {
    if (reserved contains name) Keyword(name)
    else if (delimiters contains name) Keyword(name) //Delimiter(name)
    else Identifier(name)
  }

  override def token : Parser[Token] =
    ( elem('\\') ~ rep( identChar | digit ) ^^ { case head ~ tail => processIdent(head :: tail mkString "") }
      | super.token
      )
}
