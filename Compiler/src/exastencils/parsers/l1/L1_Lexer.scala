package exastencils.parsers.l1

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharSequenceReader.EofCh

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

//
///**
//  * Defines a basic standard lexical parser for Layer 1
//  */
//object oldL1_Lexer extends StdLexical {
//
//  case class Delimiter(chars : String) extends Token {
//    override def toString = chars
//  }
//
//  override def identChar = letter
//
//  //  override def identChar = elem("identifier character", { ch =>
//  //    {
//  //      (
//  //        (ch.isUnicodeIdentifierStart
//  //          || ch == '\\'
//  //          || ch == '_'
//  //          || ch.toString().matches("""[\p{L}\p{M}\p{S}]""") // General Unicode characters: Letters, Marks (accents/umlauts), math Symbols
//  //          //          || ch.toString().matches("""[\\x{1D400}-\\x{1D7FF}]""")) // FIXME allow math symbols from unicode SMP
//  //          || ch == '²' || ch == '³' || ch == '¹' // Exponents carried over from ISO8859
//  //          || ch.toString().matches("""[\x{2070}-\x{2079}]""") // Unicode digit superscripts
//  //          || ch.toString().matches("""[\x{2080}-\x{2089}]""") // Unicode digit subscripts
//  //          || ch.toString().matches("""[\x{2090}-\x{209C}]""")) // Unicode letter subscripts
//  //          && !(delimiters.contains(ch.toString())
//  //            || ch.toString().matches("""[\x{2119}-\x{211D}]""") // Unicode identifiers for sets
//  //            ))
//  //    }
//  //  })
//
//  override def letter = elem("letter", _.isUnicodeIdentifierStart)
//
//  override def processIdent(name : String) = {
//    if (reserved contains name) Keyword(name)
//    else if (delimiters contains name) Delimiter(name)
//    else Identifier(name)
//  }
//
//  override def token : Parser[Token] = super.token
//
//  override def whitespaceChar = elem("space char", { ch => (ch <= ' ' || ch.toString().matches("""[\p{Z}]""")) && ch != EofCh })
//
//  delimiters += ("[", "]", "(", ")") // brackets
//  delimiters += ("=", "+", "-", "*", "/", "**", "^^") // operators
//  delimiters += (".", ":", ",", ";", "_", "^") // separators
//  delimiters += ("\u2202", "\\partial") // partial differential
//  delimiters += ("\u0394", "\u2206", "\\Delta") // Capital delta (Laplace-operator)
//  delimiters += ("\u00D7", "\\times") // multiplication sign
//
//  // Operator keywords
//  reserved += ("Operator")
//
//  // Domain keywords
//  reserved += ("Domain")
//  reserved += ("\\times", "\u00D7")
//
//  // Equation keywords
//  reserved += ("Equation")
//
//  // RHS keywords
//  reserved += ("RHS")
//
//  // Mapping keywords
//  reserved += ("Mapping")
//  reserved += ("C", "R")
//
//  // other keywords
//  reserved += ("x", "U") // reserved variables
//
//
//
//
//
//
//}

