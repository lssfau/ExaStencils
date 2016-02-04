package exastencils.parsers.l1

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharSequenceReader.EofCh

/**
  * Defines a basic standard lexical parser for Layer 1
  */
class LexerL1 extends StdLexical {
  override def letter = elem("letter", _.isUnicodeIdentifierStart)

  override def identChar = elem("identifier character", { ch =>
    {
      (
        (ch.isUnicodeIdentifierStart
          || ch == '\\'
          || ch.toString().matches("""[\p{L}\p{M}\p{S}]"""))
          //          || ch.toString().matches("""[\\x{1D400}-\\x{1D7FF}]""")) // FIXME allow math symbols from unicode SMP
          && !delimiters.contains(ch.toString()))
    }
  })

  override def whitespaceChar = elem("space char", { ch => (ch <= ' ' || ch.toString().matches("""[\p{Z}]""")) && ch != EofCh })

  override def token : Parser[Token] = super.token

  delimiters += ("=", "(", ")", "{", "}", "[", "]", ",", ":", "+", "-", "*", "/", "^", "**", "%")

  // Operator keywords
  reserved += ("Operator")

  // Domain keywords
  reserved += ("Domain")
  reserved += ("\\times", "\u00D7")

  // Equation keywords
  reserved += ("Equation")
}

