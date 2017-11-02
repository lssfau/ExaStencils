package exastencils.parsers.l1

import exastencils.parsers.ExaLexer

/**
  * Defines a basic standard lexical parser for Layer 1
  */
object L1_Lexer extends ExaLexer {
  // general delimiters
  delimiters += ("=", "(", ")", "{", "}", ":", "+", "-", "*", "/", "^", "**", "%", ".+", ".-", ".*", "./", ".**", ".%", "!", "+=", "-=", "*=", "/=", "|", "[", "]", ",", "<", ">", "<=", ">=", "@", "=>", "!=", "==", "&&", "||")

  // reserved signs
  delimiters += ("\u00D7", "\\times") // cartesian product or cross product

  // import functionality
  reserved += "import"

  // domain => L1_DomainDecl
  reserved += ("Domain", "to")

}

object L1_ReservedSigns {
  val times = ("\u00D7", "\\times") // cartesian product or cross product
}
