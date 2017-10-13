package exastencils.parsers.config

import exastencils.parsers.ExaLexer

/// Samples_Lexer

class Runner_Lexer extends ExaLexer {
  delimiters += ("=", "(", ")", "{", "}", "-", "+=", ",", "~")
  reserved += ("Variabilities", "Constraints", "DerivedParameters")
}
