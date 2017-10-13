package exastencils.parsers.config

import exastencils.parsers.ExaLexer

/// Knowledge_Lexer
class Knowledge_Lexer extends ExaLexer {
  delimiters += ("=", "(", ")", "{", "}", "-", "+=")
}
