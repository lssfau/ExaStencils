package exastencils.parsers.config

import exastencils.parsers._

/// Platform_Lexer

class Platform_Lexer extends ExaLexer {
  delimiters += ("=", "(", ")", "{", "}", "-", "+=")
}
