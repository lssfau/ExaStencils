package exastencils.parsers.config

import exastencils.parsers.ExaLexer

/// Settings_Lexer

class Settings_Lexer extends ExaLexer {
  delimiters += ("=", "(", ")", "{", "}", "-", "+=", ",")
}
