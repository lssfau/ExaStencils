package exastencils.parsers.config

import exastencils.parsers.ExaLexer
import scala.util.parsing.combinator.lexical.StdLexical

/// Settings_Lexer

class Settings_Lexer extends ExaLexer {
  delimiters += ("=", "(", ")", "{", "}", "-", "+=", ",")
  reserved += ("true", "false", "import")
}
