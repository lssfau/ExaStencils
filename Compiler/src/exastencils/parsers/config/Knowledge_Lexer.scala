package exastencils.parsers.config

import exastencils.parsers.ExaLexer
import scala.util.parsing.combinator.lexical.StdLexical

/// Knowledge_Lexer
class Knowledge_Lexer extends ExaLexer {
  delimiters += ("=", "(", ")", "{", "}", "-", "+=")
  reserved += ("true", "false", "import")
}
