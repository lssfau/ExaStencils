package exastencils.parsers.settings

import exastencils.parsers.ExaLexer
import scala.util.parsing.combinator.lexical.StdLexical

class LexerSettings extends ExaLexer {
  delimiters += ("=", "(", ")", "{", "}", "-", "+=", ",")
  reserved += ("true", "false")
}
