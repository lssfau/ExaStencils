package exastencils.parsers.settings

import exastencils.parsers.ExaLexer
import scala.util.parsing.combinator.lexical.StdLexical

class LexerKnowledge extends ExaLexer {
  delimiters += ("=", "(", ")", "{", "}", "-", "+=")
  reserved += ("true", "false", "import")
}
