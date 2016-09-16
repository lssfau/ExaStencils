package exastencils.parsers.settings

import exastencils.parsers._

class LexerPlatform extends ExaLexer {
  delimiters += ("=", "(", ")", "{", "}", "-", "+=")
  reserved += ("true", "false", "import")
}
