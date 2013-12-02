package exastencils.parsers

import scala.util.parsing.combinator.lexical._

/**
  * Defines a basic standard lexical parser common to all levels
  */
class ExaLexer extends StdLexical {
  // language level 1 keywords
  reserved += ("let", "loop", "next", "level", "def", "return", "ToFine", "ToCoarse", "if", "else", "repeat", "Reduction", "class", "block", "public", "order", "Communicate", "decl")

  // language level 2 keywords

  // language level 3 keywords

  // language level 4 keywords

  // language datatypes
  reserved += ("Unit", "String", "Integer", "Real", "Complex", "Array")
}