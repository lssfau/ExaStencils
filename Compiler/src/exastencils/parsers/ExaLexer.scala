package exastencils.parsers

import scala.util.parsing.combinator.lexical._

/**
  * Defines a basic standard lexical parser common to all levels
  */
class ExaLexer extends StdLexical {
  // general stuff
  delimiters += ("=", "(", ")", "{", "}", ":", "+", "-", "*", "/", "+=", "|", "[", "]", ",")
  
  reserved += ("true", "false")

  // FIXME move to corresponding levels
  reserved += ("let", "level", "def", "return", "ToFine", "ToCoarse", "if", "else", "repeat", "Reduction", "class", "block", "public", "order", "Communicate", "decl")

  /*
   * language level 1 keywords
   */

  /*
   * language level 2 keywords
   */

  /*
   * language level 3 keywords
   */

  /*
   * language level 4 keywords
   */
  // function keywords
  reserved += ("def", "return")
  
  // declaration keywords
  reserved += ("var", "Integer", "Real", "String", "Array", "Complex", "Unit", "Domain")
  
  // loop keywords
  reserved += ("loop", "over", "next", "domain", "inner", "boundary", "blocksize", "levels")

  /*
   * language datatypes
   */
  reserved += ("Unit", "String", "Integer", "Real", "Complex", "Array")
}