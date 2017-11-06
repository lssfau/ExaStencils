package exastencils.parsers.l1

import exastencils.parsers.ExaLexer

/**
  * Defines a basic standard lexical parser for Layer 1
  */
object L1_Lexer extends ExaLexer {
  // general delimiters
  delimiters += ("=", "(", ")", "{", "}", ":", "+", "-", "*", "/", "^", "**", "%", ".+", ".-", ".*", "./", ".**", ".%", "!", "+=", "-=", "*=", "/=", "|", "[", "]", ",", "<", ">", "<=", ">=", "@", "=>", "!=", "==", "&&", "||")

  // reserved signs
  delimiters += ("\u2208", "\\in")
  delimiters += ("\u2209", "\\notin")

  delimiters += ("\u00D7", "\\times") // cartesian product or cross product

  // import functionality
  reserved += "import"

  // base => L1_LevelSpecification
  reserved += ("current", "coarser", "finer", "coarsest", "finest", "to", "not", "but", "all", "and")

  // discretization
  reserved += ("Discretize")
  reserved += ("on", "with")

  // domain => L1_DomainDecl
  reserved += ("Domain", "to")

  // field => L1_FieldDecl
  reserved += ("Field")

  // grid
  reserved += ("Node", "Cell", "node", "cell", "Face_x", "face_x", "Face_y", "face_y", "Face_z", "face_z")
}

object L1_ReservedSigns {
  val elemOf = ("\u2208", "\\in")
  val notElemOf = ("\u2209", "\\notin")

  val times = ("\u00D7", "\\times") // cartesian product or cross product
}
