package exastencils.parsers.l2

import exastencils.parsers.ExaLexer

/**
  * Defines a basic standard lexical parser for Layer 2
  */
object L2_Lexer extends ExaLexer {
  // general delimiters
  delimiters += ("=", "(", ")", "{", "}", ":", "+", "-", "*", "/", "^", "**", "%", ".+", ".-", ".*", "./", ".**", ".%", "!", "+=", "-=", "*=", "/=", "|", "[", "]", ",", "<", ">", "<=", ">=", "@", "=>", "!=", "==", "&&", "||")

  // base => l3_Datatypes
  reserved += ("Unit", "unit",
    "String", "string",
    "Integer", "integer", "Int", "int",
    "Real", "real", "Float", "float", "Double", "double",
    "Complex", "complex",
    "Array", "array",
    "Boolean", "boolean", "Bool", "bool")

  // baseExt => l3_DomainDeclarations
  reserved += ("Domain")

  // base => l3_LevelSpecifications
  reserved += ("current", "coarser", "finer", "coarsest", "finest", "to", "not", "but", "all", "and")

  // baseExt => l3_FieldDeclarations
  reserved += ("Field", "with", "on", "of", "boundary")
  reserved += ("Node", "Cell", "node", "cell", "Face_x", "face_x", "Face_y", "face_y", "Face_z", "face_z")

  // baseExt => l3_OperatorDeclarations
  reserved += ("Operator", "from")

  // baseExt => l3_StencilDeclarations
  reserved += ("Stencil")

  // baseExt => l3_StencilTemplateDeclarations
  reserved += ("StencilTemplate")
}
