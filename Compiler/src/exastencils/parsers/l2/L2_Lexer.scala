package exastencils.parsers.l2

import exastencils.parsers.ExaLexer

/**
  * Defines a basic standard lexical parser for Layer 2
  */
object L2_Lexer extends ExaLexer {
  // general delimiters
  delimiters += ("=", "(", ")", "{", "}", ":", "+", "-", "*", "/", "^", "**", "%", ".+", ".-", ".*", "./", ".**", ".%", "!", "+=", "-=", "*=", "/=", "|", "[", "]", ",", "<", ">", "<=", ">=", "@", "=>", "!=", "==", "&&", "||")

  // import functionality
  reserved += "import"

  // base => L2_Datatypes
  reserved += ("Unit", "unit",
    "String", "string",
    "Integer", "integer", "Int", "int",
    "Real", "real", "Float", "float", "Double", "double",
    "Complex", "complex",
    "Array", "array",
    "Boolean", "boolean", "Bool", "bool")

  // baseExt => L2_DomainDeclarations
  reserved += ("Domain")

  // base => L2_LevelSpecifications
  reserved += ("current", "coarser", "finer", "coarsest", "finest", "to", "not", "but", "all", "and")

  // base => L2_Declaration
  reserved += ("Var", "Variable", "Val", "Value")

  // baseExt => L2_ApplicationHints
  reserved += ("ApplicationHint", "ApplicationHints", "L4Hint", "L4Hints")

  // baseExt => L2_ExpressionDeclaration
  reserved += ("Expr", "Expression")

  // baseExt => L2_FieldIteratorAccess
  reserved += ("i0", "i1", "i2", "x", "y", "z")

  // baseExt => L2_FieldDeclarations
  reserved += ("Field", "with", "on", "times", "of", "boundary")
  reserved += ("Node", "Cell", "node", "cell", "Face_x", "face_x", "Face_y", "face_y", "Face_z", "face_z")

  reserved += "FieldCombination"

  // baseExt => L2_GlobalSection
  reserved += ("Globals")

  // baseExt => L2_OperatorDeclarations
  reserved += ("Operator", "from", "default", "restriction", "prolongation", "on", "with")
  reserved += ("Node", "Cell", "node", "cell", "Face_x", "face_x", "Face_y", "face_y", "Face_z", "face_z")

  // baseExt => L2_StencilDeclarations
  reserved += ("Stencil")

  // baseExt => L2_StencilTemplateDeclarations
  reserved += ("StencilTemplate")

  // boundary
  reserved += ("None", "Neumann")

  // knowledge
  reserved += ("Knowledge")

  // solver
  reserved += ("Equation")
  reserved += ("Solve", "SolverHint", "SolverHints", "L3Hint", "L3Hints")
  reserved += ("generate", "solver", "for", "in")

  // util => L2_OffsetAlias
  reserved += ("center", "east", "west", "north", "south", "top", "bottom")

  /// TO BE INTEGRATED
  reserved += ("operators", "equation", "store", "is")
}
