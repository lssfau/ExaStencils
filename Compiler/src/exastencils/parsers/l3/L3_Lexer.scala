package exastencils.parsers.l3

import exastencils.parsers.ExaLexer

/**
  * Defines a basic standard lexical parser for Layer 3
  */
object L3_Lexer extends ExaLexer {
  // general delimiters
  delimiters += ("=", "(", ")", "{", "}", ":", "+", "-", "*", "/", "^", "**", "%", ".+", ".-", ".*", "./", ".**", ".%", "!", "+=", "-=", "*=", "/=", "|", "[", "]", ",", "<", ">", "<=", ">=", "@", "=>", "!=", "==", "&&", "||")

  // import functionality
  reserved += "import"

  // base => L3_Assignment
  reserved += ("where")

  // base => L3_Datatypes
  reserved += ("Unit", "unit",
    "String", "string",
    "Integer", "integer", "Int", "int",
    "Real", "real", "Float", "float", "Double", "double",
    "Complex", "complex",
    "Array", "array",
    "Boolean", "boolean", "Bool", "bool")

  // baseExt => L3_DomainDeclarations
  reserved += ("Domain")

  // base => L3_LevelSpecifications
  reserved += ("current", "coarser", "finer", "coarsest", "finest", "to", "not", "but", "all", "and")

  // baseExt => L3_Conditional
  reserved += ("if", "else", "and", "or")

  // base => L3_Declaration
  reserved += ("Var", "Variable", "Val", "Value")

  // baseExt => L3_Loops
  reserved += ("repeat", "until", "while", "times", "count")

  // baseExt => L3_FieldDeclarations
  reserved += ("Field", "from", "L2", "override", "bc", "for", "with", "on", "times", "of", "boundary")
  reserved += ("Node", "Cell", "node", "cell", "Face_x", "face_x", "Face_y", "face_y", "Face_z", "face_z")

  // baseExt => L3_ApplicationHints
  reserved += ("ApplicationHint", "ApplicationHints", "L4Hint", "L4Hints")

  // baseExt => L3_ExpressionDeclaration
  reserved += ("Expr", "Expression")

  // baseExt => L3_FieldIteratorAccess
  reserved += ("i0", "i1", "i2", "x", "y", "z")

  // baseExt => L3_Functions
  reserved += ("Func", "Function", "return")

  // baseExt => L3_FunctionInstantiation
  reserved += ("FuncTemplate", "FunctionTemplate", "Inst", "Instantiate", "as")

  // baseExt => L3_GlobalSection
  reserved += ("Globals")

  // baseExt => L3_OperatorDeclarations
  reserved += ("Operator", "from", "default", "restriction", "prolongation", "on", "with")
  reserved += ("Node", "Cell", "node", "cell", "Face_x", "face_x", "Face_y", "face_y", "Face_z", "face_z")

  // baseExt => L3_StencilDeclarations
  reserved += ("Stencil")

  // boundary
  reserved += ("None", "Neumann")

  // knowledge
  reserved += ("Knowledge")

  // solver
  reserved += ("generate", "solver", "for", "in", "and")
  reserved += ("modifiers", "replace", "append", "to", "prepend", "with")
  reserved += ("smootherHint", "smootherStage", "loopBase", "solveFor")

  // util => L3_OffsetAlias
  reserved += ("east", "west", "north", "south", "top", "bottom")
}
