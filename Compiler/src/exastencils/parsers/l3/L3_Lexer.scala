package exastencils.parsers.l3

import exastencils.parsers.ExaLexer

/**
  * Defines a basic standard lexical parser for Layer 3
  */
object L3_Lexer extends ExaLexer {
  // general delimiters
  delimiters += ("=", "(", ")", "{", "}", ":", "+", "-", "*", "/", "^", "**", "%", ".+", ".-", ".*", "./", ".**", ".%", "!", "+=", "-=", "*=", "/=", "|", "[", "]", ",", "<", ">", "<=", ">=", "@", "=>", "!=", "==", "&&", "||")

  // base => l3_Assignment
  reserved += ("where")

  // base => l3_Datatypes
  reserved += ("Unit", "unit",
    "String", "string",
    "Integer", "integer", "Int", "int",
    "Real", "real", "Float", "float", "Double", "double",
    "Complex", "complex",
    "Array", "array",
    "Boolean", "boolean", "Bool", "bool")

  // base => l3_LevelSpecifications
  reserved += ("current", "coarser", "finer", "coarsest", "finest", "to", "not", "but", "all", "and")

  // baseExt => l3_Conditional
  reserved += ("if", "else", "and", "or")

  // baseExt => l3_LocalDeclarations
  reserved += ("Var", "Variable", "Val", "Value")

  // baseExt => l3_Loops
  reserved += ("repeat", "until", "while", "times", "count")

  // baseExt => l3_FieldDeclarations
  reserved += ("Field", "from", "L2", "override", "bc", "for", "with")

  // baseExt => l3_Functions
  reserved += ("Func", "Function", "return")

  // baseExt => l3_FunctionInstantiation
  reserved += ("FuncTemplate", "FunctionTemplate", "Inst", "Instantiate", "as")

  // baseExt => l3_OperatorDeclarations
  reserved += ("Operator", "from", "L2")

  // baseExt => l3_StencilDeclarations
  reserved += ("Stencil")
}
