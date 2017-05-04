package exastencils.parsers.l4

import exastencils.parsers.ExaLexer

/// L4_Lexer

/**
  * Defines a basic standard lexical parser for Layer 4
  */
class L4_Lexer extends ExaLexer {
  // general delimiters
  delimiters += ("=", "(", ")", "{", "}", ":", "+", "-", "*", "/", "^", "**", "%", ".+", ".-", ".*", "./", ".**", ".%", "!", "+=", "-=", "*=", "/=", "|", "[", "]", ",", ";", "<", ">", "<=", ">=", "@", "=>", "!=", "==", "&&", "||")

  // for import functionality
  reserved += "import"

  // function keywords
  reserved += ("Func", "Function", "return", "noinline")

  // function instantiation
  reserved += ("FuncTemplate", "FunctionTemplate", "Inst", "Instantiate", "as")

  // declaration keywords - simple
  reserved += ("Var", "Variable", "Val", "Value")

  // declaration keywords - complex
  reserved += ("Domain", "Layout", "Field", "Stencil", "StencilTemplate", "StencilField", "Set", "external", "Globals")

  // loop keywords
  reserved += ("repeat", "times", "count", "with", "contraction", "break")
  reserved += ("loop", "until", "while", "over", "fragments", "only", "on", "boundary", "where", "starting", "ending", "stepping", "reduction")
  reserved += "sequentially" // FIXME: seq HACK
  reserved += ("precomm", "postcomm") // temporary loop annotation
  reserved += ("color", "with")

  // condition keywords
  reserved += ("if", "else", "and", "or")

  // language data types
  reserved += ("Unit", "String", "Integer", "Int", "Real", "Complex", "Array", "Vector", "RowVector", "ColumnVector", "RVector", "CVector", "Matrix", "Boolean", "Bool", "T")
  reserved += ("Vec2", "Vec3", "Vec4")

  // boolean keywords
  reserved += ("true", "false")

  // level specification keywords
  reserved += ("current", "coarser", "finer", "coarsest", "finest", "to", "not", "but", "all", "and")

  // domain keywords
  reserved += "fromFile"

  // layout and field keywords
  reserved += ("with", "communication", "None",
    "Node", "Cell", "node", "cell",
    "Face_x", "face_x", "Face_y", "face_y", "Face_z", "face_z",
    "Edge_Cell", "edge_cell", "Edge_Node", "edge_node")

  // boundary condition keywords
  reserved += ("apply", "bc", "to", "Neumann")

  // communication keywords
  reserved += ("begin", "finish", "communicate", "communicating", "dup", "ghost", "of")

  // slot keywords
  reserved += ("advance", "active", "activeSlot", "currentSlot", "next", "nextSlot", "previous", "previousSlot")

  // solve keywords
  reserved += ("solve", "locally", "relax")

  // math keywords
  //  reserved += ()

  // obsolete keywords
  reserved += "steps"
}
