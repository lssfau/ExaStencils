package exastencils.parsers.l4

/**
  * Defines a basic standard lexical parser for Layer 4
  */
class LexerL4 extends exastencils.parsers.ExaLexer {

  // function keywords
  reserved += ("Func", "Function", "return")

  // declaration keywords - simple
  reserved += ("Var", "Variable", "Val", "Value")

  // declaration keywords - complex
  reserved += ("Domain", "Layout", "Field", "Stencil", "StencilField", "Set", "external", "Globals")

  // loop keywords
  reserved += ("repeat", "times", "count", "with", "contraction", "break")
  reserved += ("loop", "until", "over", "fragments", "only", "on", "boundary", "where", "starting", "ending", "stepping", "reduction")
  reserved += ("sequentially") // FIXME: seq HACK

  // condition keywords
  reserved += ("if", "else")

  // language datatypes
  reserved += ("Unit", "String", "Integer", "Real", "Complex", "Array")

  // level specification keywords
  reserved += ("current", "coarser", "finer", "coarsest", "finest", "to", "not", "all", "and")

  //domain keywords
  reserved += ("fromFile")

  // layout and field keywords
  reserved += ("ghostLayers", "duplicateLayers", "innerPoints", "with", "communication", "None", "Node", "Cell", "node", "cell", "Face_x", "face_x", "Face_y", "face_y", "Face_z", "face_z")

  // boundary condition keywords
  reserved += ("apply", "bc", "to")

  // communication keywords
  reserved += ("begin", "finish", "communicate", "communicating", "dup", "ghost", "of")

  // slot keywords
  reserved += ("advance", "active", "activeSlot", "currentSlot", "next", "nextSlot", "previous", "previousSlot")

  // math keywords
//  reserved += ()

  // obsolete keywords
  reserved += ("steps")
}
