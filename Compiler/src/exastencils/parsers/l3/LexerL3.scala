package exastencils.parsers.l3

import scala.util.parsing.combinator.lexical.StdLexical

/**
  * Defines a basic standard lexical parser for Layer 3
  */
class LexerL3 extends exastencils.parsers.ExaLexer {

  // function keywords
  reserved += ("Func", "Function", "Inst", "Instantiate", "with", "as")

  //  // declaration keywords - simple
  //  reserved += ("Var", "Variable", "Val", "Value")
  //
  // declaration keywords - complex
  reserved += ("Field", "Stencil")
  //
  //  // loop keywords
  //  reserved += ("repeat", "times", "count", "with", "contraction")
  //  reserved += ("loop", "until", "over", "fragments", "where", "starting", "ending", "stepping", "reduction")
  //  reserved += ("sequentially") // FIXME: seq HACK
  //
  //  // condition keywords
  //  reserved += ("if", "else")
  //
  // language datatypes
  reserved += ("Unit", "String", "Integer", "Real", "Complex", "Array")
  //
  // level specification keywords
  reserved += ("current", "coarser", "finer", "coarsest", "finest", "to", "not", "all", "and")
  //
  //  // layout and field keywords
  //  reserved += ("with", "communication", "None")
  //
  //  // boundary condition keywords
  //  reserved += ("apply", "bc", "to")
  //
  //  // communication keywords
  //  reserved += ("begin", "finish", "communicate", "communicating", "dup", "ghost", "of")
  //
  //  // math keywords
  //  reserved += ("diag")
  //
  //  // obsolete keywords
  //  reserved += ("steps")
}