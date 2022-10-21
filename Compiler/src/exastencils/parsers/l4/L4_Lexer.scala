//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.parsers.l4

import exastencils.parsers.ExaLexer

/// L4_Lexer

/**
 * Defines a basic standard lexical parser for Layer 4
 */
class L4_Lexer extends ExaLexer {
  // general delimiters
  delimiters += ("=", "(", ")", "{", "}", ":", "+", "-", "*", "/", "^", "**", "%", ".+", ".-", ".*", "./", ".**", ".%", "!", "+=", "-=", "*=", "/=", "|", "[", "]", ",", ";", "<", ">", "<=", ">=", "@", "=>", "!=", "==", "&&", "||", ":=")

  // import functionality
  reserved += "import"

  // function keywords
  reserved += ("Func", "Function", "return", "noinline")

  // function instantiation
  reserved += ("FuncTemplate", "FunctionTemplate", "Inst", "Instantiate", "as")

  // declaration keywords - complex
  reserved += ("Domain", "Layout", "Field", "Stencil", "StencilTemplate", "StencilField", "Set", "external", "from", "with", "tensN", "tens1", "tens2")

  // waLBerla keywords
  reserved += ("waLBerla", "waLBerlaSwapPtr", "blocks", "WaLBerlaVars")

  // loop keywords
  reserved += ("repeat", "times", "count", "with", "contraction", "break")
  reserved += ("loop", "until", "while", "over", "fragments", "only", "on", "boundary", "where", "starting", "ending", "stepping", "reduction")
  reserved += ("dup", "ghost", "inner")
  reserved += "sequentially" // FIXME: seq HACK
  reserved += "novect"
  reserved += ("precomm", "postcomm") // temporary loop annotation
  reserved += ("color", "repeat", "with")

  // condition keywords
  reserved += ("if", "else", "and", "or")

  // language data types
  reserved += ("Unit", "unit",
    "String", "string",
    "Integer", "integer", "Int", "int",
    "Real", "real", "Float", "float", "Double", "double", "Dbl", "Flt",
    "Complex", "complex",
    "j",
    "Boolean", "boolean", "Bool", "bool")
  reserved += ("Array", "Vector", "RowVector", "ColumnVector", "RVector", "CVector", "Matrix", "T", "Tensor", "Tensor2", "Tensor1", "TensorN")
  reserved += ("Vec2", "Vec3", "Vec4")

  // level specification keywords
  reserved += ("current", "coarser", "finer", "coarsest", "finest", "to", "not", "but", "all", "and")

  // domain keywords
  reserved += "fromFile"

  // layout and field keywords
  reserved += ("with", "communication", "None",
    "Node", "Cell", "node", "cell",
    "Face_x", "face_x", "Face_y", "face_y", "Face_z", "face_z",
    "Edge_Cell", "edge_cell", "Edge_Node", "edge_node",
    "MatrixShape", "Diagonal", "Blockdiagonal", "Schur"
  )

  reserved += "FieldCombination"

  // boundary condition keywords
  reserved += ("apply", "bc", "to", "Neumann")

  // communication keywords
  reserved += ("begin", "finish", "communicate", "communicating", "dup", "ghost", "of", "upstream", "downstream")

  // slot keywords
  reserved += ("advance", "active", "activeSlot", "currentSlot", "next", "nextSlot", "previous", "previousSlot")

  // solve keywords
  reserved += ("solve", "locally", "jacobi", "relax", "solveMatSys")

  // math keywords
  //  reserved += ()

  // obsolete keywords
  reserved += "steps"

  // base => L4_Declaration
  reserved += ("Var", "Variable", "Val", "Value")

  // baseExt => L4_ExpressionDeclaration
  reserved += ("Expr", "Expression")

  // baseExt => L4_FieldIteratorAccess
  reserved += ("i0", "i1", "i2", "x", "y", "z")

  // baseExt => L4_GlobalSection
  reserved += ("Globals")

  // layoutTransformation.l4 => L4_LayoutSection
  reserved += ("LayoutTransformations", "transform", "concat", "rename", "as", "with", "into")

  // baseExt => l4_OperatorDeclarations
  reserved += ("Operator", "from", "default", "restriction", "prolongation", "on", "with")
  reserved += ("Node", "Cell", "node", "cell", "Face_x", "face_x", "Face_y", "face_y", "Face_z", "face_z")

  // knowledge
  reserved += ("Knowledge")

  // solver
  reserved += ("Equation")

  // util => L4_OffsetAlias
  reserved += ("center", "east", "west", "north", "south", "top", "bottom")
}
