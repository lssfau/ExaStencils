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
    "Real", "real", "Float", "float", "Double", "double", "Dbl", "Flt",
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

  // waLBerla keywords
  reserved += ("waLBerla", "waLBerlaSwapPtr", "blocks", "WaLBerlaVars")

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
