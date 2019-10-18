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

  delimiters += ("\u2202", "\\partial") // partial differential

  delimiters += ("\u00D7", "\\times") // cartesian product or cross product

  delimiters += ("\u2206", "\\Delta")

  delimiters += ("\u03a9", "\\Omega")

  // import functionality
  reserved += "import"

  // base => L1_LevelSpecification
  reserved += ("current", "coarser", "finer", "coarsest", "finest", "to", "not", "but", "all", "and")

  // baseExt => L1_ApplicationHints
  reserved += ("ApplicationHint", "ApplicationHints", "L4Hint", "L4Hints")

  // discretization
  reserved += ("Discretize", "DiscretizationHint", "DiscretizationHints", "L2Hint", "L2Hints")
  reserved += ("on", "with", "order", "direction")

  // domain => L1_DomainDecl
  reserved += ("Domain", "to")

  // field => L1_FieldDecl
  reserved += ("Field")

  reserved += "FieldCombination"

  // grid
  reserved += ("Node", "Cell", "node", "cell", "Face_x", "face_x", "Face_y", "face_y", "Face_z", "face_z")

  // boundary
  reserved += ("Neumann")

  // knowledge
  reserved += ("Knowledge")

  // operator
  reserved += ("Operator")
  reserved += "_"

  // solver
  reserved += ("Equation")
  reserved += ("Solve", "SolverHint", "SolverHints", "L3Hint", "L3Hints")
  reserved += ("generate", "solver", "for", "in")
}

object L1_ReservedSigns {
  val elemOf = ("\u2208", "\\in")
  val notElemOf = ("\u2209", "\\notin")

  val partial = ("\u2202", "\\partial")

  val times = ("\u00D7", "\\times") // cartesian product or cross product

  val capitalDelta = ("\u2206", "\\Delta")

  val capitalOmega = ("\u03a9", "\\Omega")
}
