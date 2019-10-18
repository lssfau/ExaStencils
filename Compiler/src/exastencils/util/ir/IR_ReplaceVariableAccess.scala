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

package exastencils.util.ir

import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.datastructures._

/// IR_ReplaceVariableAccess

object IR_ReplaceVariableAccess extends QuietDefaultStrategy("Replace something with something else") {
  var replace : Map[String, Node] = null // must be replaced

  this += new Transformation("Search and replace", {
// TODO: rely only on IR_VariableAccess => eliminate IR_StringLiteral occurrences
    case IR_VariableAccess(str, _) if replace.isDefinedAt(str) => Duplicate(replace(str))
    case IR_StringLiteral(str) if replace.isDefinedAt(str)     => Duplicate(replace(str))
  }, false)
}
