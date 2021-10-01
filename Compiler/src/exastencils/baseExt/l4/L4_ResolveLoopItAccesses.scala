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

package exastencils.baseExt.l4

import exastencils.base.l4._
import exastencils.datastructures._

/// L4_ResolveLoopItAccesses

object L4_ResolveLoopItAccesses extends DefaultStrategy("Resolve accesses to loop iterators") {
  this += new Transformation("Resolve iterator accesses", {
    // basic loop iterators
    case L4_UnresolvedAccess("x", _, None, _, _, _) => L4_PlainVariableAccess("x", L4_IntegerDatatype, false)
    case L4_UnresolvedAccess("y", _, None, _, _, _) => L4_PlainVariableAccess("y", L4_IntegerDatatype, false)
    case L4_UnresolvedAccess("z", _, None, _, _, _) => L4_PlainVariableAccess("z", L4_IntegerDatatype, false)

    // fragmentIdx
    case L4_UnresolvedAccess("fragmentIdx", _, None, _, _, _) => L4_PlainVariableAccess("fragmentIdx", L4_IntegerDatatype, false)

    // TODO: other potential iterators
  })
}
