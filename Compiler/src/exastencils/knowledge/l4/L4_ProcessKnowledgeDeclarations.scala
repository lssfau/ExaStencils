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

package exastencils.knowledge.l4

import exastencils.field.l4._
import exastencils.interfacing.l4.L4_ProcessExternalFieldDeclarations
import exastencils.operator.l4._

object L4_ProcessKnowledgeDeclarations {
  def apply() = {
    // may require:
    L4_ProcessStencilDeclarations.apply()

    // may require:
    L4_FieldLayoutCollection.L4_ProcessFieldLayoutDeclarations.apply()

    // may require: Domain, FieldLayout
    L4_FieldCollection.L4_ProcessFieldDeclarations.apply() // TODO other collections?

    // may require: Field
    L4_ProcessExternalFieldDeclarations.apply()

    // may require: Stencil and Field
    L4_ProcessStencilFieldDeclarations.apply()
  }
}
