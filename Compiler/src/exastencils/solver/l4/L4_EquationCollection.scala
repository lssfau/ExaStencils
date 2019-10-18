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

package exastencils.solver.l4

import exastencils.knowledge.l4.L4_KnowledgeContainer._
import exastencils.knowledge.l4._
import exastencils.solver.ir._

/// L4_EquationCollection

object L4_EquationCollection extends L4_LeveledKnowledgeCollection[L4_NamedEquation, IR_NamedEquation] {
  exastencils.core.Duplicate.registerConstant(this)

  L4_KnowledgeContainer.register(this)

  L4_PrepareDeclarations.strategies += L4_PrepareEquationDeclarations
  L4_ProcessDeclarations.strategies += L4_ProcessEquationDeclarations

  L4_PrepareAccesses.strategies += L4_PrepareEquationAccesses
  L4_ResolveAccesses.strategies += L4_ResolveEquationAccesses

  override def name = "L4_EquationCollection"
  override def progress() = objects.foreach(obj => IR_EquationCollection.add(obj.progress()))
}
