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

package exastencils.solver.l2

import exastencils.knowledge.l2.L2_KnowledgeContainer._
import exastencils.knowledge.l2._
import exastencils.solver.l3._

/// L2_EquationCollection

object L2_EquationCollection extends L2_LeveledKnowledgeCollection[L2_NamedEquation, L3_NamedEquation] {
  exastencils.core.Duplicate.registerConstant(this)

  L2_KnowledgeContainer.register(this)

  L2_PrepareDeclarations.strategies += L2_PrepareEquationDeclarations
  L2_ProcessDeclarations.strategies += L2_ProcessEquationDeclarations

  L2_PrepareAccesses.strategies += L2_PrepareEquationAccesses
  L2_ResolveAccesses.strategies += L2_ResolveEquationAccesses

  override def name = "L2_EquationCollection"
  override def progress() = objects.foreach(obj => L3_EquationCollection.add(obj.progress()))
}
