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

package exastencils.solver.l1

import exastencils.knowledge.l1.L1_KnowledgeContainer._
import exastencils.knowledge.l1._
import exastencils.solver.l2._

/// L1_EquationCollection

object L1_EquationCollection extends L1_LeveledKnowledgeCollection[L1_NamedEquation, L2_NamedEquation] {
  exastencils.core.Duplicate.registerConstant(this)

  L1_KnowledgeContainer.register(this)

  L1_PrepareDeclarations.strategies += L1_PrepareEquationDeclarations
  L1_ProcessDeclarations.strategies += L1_ProcessEquationDeclarations

  L1_PrepareAccesses.strategies += L1_PrepareEquationAccesses
  L1_ResolveAccesses.strategies += L1_ResolveEquationAccesses

  override def name = "L1_EquationCollection"
  override def progress() = objects.foreach(obj => L2_EquationCollection.add(obj.progress()))
}
