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

package exastencils.domain.l3

import exastencils.domain.l4._
import exastencils.knowledge.l3.L3_KnowledgeContainer._
import exastencils.knowledge.l3._

/// L3_DomainCollection

object L3_DomainCollection extends L3_BasicKnowledgeCollection[L3_Domain, L4_Domain] {
  exastencils.core.Duplicate.registerConstant(this)

  L3_KnowledgeContainer.register(this)

  L3_PrepareDeclarations.strategies += L3_PrepareDomainDeclarations
  L3_ProcessDeclarations.strategies += L3_ProcessDomainDeclarations

  L3_PrepareAccesses.strategies += L3_PrepareDomainAccesses
  L3_ResolveAccesses.strategies += L3_ResolveDomainAccesses

  override def name = "L3_DomainCollection"
  override def progress() = objects.foreach(obj => L4_DomainCollection.add(obj.progress()))
}
