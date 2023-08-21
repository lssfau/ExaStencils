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

package exastencils.domain.l1

import exastencils.domain.l2._
import exastencils.knowledge.l1.L1_KnowledgeContainer._
import exastencils.knowledge.l1._
import exastencils.scheduling.NoStrategyWrapper

/// L1_DomainCollection

object L1_DomainCollection extends L1_BasicKnowledgeCollection[L1_Domain, L2_Domain] {
  exastencils.core.Duplicate.registerConstant(this)

  L1_KnowledgeContainer.register(this)

  L1_PrepareDeclarations.strategies += L1_PrepareDomainDeclarations
  L1_ProcessDeclarations.strategies += L1_ProcessDomainDeclarations

  L1_PrepareAccesses.strategies += L1_PrepareDomainAccesses
  L1_ResolveAccesses.strategies += L1_ResolveDomainAccesses

  override def name = "L1_DomainCollection"
  override def progress() = objects.foreach(obj => L2_DomainCollection.add(obj.progress()))

  def handleAliases() = {
    val aliasForGlobal = List("\\Omega", "\\u03a9", "Omega")
    objects.foreach {
      case d if aliasForGlobal.contains(d.name) => d.name = "global"
      case _                                    =>
    }
  }
}

/// L1_HandleDomainAliasesWrapper

object L1_HandleDomainAliasesWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => L1_DomainCollection.handleAliases()
}
