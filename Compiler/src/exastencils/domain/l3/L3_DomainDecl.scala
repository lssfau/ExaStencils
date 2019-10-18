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

import exastencils.datastructures._
import exastencils.knowledge.l3.L3_KnowledgeDecl
import exastencils.logger._

/// L3_DomainDecl

abstract class L3_DomainDecl extends L3_KnowledgeDecl {
  override def progress = Logger.error(s"Trying to progress L3 domain declaration for domain $name; this is not supported")
  def addToKnowledge() : Unit
}

/// L3_PrepareDomainDeclarations

object L3_PrepareDomainDeclarations extends DefaultStrategy("Prepare knowledge for L3 domains") {
  this += Transformation("Process new domains", {
    case decl : L3_DomainDecl =>
      L3_DomainCollection.addDeclared(decl.name)
      decl // preserve declaration statement
  })
}

/// L3_ProcessDomainDeclarations

object L3_ProcessDomainDeclarations extends DefaultStrategy("Integrate L3 domain declarations with knowledge") {
  this += Transformation("Process new domains", {
    case decl : L3_DomainDecl =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
