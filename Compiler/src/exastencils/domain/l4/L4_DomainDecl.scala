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

package exastencils.domain.l4

import exastencils.datastructures._
import exastencils.knowledge.l4.L4_KnowledgeDecl
import exastencils.logger._

/// L4_DomainDecl

abstract class L4_DomainDecl extends L4_KnowledgeDecl {
  override def progress = Logger.error(s"Trying to progress L4 domain declaration for domain $name; this is not supported")
  def addToKnowledge() : Unit
}

/// L4_PrepareDomainDeclarations

object L4_PrepareDomainDeclarations extends DefaultStrategy("Prepare knowledge for L4 domains") {
  this += Transformation("Process new domains", {
    case decl : L4_DomainDecl =>
      L4_DomainCollection.addDeclared(decl.name)
      decl // preserve declaration statement
  })
}

/// L4_ProcessDomainDeclarations

object L4_ProcessDomainDeclarations extends DefaultStrategy("Integrate L4 domain declarations with knowledge") {
  this += Transformation("Process new domains", {
    case decl : L4_DomainDecl =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
