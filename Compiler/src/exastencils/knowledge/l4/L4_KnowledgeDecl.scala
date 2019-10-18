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

import exastencils.base.l4._
import exastencils.core.Duplicate
import exastencils.datastructures._

/// L4_KnowledgeDecl

trait L4_KnowledgeDecl extends L4_Statement {
  def name : String
  def addToKnowledge() : Unit
}

/// L4_LeveledKnowledgeDecl

object L4_LeveledKnowledgeDecl {
  def unfoldDecl[TypeOfDecl <: L4_LeveledKnowledgeDecl](decl : TypeOfDecl) = {
    val levelList = L4_LevelSpecification.extractLevelListDefAll(decl.levels)
    levelList.map(level => {
      val newDecl = Duplicate(decl)
      newDecl.levels = Some(L4_SingleLevel(level))
      newDecl
    })
  }
}

trait L4_LeveledKnowledgeDecl extends L4_KnowledgeDecl {
  var levels : Option[L4_DeclarationLevelSpecification]
}

/// L4_UnfoldKnowledgeDeclarations

object L4_UnfoldKnowledgeDeclarations extends DefaultStrategy("Unfold leveled L4 knowledge declarations") {
  this += Transformation("Process new declarations", {
    case decl : L4_LeveledKnowledgeDecl => L4_LeveledKnowledgeDecl.unfoldDecl(decl)
  })
}
