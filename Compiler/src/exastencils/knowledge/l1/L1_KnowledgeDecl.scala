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

package exastencils.knowledge.l1

import exastencils.base.l1._
import exastencils.core.Duplicate
import exastencils.datastructures._

/// L1_KnowledgeDecl

trait L1_KnowledgeDecl extends L1_Statement {
  def name : String
  def addToKnowledge() : Unit
}

/// L1_LeveledKnowledgeDecl

object L1_LeveledKnowledgeDecl {
  def unfoldDecl[TypeOfDecl <: L1_LeveledKnowledgeDecl](decl : TypeOfDecl) = {
    val levelList = L1_LevelSpecification.extractLevelListDefAll(decl.levels)
    levelList.map(level => {
      val newDecl = Duplicate(decl)
      newDecl.levels = Some(L1_SingleLevel(level))
      newDecl
    })
  }
}

trait L1_LeveledKnowledgeDecl extends L1_KnowledgeDecl {
  var levels : Option[L1_LevelSpecification]
}

/// L1_UnfoldKnowledgeDeclarations

object L1_UnfoldKnowledgeDeclarations extends DefaultStrategy("Unfold leveled L1 knowledge declarations") {
  this += Transformation("Process new declarations", {
    case decl : L1_LeveledKnowledgeDecl => L1_LeveledKnowledgeDecl.unfoldDecl(decl)
  })
}
