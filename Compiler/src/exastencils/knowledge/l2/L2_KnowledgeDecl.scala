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

package exastencils.knowledge.l2

import exastencils.base.l2._
import exastencils.core.Duplicate
import exastencils.datastructures._

/// L2_KnowledgeDecl

trait L2_KnowledgeDecl extends L2_Statement {
  def name : String
  def addToKnowledge() : Unit
}

/// L2_LeveledKnowledgeDecl

object L2_LeveledKnowledgeDecl {
  def unfoldDecl[TypeOfDecl <: L2_LeveledKnowledgeDecl](decl : TypeOfDecl) = {
    val levelList = L2_LevelSpecification.extractLevelListDefAll(decl.levels)
    levelList.map(level => {
      val newDecl = Duplicate(decl)
      newDecl.levels = Some(L2_SingleLevel(level))
      newDecl
    })
  }
}

trait L2_LeveledKnowledgeDecl extends L2_KnowledgeDecl {
  var levels : Option[L2_LevelSpecification]
}

/// L2_UnfoldKnowledgeDeclarations

object L2_UnfoldKnowledgeDeclarations extends DefaultStrategy("Unfold leveled L2 knowledge declarations") {
  this += Transformation("Process new declarations", {
    case decl : L2_LeveledKnowledgeDecl => L2_LeveledKnowledgeDecl.unfoldDecl(decl)
  })
}
