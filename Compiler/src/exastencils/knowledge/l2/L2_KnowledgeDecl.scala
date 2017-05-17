package exastencils.knowledge.l2

import exastencils.base.l2._
import exastencils.core.Duplicate

/// L2_KnowledgeDecl

trait L2_KnowledgeDecl extends L2_Statement {
  def name : String
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
