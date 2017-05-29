package exastencils.knowledge.l3

import exastencils.base.l3._
import exastencils.core.Duplicate
import exastencils.datastructures._

/// L3_KnowledgeDecl

trait L3_KnowledgeDecl extends L3_Statement {
  def name : String
  def addToKnowledge() : Unit
}

/// L3_LeveledKnowledgeDecl

object L3_LeveledKnowledgeDecl {
  def unfoldDecl[TypeOfDecl <: L3_LeveledKnowledgeDecl](decl : TypeOfDecl) = {
    val levelList = L3_LevelSpecification.extractLevelListDefAll(decl.levels)
    levelList.map(level => {
      val newDecl = Duplicate(decl)
      newDecl.levels = Some(L3_SingleLevel(level))
      newDecl
    })
  }
}

trait L3_LeveledKnowledgeDecl extends L3_KnowledgeDecl {
  var levels : Option[L3_LevelSpecification]
}

/// L3_UnfoldKnowledgeDeclarations

object L3_UnfoldKnowledgeDeclarations extends DefaultStrategy("Unfold leveled L3 knowledge declarations") {
  this += Transformation("Process new declarations", {
    case decl : L3_LeveledKnowledgeDecl => L3_LeveledKnowledgeDecl.unfoldDecl(decl)
  })
}
