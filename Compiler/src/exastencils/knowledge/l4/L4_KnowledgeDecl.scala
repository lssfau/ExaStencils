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
  var levels : Option[L4_LevelSpecification]
}

/// L4_UnfoldKnowledgeDeclarations

object L4_UnfoldKnowledgeDeclarations extends DefaultStrategy("Unfold leveled L4 knowledge declarations") {
  this += Transformation("Process new declarations", {
    case decl : L4_LeveledKnowledgeDecl => L4_LeveledKnowledgeDecl.unfoldDecl(decl)
  })
}
