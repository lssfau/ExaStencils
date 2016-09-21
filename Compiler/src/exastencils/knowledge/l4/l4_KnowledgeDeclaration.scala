package exastencils.knowledge.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PrettyPrintable

/// trait L4_KnowledgeDecl

trait L4_KnowledgeDecl extends L4_Node with PrettyPrintable {
  def addToKnowledge() : Unit
}

/// trait L4_LeveledKnowledgeDecl

trait L4_LeveledKnowledgeDecl extends L4_KnowledgeDecl with L4_HasIdentifier {
  def addToKnowledge() : Unit
}

/// L4_UnfoldLeveledKnowledgeDecls

object L4_UnfoldLeveledKnowledgeDecls extends DefaultStrategy("Unfold leveled knowledge declarations") {
  this += new Transformation("Unfold leveled knowledge declarations", {
    case decl : L4_LeveledKnowledgeDecl => doDuplicate(decl, decl.identifier.asInstanceOf[L4_LeveledIdentifier].level)
  })

  def doDuplicate(toDuplicate : L4_LeveledKnowledgeDecl, level : L4_LevelSpecification) : ListBuffer[L4_LeveledKnowledgeDecl] = {
    def duplicateInstance(newLevel : L4_LevelSpecification) = {
      val newInstance = Duplicate(toDuplicate)
      newInstance.identifier = L4_LeveledIdentifier(newInstance.identifier.name, newLevel)
      newInstance
    }

    var duplicated = ListBuffer[L4_LeveledKnowledgeDecl]()
    level match {
      case level @ (L4_SingleLevel(_) | L4_CurrentLevel | L4_CoarserLevel | L4_FinerLevel) =>
        duplicated += duplicateInstance(level)
      case level : L4_LevelList                                                            =>
        level.levels.foreach(level => duplicated ++= doDuplicate(toDuplicate, level))
      case level : L4_LevelRange                                                           =>
        val (begin, end) = (level.begin.resolveLevel, level.end.resolveLevel)
        for (level <- math.min(begin, end) to math.max(begin, end))
          duplicated += duplicateInstance(L4_SingleLevel(level))
      case _                                                                               =>
        Logger.error(s"Invalid level specification for Value $toDuplicate: $level")
    }

    duplicated
  }
}
