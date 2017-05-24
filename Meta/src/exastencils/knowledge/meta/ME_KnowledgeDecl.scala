package exastencils.knowledge.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_KnowledgeDecl extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/knowledge/|LAYER_LC|/|LAYER_UC|_KnowledgeDecl.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.knowledge.|LAYER_LC|

import exastencils.base.|LAYER_LC|._
import exastencils.core.Duplicate
import exastencils.datastructures._

/// |LAYER_UC|_KnowledgeDecl

trait |LAYER_UC|_KnowledgeDecl extends |LAYER_UC|_Statement {
  def name : String
}

/// |LAYER_UC|_LeveledKnowledgeDecl

object |LAYER_UC|_LeveledKnowledgeDecl {
  def unfoldDecl[TypeOfDecl <: |LAYER_UC|_LeveledKnowledgeDecl](decl : TypeOfDecl) = {
    val levelList = |LAYER_UC|_LevelSpecification.extractLevelListDefAll(decl.levels)
    levelList.map(level => {
      val newDecl = Duplicate(decl)
      newDecl.levels = Some(|LAYER_UC|_SingleLevel(level))
      newDecl
    })
  }
}

trait |LAYER_UC|_LeveledKnowledgeDecl extends |LAYER_UC|_KnowledgeDecl {
  var levels : Option[|LAYER_UC|_LevelSpecification]
}

/// |LAYER_UC|_UnfoldKnowledgeDeclarations

object |LAYER_UC|_UnfoldKnowledgeDeclarations extends DefaultStrategy("Unfold leveled |LAYER_UC| knowledge declarations") {
  this += Transformation("Process new declarations", {
    case decl : |LAYER_UC|_LeveledKnowledgeDecl => |LAYER_UC|_LeveledKnowledgeDecl.unfoldDecl(decl)
  })
}
"""
  }
}
