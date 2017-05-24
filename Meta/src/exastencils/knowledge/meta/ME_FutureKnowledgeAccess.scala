package exastencils.knowledge.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_FutureKnowledgeAccess extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/knowledge/|LAYER_LC|/|LAYER_UC|_FutureKnowledgeAccess.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.knowledge.|LAYER_LC|

import exastencils.base.|LAYER_LC|._
import exastencils.core.StateManager

/// |LAYER_UC|_FutureKnowledgeAccess

object |LAYER_UC|_FutureKnowledgeAccess {
  def existsInStmt(stmt : |LAYER_UC|_Statement) = StateManager.findFirst[|LAYER_UC|_FutureKnowledgeAccess](stmt).isDefined
}

trait |LAYER_UC|_FutureKnowledgeAccess extends |LAYER_UC|_Access
"""
  }
}
