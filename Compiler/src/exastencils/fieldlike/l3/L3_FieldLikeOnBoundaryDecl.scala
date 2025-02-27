package exastencils.fieldlike.l3

import exastencils.base.l3._
import exastencils.boundary.l3.L3_BoundaryCondition
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.knowledge.l3.L3_LeveledKnowledgeDecl
import exastencils.logger.Logger

/// L3_FieldLikeOnBoundaryDecl

trait L3_FieldLikeOnBoundaryDecl extends L3_LeveledKnowledgeDecl {
  def name : String
  def levels : Option[L3_LevelSpecification]
  def boundary : L3_BoundaryCondition

  override def progress = Logger.error(s"Trying to progress L3 boundary declaration for field $name; this is not supported")
}

/// L3_ProcessBoundaryDeclarations

object L3_ProcessBoundaryDeclarations extends DefaultStrategy("Integrate L3 boundary declarations with knowledge") {
  this += Transformation("Adapt bc's of new fields", {
    case decl : L3_FieldLikeOnBoundaryDecl if L3_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}