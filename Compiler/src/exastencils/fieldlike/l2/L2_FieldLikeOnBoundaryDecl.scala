package exastencils.fieldlike.l2

import exastencils.base.l2._
import exastencils.boundary.l2.L2_BoundaryCondition
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.knowledge.l2.L2_LeveledKnowledgeDecl
import exastencils.logger.Logger

/// L2_FieldLikeOnBoundaryDecl

trait L2_FieldLikeOnBoundaryDecl extends L2_LeveledKnowledgeDecl {
  def name : String
  def levels : Option[L2_LevelSpecification]
  def boundary : L2_BoundaryCondition

  override def progress = Logger.error(s"Trying to progress L2 boundary declaration for field $name; this is not supported")
}

/// L2_ProcessBoundaryDeclarations

object L2_ProcessBoundaryDeclarations extends DefaultStrategy("Integrate L2 boundary declarations with knowledge") {
  this += Transformation("Adapt bc's of new fields", {
    case decl : L2_FieldLikeOnBoundaryDecl if L2_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
