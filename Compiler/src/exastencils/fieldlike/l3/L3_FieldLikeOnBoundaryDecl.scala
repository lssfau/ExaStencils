package exastencils.fieldlike.l3

import exastencils.base.l3._
import exastencils.boundary.l3.L3_BoundaryCondition
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.logger.Logger

/// L3_FieldLikeOnBoundaryDecl

trait L3_FieldLikeOnBoundaryDecl[L3_Type <: L3_FieldLike[_]] extends L3_FieldLikeDecl[L3_Type] {
  def name : String
  def levels : Option[L3_LevelSpecification]
  def boundary : L3_BoundaryCondition

  override def progress = Logger.error(s"Trying to progress L3 boundary declaration for field $name; this is not supported")
}

/// L3_ProcessBoundaryDeclarations

object L3_ProcessBoundaryDeclarations extends DefaultStrategy("Integrate L3 boundary declarations with knowledge") {
  this += Transformation("Adapt bc's of new fields", {
    case decl : L3_FieldLikeOnBoundaryDecl[_] if L3_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}