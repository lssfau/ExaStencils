package exastencils.field.l2

import exastencils.base.l2._
import exastencils.boundary.l2._
import exastencils.datastructures._
import exastencils.knowledge.l2._
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L2_BoundaryFieldDecl

case class L2_BoundaryFieldDecl(
    var name : String,
    var levels : Option[L2_LevelSpecification],
    var boundary : L2_BoundaryCondition) extends L2_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) = out << "--- FIXME ---"
  override def progress = Logger.error(s"Trying to progress L2 boundary declaration for field $name; this is not supported")

  def addToKnowledge() : Unit = {
    val fieldToAdapt = L2_FieldCollection.getByIdentifier(name, L2_LevelSpecification.asSingleLevel(levels)).get
    fieldToAdapt.boundary = boundary
  }
}

/// L2_ProcessBoundaryDeclarations

object L2_ProcessBoundaryDeclarations extends DefaultStrategy("Integrate L2 boundary declarations with knowledge") {
  this += Transformation("Adapt bc's of new fields", {
    case decl : L2_BoundaryFieldDecl if L2_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
