package exastencils.field.l3

import exastencils.base.l3._
import exastencils.boundary.l3._
import exastencils.datastructures._
import exastencils.knowledge.l3._
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L3_BoundaryFieldDecl

case class L3_BoundaryFieldDecl(
    var name : String,
    var levels : Option[L3_LevelSpecification],
    var boundary : L3_BoundaryCondition) extends L3_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) = {
    out << "Field " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " on boundary = " << boundary
  }

  override def progress = Logger.error(s"Trying to progress L3 boundary declaration for field $name; this is not supported")

  def addToKnowledge() : Unit = {
    val fieldToAdapt = L3_FieldCollection.getByIdentifier(name, L3_LevelSpecification.asSingleLevel(levels)).get
    fieldToAdapt.boundary = boundary
  }
}

/// L3_ProcessBoundaryDeclarations

object L3_ProcessBoundaryDeclarations extends DefaultStrategy("Integrate L3 boundary declarations with knowledge") {
  this += Transformation("Adapt bc's of new fields", {
    case decl : L3_BoundaryFieldDecl if L3_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
