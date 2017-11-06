package exastencils.field.l1

import exastencils.base.l1._
import exastencils.boundary.l1._
import exastencils.datastructures._
import exastencils.domain.l1._
import exastencils.knowledge.l1._
import exastencils.logger.Logger
import exastencils.parsers.l1.L1_ReservedSigns
import exastencils.prettyprinting._

/// L1_BoundaryFieldDecl

object L1_BoundaryFieldDecl {
  def apply(identifier : String, levels : Option[L1_LevelSpecification], domain : String, boundary : L1_BoundaryCondition) =
    new L1_BoundaryFieldDecl(identifier, levels, L1_FutureDomainAccess(domain), boundary)
}

case class L1_BoundaryFieldDecl(
    var name : String,
    var levels : Option[L1_LevelSpecification],
    var domain : L1_Access,
    var boundary : L1_BoundaryCondition) extends L1_LeveledKnowledgeDecl {

  override def prettyprint(out : PpStream) = {
    out << "Field " << name
    if (levels.isDefined) out << '@' << levels.get
    out << s" ${ L1_ReservedSigns.elemOf._2 } ${ L1_ReservedSigns.partial._2 } " << domain << " = " << boundary
  }

  override def progress = Logger.error(s"Trying to progress L1 boundary declaration for field $name; this is not supported")

  def addToKnowledge() : Unit = {
    val fieldToAdapt = L1_FieldCollection.getByIdentifier(name, L1_LevelSpecification.asSingleLevel(levels)).get
    if (fieldToAdapt.domain != domain.asInstanceOf[L1_DomainAccess].target)
      Logger.warn(s"Incompatible domain specification for base and boundary definitions of field $name on level $levels")
    fieldToAdapt.boundary = boundary
  }
}

/// L1_ProcessBoundaryDeclarations

object L1_ProcessBoundaryDeclarations extends DefaultStrategy("Integrate L1 boundary declarations with knowledge") {
  this += Transformation("Adapt bc's of new fields", {
    case decl : L1_BoundaryFieldDecl if L1_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
