package exastencils.waLBerla.l2.field

import exastencils.base.l2.L2_LevelSpecification
import exastencils.boundary.l2.L2_BoundaryCondition
import exastencils.fieldlike.l2.L2_FieldLikeOnBoundaryDecl
import exastencils.prettyprinting.PpStream

case class L2_WaLBerlaBoundaryFieldDecl(
    var name : String,
    var levels : Option[L2_LevelSpecification],
    var boundary : L2_BoundaryCondition
) extends L2_FieldLikeOnBoundaryDecl {

  override def prettyprint(out : PpStream) = {
    out << "waLBerla Field " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " on boundary = " << boundary
  }

  def addToKnowledge() : Unit = {
    val fieldToAdapt = L2_WaLBerlaFieldCollection.getByIdentifier(name, L2_LevelSpecification.asSingleLevel(levels)).get
    fieldToAdapt.boundary = boundary
  }
}
