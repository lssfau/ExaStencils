package exastencils.waLBerla.l3

import exastencils.base.l3.L3_LevelSpecification
import exastencils.boundary.l3.L3_BoundaryCondition
import exastencils.fieldlike.l3.L3_FieldLikeOnBoundaryDecl
import exastencils.prettyprinting.PpStream

case class L3_WaLBerlaBoundaryFieldDecl(
  var name : String,
  var levels : Option[L3_LevelSpecification],
  var boundary : L3_BoundaryCondition
) extends L3_FieldLikeOnBoundaryDecl[L3_WaLBerlaField] {

  override def prettyprint(out : PpStream) = {
    out << "waLBerla Field " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " on boundary = " << boundary
  }

  def addToKnowledge() : Unit = {
    val fieldToAdapt = L3_WaLBerlaFieldCollection.getByIdentifier(name, L3_LevelSpecification.asSingleLevel(levels)).get
    fieldToAdapt.boundary = boundary
  }
}
