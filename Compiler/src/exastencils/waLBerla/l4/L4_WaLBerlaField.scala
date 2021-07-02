package exastencils.waLBerla.l4

import exastencils.base.l4.L4_Datatype
import exastencils.baseExt.l4.L4_MatShape
import exastencils.boundary.l4.L4_BoundaryCondition
import exastencils.core.Duplicate
import exastencils.domain.l4.L4_Domain
import exastencils.domain.l4.L4_DomainCollection
import exastencils.field.l4.L4_Field
import exastencils.grid.l4.L4_Localization
import exastencils.knowledge.l4.L4_LeveledKnowledgeObject
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.IR_WaLBerlaField

case class L4_WaLBerlaField(
    var name : String,
    var level : Int,
    var index : Int,
    var fieldLayout : L4_WaLBerlaFieldLayout,
    var boundary : L4_BoundaryCondition,
    var matShape : Option[L4_MatShape] = None
) extends L4_LeveledKnowledgeObject[IR_WaLBerlaField] {

  override def createDuplicate() : L4_WaLBerlaField = {
    L4_WaLBerlaField.tupled(Duplicate(L4_WaLBerlaField.unapply(this).get))
  }

  def datatype : L4_Datatype = fieldLayout.datatype
  def localization : L4_Localization = fieldLayout.localization

  def domain : L4_Domain = L4_DomainCollection.getByIdentifier("global").get
  def codeName : String = name + "_" + level
  def numDimsGrid : Int = domain.numDims
  def numSlots = 1

  override def prettyprintDecl(out : PpStream) : Unit = {
    out << "waLBerla Field " << name
    out << "< " << fieldLayout.name
    if (matShape.isDefined) out << ", " << matShape
    out << " >"
    out << "@" << level
  }

  def toField = L4_Field(name, level, index, domain, fieldLayout.toFieldLayout, numSlots, boundary, matShape)

  override def progressImpl() =
    IR_WaLBerlaField(name, level, index, codeName, fieldLayout.getProgressedObj(), boundary.progress,
      if(matShape.isDefined) Some(matShape.get.progress) else None)
}
