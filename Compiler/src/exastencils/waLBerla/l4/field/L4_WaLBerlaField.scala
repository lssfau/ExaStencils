package exastencils.waLBerla.l4.field

import exastencils.base.l4.L4_ConstIndex
import exastencils.base.l4.L4_MatIndex
import exastencils.baseExt.l4.L4_MatShape
import exastencils.boundary.l4.L4_BoundaryCondition
import exastencils.core.Duplicate
import exastencils.domain.l4.L4_Domain
import exastencils.domain.l4.L4_DomainCollection
import exastencils.field.l4.L4_SlotSpecification
import exastencils.fieldlike.l4.L4_FieldLike
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.field.IR_WaLBerlaField
import exastencils.waLBerla.ir.field.IR_WaLBerlaFieldLayout

case class L4_WaLBerlaField(
    var name : String,
    var level : Int,
    var index : Int,
    var fieldLayout : L4_WaLBerlaFieldLayout,
    var numSlots : Int,
    var boundary : L4_BoundaryCondition,
    var matShape : Option[L4_MatShape] = None,
    var gpuCompatible : Boolean = true
) extends L4_FieldLike[IR_WaLBerlaField, IR_WaLBerlaFieldLayout] {

  override def createDuplicate() : L4_WaLBerlaField = {
    L4_WaLBerlaField.tupled(Duplicate(L4_WaLBerlaField.unapply(this).get))
  }

  def domain : L4_Domain = L4_DomainCollection.getByIdentifier("global").get

  override def prettyprintDecl(out : PpStream) : Unit = {
    out << "waLBerla Field " << name
    out << "< " << fieldLayout.name
    if (matShape.isDefined) out << ", " << matShape
    out << " >"
    if (numSlots > 1) out << "[" << numSlots << "]"
    out << "@" << level
  }

  override def progressImpl() =
    IR_WaLBerlaField(
      name,
      level,
      index,
      codeName,
      fieldLayout.getProgressedObj(),
      numSlots,
      boundary.progress,
      if (matShape.isDefined) Some(matShape.get.progress) else None,
      gpuCompatible)

  override def getFieldAccess(slot : L4_SlotSpecification, offset : Option[L4_ConstIndex], frozen : Boolean, matIndex : Option[L4_MatIndex]) : L4_WaLBerlaFieldAccess =
    L4_WaLBerlaFieldAccess(this, slot, offset, frozen, matIndex)
}
