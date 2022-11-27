package exastencils.waLBerla.ir.field

import exastencils.baseExt.ir.IR_MatShape
import exastencils.boundary.ir.IR_BoundaryCondition
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_Domain
import exastencils.domain.ir.IR_DomainCollection
import exastencils.field.ir.IR_FieldAccess
import exastencils.fieldlike.ir.IR_FieldLike

/// IR_WaLBerlaField

object IR_WaLBerlaField {
  // adaptor for regular field accesses
  def apply(fieldAccess : IR_FieldAccess) : IR_WaLBerlaField = {
    IR_WaLBerlaFieldCollection.getByIdentifier(fieldAccess.field.name, fieldAccess.field.level, suppressError = true).get
  }
}

case class IR_WaLBerlaField(
    var name : String,
    var level : Int,
    var index : Int,
    var codeName : String,
    var layout : IR_WaLBerlaFieldLayout,
    var numSlots : Int,
    var boundary : IR_BoundaryCondition,
    var matShape: Option[IR_MatShape],
    var gpuCompatible : Boolean = false
) extends IR_FieldLike {

  override def createDuplicate() : IR_WaLBerlaField = {
    IR_WaLBerlaField(name, level, index, codeName, Duplicate(layout), numSlots, Duplicate(boundary), Duplicate(matShape))
  }

  def stringIdentifier(slot : Int) = codeName + s"_s$slot"

  def domain : IR_Domain = IR_DomainCollection.getByIdentifier("global").get

  // TODO distinguish between CUDA GPU fields and CPU GhostLayerFields
  def waLBerlaFieldType = "GhostLayerField"
}