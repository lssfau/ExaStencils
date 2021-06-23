package exastencils.waLBerla.ir

import exastencils.baseExt.ir.IR_MatShape
import exastencils.boundary.ir.IR_BoundaryCondition
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_Domain
import exastencils.domain.ir.IR_DomainCollection
import exastencils.field.ir.IR_FieldAccess
import exastencils.field.ir.IR_FieldLike
import exastencils.knowledge.ir.IR_LeveledKnowledgeObject

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
    var maxLevel : Int,
    var index : Int,
    var codeName : String,
    var layout : IR_WaLBerlaFieldLayout,
    var boundary : IR_BoundaryCondition,
    var matShape: Option[IR_MatShape]
) extends IR_LeveledKnowledgeObject with IR_FieldLike {

  override def createDuplicate() : IR_WaLBerlaField = {
    IR_WaLBerlaField(name, level, maxLevel, index, codeName, Duplicate(layout), Duplicate(boundary), Duplicate(matShape))
  }

  def domain : IR_Domain = IR_DomainCollection.getByIdentifier("global").get
  var numSlots : Int = 1 // TODO: how to realize slots for waLBerla fields?

  // TODO distinguish between CUDA GPU fields and CPU GhostLayerFields
  def waLBerlaFieldType = "GhostLayerField"
}