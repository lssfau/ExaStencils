package exastencils.waLBerla.ir

import exastencils.baseExt.ir.IR_MatShape
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
    var name : String, // will be used to find the field
    var level : Int, // the (geometric) level the field lives on
    var maxLevel : Int,
    var index : Int, // (consecutive) index of the field, can be used as array subscript
    var codeName : String, // will be used in the generated source code
    var layout : IR_WaLBerlaFieldLayout, // represents the number of data points and their distribution in each dimension
    var matShape: Option[IR_MatShape]
) extends IR_LeveledKnowledgeObject with IR_FieldLike {

  override def createDuplicate() : IR_WaLBerlaField = {
    IR_WaLBerlaField(name, level, maxLevel, index, codeName, Duplicate(layout), Duplicate(matShape))
  }

  def domain : IR_Domain = IR_DomainCollection.getByIdentifier("global").get
  var numSlots : Int = 1 // TODO: how to realize slots for waLBerla fields?

  // TODO distinguish between CUDA GPU fields and CPU GhostLayerFields
  def waLBerlaFieldType = "GhostLayerField"
}