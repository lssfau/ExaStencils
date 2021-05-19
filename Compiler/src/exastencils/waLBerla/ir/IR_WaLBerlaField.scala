package exastencils.waLBerla.ir

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_Index
import exastencils.baseExt.ir.IR_Linearization
import exastencils.baseExt.ir.IR_MatShape
import exastencils.boundary.ir.IR_BoundaryCondition
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_Domain
import exastencils.field.ir.IR_Field
import exastencils.knowledge.ir.IR_LeveledKnowledgeObject
import exastencils.logger.Logger

/// IR_WaLBerlaField

object IR_WaLBerlaField extends ((String, Int, Int, IR_Field, IR_Domain, String, IR_WaLBerlaFieldLayout, Int, IR_BoundaryCondition, Option[IR_MatShape]) => IR_WaLBerlaField) {
  // adaptor for regular fields
  def apply(field : IR_Field) : IR_WaLBerlaField = {
    new IR_WaLBerlaField(field.name, field.level, field.index, field, field.domain, field.codeName,
      IR_WaLBerlaFieldLayout(field.layout), field.numSlots, field.boundary, field.matShape)
  }
}


case class IR_WaLBerlaField(
    var name : String, // will be used to find the field
    var level : Int, // the (geometric) level the field lives on
    var index : Int, // (consecutive) index of the field, can be used as array subscript
    var field : IR_Field, // underlying field
    var domain : IR_Domain, // the (sub)domain the field lives on
    var codeName : String, // will be used in the generated source code
    var layout : IR_WaLBerlaFieldLayout, // represents the number of data points and their distribution in each dimension
    var numSlots : Int, // the number of copies of the field to be available; can be used to represent different vector components or different versions of the same field (e.g. Jacobi smoothers, time-stepping)
    var boundary : IR_BoundaryCondition, // the boundary condition to be enforced when calling apply bc
    var matShape: Option[IR_MatShape]
) extends IR_LeveledKnowledgeObject {

  override def createDuplicate() : IR_WaLBerlaField = {
    IR_WaLBerlaField.tupled(Duplicate(IR_WaLBerlaField.unapply(this).get))
  }

  def linearizeIndex(index : IR_Index) : IR_Expression = {
    if (layout.numDimsData != index.length())
      Logger.warn(s"Index length mismatch for $name@$level: ${ index.length() }, should be ${layout.numDimsData}")
    IR_Linearization.linearizeIndex(index, IR_ExpressionIndex((0 until math.min(layout.numDimsData, index.length())).map(layout.idxById(this, "TOT", _)).toArray))
  }

  // TODO distinguish between CUDA GPU fields and CPU GhostLayerFields
  def waLBerlaFieldType = "GhostLayerField"

  def numDimsGrid = domain.numDims
  def numDimsData = layout.numDimsData

  // shortcuts to layout options
  def gridDatatype = layout.datatype
  def resolveBaseDatatype = layout.datatype.resolveBaseDatatype
  def resolveDeclType = layout.datatype.resolveDeclType
  def localization = layout.localization
  def referenceOffset = layout.referenceOffset
  def communicatesDuplicated = layout.communicatesDuplicated
  def communicatesGhosts = layout.communicatesGhosts
}

