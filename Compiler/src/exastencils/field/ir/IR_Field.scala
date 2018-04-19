package exastencils.field.ir

import exastencils.boundary.ir.IR_BoundaryCondition
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_Domain
import exastencils.knowledge.ir.IR_LeveledKnowledgeObject

/// IR_Field

case class IR_Field(
    var name : String, // will be used to find the field
    var level : Int, // the (geometric) level the field lives on
    var index : Int, // (consecutive) index of the field, can be used as array subscript
    var domain : IR_Domain, // the (sub)domain the field lives on
    var codeName : String, // will be used in the generated source code
    var fieldLayout : IR_FieldLayout, // represents the number of data points and their distribution in each dimension
    var numSlots : Int, // the number of copies of the field to be available; can be used to represent different vector components or different versions of the same field (e.g. Jacobi smoothers, time-stepping)
    var boundary : IR_BoundaryCondition // the boundary condition to be enforced when calling apply bc
) extends IR_LeveledKnowledgeObject {

  override def createDuplicate() : IR_Field = {
    IR_Field.tupled(Duplicate(IR_Field.unapply(this).get))
  }

  def numDimsGrid = domain.numDims

  // shortcuts to layout options
  def gridDatatype = fieldLayout.datatype
  def resolveBaseDatatype = fieldLayout.datatype.resolveBaseDatatype
  def resolveDeclType = fieldLayout.datatype.resolveDeclType
  def localization = fieldLayout.localization
  def referenceOffset = fieldLayout.referenceOffset
  def communicatesDuplicated = fieldLayout.communicatesDuplicated
  def communicatesGhosts = fieldLayout.communicatesGhosts
}
