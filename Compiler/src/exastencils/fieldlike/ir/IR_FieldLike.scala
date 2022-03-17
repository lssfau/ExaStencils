package exastencils.fieldlike.ir

import exastencils.boundary.ir.IR_BoundaryCondition
import exastencils.domain.ir.IR_Domain
import exastencils.knowledge.ir.IR_LeveledKnowledgeObject

/// IR_FieldLike

trait IR_FieldLike extends IR_LeveledKnowledgeObject {
  def name : String // will be used to find the field
  def level : Int // the (geometric) level the field lives on
  def index : Int // (consecutive) index of the field, can be used as array subscript
  def domain : IR_Domain // the (sub)domain the field lives on
  def codeName : String // will be used in the generated source code
  def layout : IR_FieldLayoutLike // represents the number of data points and their distribution in each dimension
  def numSlots : Int // the number of copies of the field to be available; can be used to represent different vector components or different versions of the same field (e.g. Jacobi smoothers, time-stepping)
  def boundary : IR_BoundaryCondition // the boundary condition to be enforced when calling apply bc

  def numDimsGrid = domain.numDims

  // shortcuts to layout options
  def gridDatatype = layout.datatype
  def resolveBaseDatatype = layout.datatype.resolveBaseDatatype
  def resolveDeclType = layout.datatype.resolveDeclType
  def localization = layout.localization
  def referenceOffset = layout.referenceOffset
  def communicatesDuplicated = layout.communicatesDuplicated
  def communicatesGhosts = layout.communicatesGhosts
}
