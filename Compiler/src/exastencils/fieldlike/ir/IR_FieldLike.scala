package exastencils.fieldlike.ir

import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_MatIndex
import exastencils.baseExt.ir.IR_MatShape
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
  def layout : IR_FieldLikeLayout // represents the number of data points and their distribution in each dimension
  def numSlots : Int // the number of copies of the field to be available; can be used to represent different vector components or different versions of the same field (e.g. Jacobi smoothers, time-stepping)
  def boundary : IR_BoundaryCondition // the boundary condition to be enforced when calling apply bc

  def matShape : Option[IR_MatShape]
  def numDimsGrid = domain.numDims

  def gpuCompatible : Boolean

  // shortcuts to layout options
  def gridDatatype = layout.datatype
  def resolveBaseDatatype = layout.datatype.resolveBaseDatatype
  def resolveDeclType = layout.datatype.resolveDeclType
  def localization = layout.localization
  def referenceOffset = layout.referenceOffset
  def communicatesDuplicated = layout.communicatesDuplicated
  def communicatesGhosts = layout.communicatesGhosts

  // getter functions
  def getFieldAccess(slot : IR_Expression, fragIdx : IR_Expression, index : IR_ExpressionIndex,
      offset : Option[IR_ConstIndex] = None, frozen : Boolean = false, matIndex : Option[IR_MatIndex] = None) : IR_FieldLikeAccess

  def getDirectFieldAccess(slot : IR_Expression, fragIdx : IR_Expression, index : IR_ExpressionIndex) : IR_DirectFieldLikeAccess

  def getLinearizedFieldAccess(slot : IR_Expression, fragIdx : IR_Expression, index : IR_Expression) : IR_LinearizedFieldLikeAccess

  def getFieldData(slot : IR_Expression, fragIdx : IR_Expression) : IR_IV_AbstractFieldLikeData
}
