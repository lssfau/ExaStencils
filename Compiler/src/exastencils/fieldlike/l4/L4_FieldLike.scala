package exastencils.fieldlike.l4

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_MatShape
import exastencils.boundary.l4.L4_BoundaryCondition
import exastencils.domain.l4.L4_Domain
import exastencils.field.l4.L4_SlotSpecification
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.fieldlike.ir.IR_FieldLikeLayout
import exastencils.knowledge.l4.L4_LeveledKnowledgeObject

trait L4_FieldLike[IR_FieldAbstraction <: IR_FieldLike, IR_FieldLayoutAbstraction <: IR_FieldLikeLayout] extends L4_LeveledKnowledgeObject[IR_FieldAbstraction] {
  def name : String // will be used to find the field
  def level : Int // the level the field lives on
  def index : Int
  def domain : L4_Domain
  def fieldLayout : L4_FieldLikeLayout[IR_FieldLayoutAbstraction]
  def numSlots : Int
  var boundary : L4_BoundaryCondition
  def matShape : Option[L4_MatShape]

  var gpuCompatible : Boolean

  def datatype = fieldLayout.datatype
  def localization = fieldLayout.localization

  def codeName = name + "_" + level
  def numDimsGrid = domain.numDims

  def getFieldAccess(slot : L4_SlotSpecification, offset : Option[L4_ConstIndex] = None, frozen : Boolean = false, matIndex : Option[L4_MatIndex] = None) : L4_FieldLikeAccess
}
