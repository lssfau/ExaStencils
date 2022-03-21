package exastencils.fieldlike.l2

import exastencils.base.l2.L2_Datatype
import exastencils.base.l2.L2_Expression
import exastencils.boundary.l2.L2_BoundaryCondition
import exastencils.domain.l2.L2_Domain
import exastencils.field.l2.L2_Field
import exastencils.fieldlike.l3.L3_FieldLike
import exastencils.grid.l2.L2_Localization
import exastencils.knowledge.l2.L2_LeveledKnowledgeObject


trait L2_FieldLike[L3_FieldAbstraction <: L3_FieldLike[_]] extends L2_LeveledKnowledgeObject[L3_FieldAbstraction] {

  var name : String
  var level : Int
  var domain : L2_Domain
  var datatype : L2_Datatype
  var localization : L2_Localization
  var numSlots : Int
  var initial : Option[L2_Expression]
  var boundary : L2_BoundaryCondition

  def toField : L2_Field // TODO: remove

  def codeName : String = name + "_" + level
  def numDimsGrid : Int = domain.numDims
}
