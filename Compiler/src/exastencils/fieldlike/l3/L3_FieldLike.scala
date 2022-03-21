package exastencils.fieldlike.l3

import exastencils.base.l3.L3_ComplexDatatype
import exastencils.base.l3.L3_Datatype
import exastencils.base.l3.L3_Expression
import exastencils.base.l3.L3_ScalarDatatype
import exastencils.baseExt.l3.L3_MatrixDatatype
import exastencils.baseExt.l3.L3_VectorDatatype
import exastencils.boundary.l3.L3_BoundaryCondition
import exastencils.domain.l3.L3_Domain
import exastencils.field.l3.L3_Field
import exastencils.fieldlike.l4.L4_FieldLike
import exastencils.grid.l3.L3_Localization
import exastencils.knowledge.l3.L3_LeveledKnowledgeObject

trait L3_FieldLike[L4_FieldAbstraction <: L4_FieldLike[_, _]] extends L3_LeveledKnowledgeObject[L4_FieldAbstraction] {

  var name : String
  var level : Int
  var domain : L3_Domain
  var datatype : L3_Datatype
  var localization : L3_Localization
  var numSlots : Int
  var initial : Option[L3_Expression]
  var boundary : L3_BoundaryCondition

  def fieldLayoutName : String

  def toField : L3_Field // TODO: remove

  def codeName : String = name + "_" + level
  def numDimsGrid : Int = domain.numDims

  def printDatatype(dt : L3_Datatype) : String = {
    dt match {
      case dt : L3_ScalarDatatype                 => dt.prettyprint()
      case L3_ComplexDatatype(inner)              => "Complex" + printDatatype(inner)
      case L3_VectorDatatype(inner, count, isRow) => "Vec" + printDatatype(inner) + count + (if (isRow) "Row" else "")
      case L3_MatrixDatatype(inner, n, m)         => "Mat" + printDatatype(inner) + n + "x" + m
    }
  }
}
