package exastencils.field.l3

import exastencils.base.l3._
import exastencils.baseExt.l3._
import exastencils.boundary.l3.L3_BoundaryCondition
import exastencils.domain.l3.L3_Domain
import exastencils.field.l4._
import exastencils.grid.l3.L3_Localization
import exastencils.knowledge.l3.L3_LeveledKnowledgeObject
import exastencils.prettyprinting.PpStream

/// L3_Field

object L3_Field {
  def apply(name : String, level : Int, domain : L3_Domain, datatype : L3_Datatype, localization : L3_Localization,
      initial : Option[L3_Expression], boundary : L3_BoundaryCondition) =
    new L3_Field(name, level, domain, datatype, localization, 1, initial, boundary)
}

case class L3_Field(
    var name : String,
    var level : Int,
    var domain : L3_Domain,
    var datatype : L3_Datatype,
    var localization : L3_Localization,
    var numSlots : Int,
    var initial : Option[L3_Expression],
    var boundary : L3_BoundaryCondition) extends L3_LeveledKnowledgeObject[L4_Field] {

  def printDatatype(dt : L3_Datatype) : String = {
    dt match {
      case dt : L3_ScalarDatatype                 => dt.prettyprint()
      case L3_ComplexDatatype(inner)              => "Complex" + printDatatype(inner)
      case L3_VectorDatatype(inner, count, isRow) => "Vec" + printDatatype(inner) + count + (if (isRow) "Row" else "")
      case L3_MatrixDatatype(inner, n, m)         => "Mat" + printDatatype(inner) + n + "x" + m
    }
  }

  def fieldLayoutName = s"defLayoutFor_${ printDatatype(datatype) }_on_${ localization.prettyprint() }"

  override def prettyprintDecl(out : PpStream) : Unit = ???

  def codeName = name + "_" + level
  def numDimsGrid = domain.numDims

  override def progressImpl() = {
    L4_Field(
      name,
      level,
      -1, // index is to be set later
      domain.getProgressedObj(),
      L4_FieldLayoutCollection.getByIdentifier(fieldLayoutName, level).get, // l3 field layout is not available -> grab l4 layout directly
      numSlots,
      boundary.progress)
  }
}
