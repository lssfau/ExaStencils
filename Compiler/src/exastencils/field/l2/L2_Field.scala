package exastencils.field.l2

import exastencils.base.l2._
import exastencils.boundary.l2.L2_BoundaryCondition
import exastencils.domain.l2.L2_Domain
import exastencils.field.l3.L3_Field
import exastencils.grid.l2.L2_Localization
import exastencils.knowledge.l2.L2_LeveledKnowledgeObject
import exastencils.prettyprinting.PpStream

/// L2_Field

object L2_Field {
  def apply(name : String, level : Int, domain : L2_Domain, datatype : L2_Datatype, localization : L2_Localization,
      initial : Option[L2_Expression], boundary : L2_BoundaryCondition) =
    new L2_Field(name, level, domain, datatype, localization, 1, initial, boundary)
}

case class L2_Field(
    var name : String,
    var level : Int,
    var domain : L2_Domain,
    var datatype : L2_Datatype,
    var localization : L2_Localization,
    var numSlots : Int,
    var initial : Option[L2_Expression],
    var boundary : L2_BoundaryCondition) extends L2_LeveledKnowledgeObject[L3_Field] {

  override def prettyprintDecl(out : PpStream) : Unit = ???

  def codeName = name + "_" + level
  def numDimsGrid = domain.numDims

  override def progressImpl() = {
    L3_Field(
      name,
      level,
      domain.getProgressedObj(),
      datatype.progress,
      localization.progress,
      numSlots,
      L2_ProgressOption(initial)(_.progress),
      boundary.progress)
  }
}
