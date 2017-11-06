package exastencils.field.l1

import exastencils.base.l1._
import exastencils.base.l2.L2_RealDatatype
import exastencils.boundary.l1.L1_BoundaryCondition
import exastencils.domain.l1.L1_Domain
import exastencils.field.l2.L2_Field
import exastencils.grid.l1.L1_Localization
import exastencils.knowledge.l1.L1_LeveledKnowledgeObject
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L1_Field

case class L1_Field(
    var name : String,
    var level : Int,
    var domain : L1_Domain,
    var initial : Option[L1_Expression],
    var boundary : L1_BoundaryCondition) extends L1_LeveledKnowledgeObject[L2_Field] {

  override def prettyprintDecl(out : PpStream) : Unit = ???

  def codeName = name + "_" + level
  def numDimsGrid = domain.numDims

  var localization : Option[L1_Localization] = None

  override def progressImpl() = {
    if (localization.isEmpty) Logger.error("Trying to progress L1 field without localization")
    L2_Field(
      name,
      level,
      domain.getProgressedObj(),
      L2_RealDatatype /*FIXME*/ ,
      localization.get.progress,
      L1_ProgressOption(initial)(_.progress),
      boundary.progress)
  }
}
