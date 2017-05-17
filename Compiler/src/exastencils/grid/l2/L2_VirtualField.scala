package exastencils.grid.l2

import exastencils.base.l2._
import exastencils.domain.l2.L2_Domain
import exastencils.grid.l3.L3_VirtualField
import exastencils.knowledge.l2.L2_LeveledKnowledgeObject
import exastencils.prettyprinting.PpStream

/// L2_VirtualField

case class L2_VirtualField(
    var name : String,
    var level : Int,
    var domain : L2_Domain,
    var datatype : L2_Datatype,
    var localization : String) extends L2_LeveledKnowledgeObject[L3_VirtualField] {

  override def prettyprintDecl(out : PpStream) : Unit = ???

  override def progressImpl() = {
    L3_VirtualField(
      name,
      level,
      domain.getProgressedObject(),
      datatype.progress,
      localization)
  }
}
