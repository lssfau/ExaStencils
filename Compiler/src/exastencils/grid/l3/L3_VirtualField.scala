package exastencils.grid.l3

import exastencils.base.l3._
import exastencils.domain.l3.L3_Domain
import exastencils.grid.l4.L4_VirtualField
import exastencils.knowledge.l3.L3_LeveledKnowledgeObject
import exastencils.prettyprinting.PpStream

/// L3_VirtualField

case class L3_VirtualField(
    var name : String,
    var level : Int,
    var domain : L3_Domain,
    var datatype : L3_Datatype,
    var localization : String) extends L3_LeveledKnowledgeObject[L4_VirtualField] {

  override def prettyprintDecl(out : PpStream) : Unit = ???

  override def progressImpl() = {
    L4_VirtualField(
      name,
      level,
      domain.getProgressedObject(),
      datatype.progress,
      localization)
  }
}
