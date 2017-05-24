package exastencils.grid.l4

import exastencils.base.l4._
import exastencils.domain.l4.L4_Domain
import exastencils.grid.ir.IR_VirtualField
import exastencils.knowledge.l4.L4_LeveledKnowledgeObject
import exastencils.prettyprinting.PpStream

/// L4_VirtualField

case class L4_VirtualField(
    var name : String,
    var level : Int,
    var domain : L4_Domain,
    var datatype : L4_Datatype,
    var localization : String) extends L4_LeveledKnowledgeObject[IR_VirtualField] {

  override def prettyprintDecl(out : PpStream) : Unit = ???

  override def progressImpl() = {
    IR_VirtualField(
      name,
      level,
      domain.getProgressedObj(),
      datatype.progress,
      localization)
  }
}
