package exastencils.field.l2

import exastencils.base.l2._
import exastencils.boundary.l2.L2_BoundaryCondition
import exastencils.domain.l2.L2_Domain
import exastencils.field.l3.L3_Field
import exastencils.knowledge.l2.L2_KnowledgeObjectWithLevel
import exastencils.prettyprinting.PpStream

/// L2_Field

object L2_Field {
  exastencils.core.Duplicate.dontCloneHierarchy(this.getClass)
}

case class L2_Field(
    var name : String,
    var level : Int,
    var domain : L2_Domain,
    var datatype : L2_Datatype,
    var localization : String,
    var initial : Option[L2_Expression],
    var boundary : L2_BoundaryCondition) extends L2_KnowledgeObjectWithLevel[L3_Field] {

  def fieldLayoutName = s"defLayout$localization"
  override def prettyprintDecl(out : PpStream) : Unit = ???

  override def progressImpl() = {
    L3_Field(
      name,
      level,
      domain.name,
      datatype.progress,
      localization,
      L2_ProgressOption(initial)(_.progress),
      boundary.progress)
  }
}
