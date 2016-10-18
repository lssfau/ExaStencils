package exastencils.field.l3

import exastencils.base.l3._
import exastencils.field.l4.L4_Field
import exastencils.knowledge.l3.L3_KnowledgeObjectWithLevel

/// L3_Field

//object L3_Field {
//  exastencils.core.Duplicate.dontCloneHierarchy(this.getClass)
//}
//
//case class L3_Field(
//    var name : String,
//    var level : Int,
//    var domainName : String, // TODO: var domain : L3_Domain
//    var datatype : L3_Datatype,
//    var localization : String,
//    var initial : Option[L3_Expression],
//    var boundary : L3_BoundaryCondition) extends L3_KnowledgeObjectWithLevel {
//
//  def fieldLayoutName = s"defLayout$localization"
//
//  def progress : L4_Field = {
//    L4_Field(
//      name,
//      level,
//      -1, // index is to be set later
//      domainName,
//      fieldLayoutName,
//      1, // one slot by default - may be increased later
//      boundary.progress)
//  }
//}
