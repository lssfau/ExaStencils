package exastencils.field.l3

import scala.collection.mutable.ListBuffer

import exastencils.core.Duplicate
import exastencils.field.l4.L4_FieldCombination
import exastencils.knowledge.l3.L3_LeveledKnowledgeObject
import exastencils.prettyprinting.PpStream

/// L3_FieldCombination

case class L3_FieldCombination(
    var name : String,
    var level : Int,
    var combinationType : String,
    var fields : ListBuffer[L3_Field]) extends L3_LeveledKnowledgeObject[L4_FieldCombination] {

  override def createDuplicate() : L3_FieldCombination = {
    L3_FieldCombination.tupled(Duplicate(L3_FieldCombination.unapply(this).get))
  }

  override def prettyprintDecl(out : PpStream) = {
    out << "FieldCombination " << name << '@' << level << " : " << combinationType << " = " << fields.map(_.name).mkString(", ")
  }

  override def progressImpl() = {
    L4_FieldCombination(name, level, combinationType, fields.map(_.getProgressedObj()))
  }
}
