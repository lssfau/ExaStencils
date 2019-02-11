package exastencils.field.l2

import scala.collection.mutable.ListBuffer

import exastencils.core.Duplicate
import exastencils.field.l3.L3_FieldCombination
import exastencils.knowledge.l2.L2_LeveledKnowledgeObject
import exastencils.prettyprinting.PpStream

/// L2_FieldCombination

case class L2_FieldCombination(
    var name : String,
    var level : Int,
    var combinationType : String,
    var fields : ListBuffer[L2_Field]) extends L2_LeveledKnowledgeObject[L3_FieldCombination] {

  override def createDuplicate() : L2_FieldCombination = {
    L2_FieldCombination.tupled(Duplicate(L2_FieldCombination.unapply(this).get))
  }

  override def prettyprintDecl(out : PpStream) = {
    out << "FieldCombination " << name << '@' << level << " : " << combinationType << " = " << fields.map(_.name).mkString(", ")
  }

  override def progressImpl() = {
    L3_FieldCombination(name, level, combinationType, fields.map(_.getProgressedObj()))
  }
}
