package exastencils.field.l3

import exastencils.base.l3._
import exastencils.core._
import exastencils.datastructures._
import exastencils.field.l2.L2_FieldCollection
import exastencils.knowledge.l3.L3_FieldCollection
import exastencils.logger._
import exastencils.prettyprinting._

/// L3_FieldDecl

trait L3_FieldDecl extends L3_Statement {
  def name : String
  override def progress = Logger.error(s"Trying to progress l3 field declaration for field $name; this is not supported")
}

/// L3_FieldFromL2

case class L3_FieldFromL2(var name : String, var levels : Option[L3_LevelSpecification]) extends L3_FieldDecl {
  override def prettyprint(out : PpStream) = out << "Field" << ' ' << name << "@" << levels << ' ' << "from" << ' ' << "L2"
}

/// L3_FieldFromOther

case class L3_FieldFromOther(var name : String, var levels : Option[L3_LevelSpecification], var srcIdentifier : String) extends L3_FieldDecl {
  override def prettyprint(out : PpStream) = out << "Field" << ' ' << name << "@" << levels << ' ' << "from" << ' ' << srcIdentifier
}

/// L3_ProcessFieldDeclarations

object L3_ProcessFieldDeclarations extends DefaultStrategy("Integrate Layer3 field declarations with knowledge") {
  this += Transformation("Take over l2 fields", {
    case field : L3_FieldFromL2 =>
      val levelList = L3_LevelSpecification.extractLevelListDefAll(field.levels)
      for (level <- levelList) {
        val l2Field = L2_FieldCollection.getByIdentifier(field.name, level, true)
        if (l2Field.isEmpty)
          Logger.warn(s"Trying to take over l2 field ${ field.name } for level $level - not found")
        else
          L3_FieldCollection.add(l2Field.get.progress())
      }

      None // consume declaration statement
  })

  this += Transformation("Process field copies", {
    case field : L3_FieldFromOther =>
      val levelList = L3_LevelSpecification.extractLevelListDefAll(field.levels)
      for (level <- levelList) {
        val l3Field = L3_FieldCollection.getByIdentifier(field.srcIdentifier, level, true)
        if (l3Field.isEmpty)
          Logger.warn(s"Trying to copy l3 field ${ field.name } for level $level - not found")
        else {
          val destField = Duplicate(l3Field.get)
          destField.name = field.name
          L3_FieldCollection.add(destField)
        }
      }

      None // consume declaration statement
  })
}
