package exastencils.field.l3

import exastencils.base.l3._
import exastencils.core.Duplicate
import exastencils.prettyprinting._

/// L3_FieldFromOther

case class L3_FieldFromOther(var name : String, var levels : Option[L3_LevelSpecification], var src : L3_Access) extends L3_FieldDecl {
  override def prettyprint(out : PpStream) = {
    out << "Field" << ' ' << name
    if (levels.isDefined) out << '@' << levels.get
    out << " from" << ' ' << src
  }
  override def addToKnowledge() : Unit = {
    val destField = Duplicate.forceClone(src.asInstanceOf[L3_FieldAccess].target)
    destField.name = name
    L3_FieldCollection.add(destField)
  }
}
