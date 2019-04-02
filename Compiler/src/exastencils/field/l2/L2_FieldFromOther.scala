package exastencils.field.l2

import exastencils.base.l2._
import exastencils.core.Duplicate
import exastencils.prettyprinting._

/// L2_FieldFromOther

case class L2_FieldFromOther(var name : String, var levels : Option[L2_LevelSpecification], var src : L2_Access) extends L2_FieldDecl {
  override def prettyprint(out : PpStream) = {
    out << "Field" << ' ' << name
    if (levels.isDefined) out << '@' << levels.get
    out << " from" << ' ' << src
  }
  override def addToKnowledge() : Unit = {
    val destField = src.asInstanceOf[L2_FieldAccess].target.createDuplicate()
    destField.name = name
    L2_FieldCollection.add(destField)
  }
}
