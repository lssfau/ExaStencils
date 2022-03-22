package exastencils.waLBerla.l3

import exastencils.base.l3._
import exastencils.field.l3.L3_FieldAccess
import exastencils.prettyprinting.PpStream

/// L3_WaLBerlaFieldFromOther

case class L3_WaLBerlaFieldFromOther(var name : String, var levels : Option[L3_LevelSpecification], var src : L3_Access) extends L3_WaLBerlaFieldDecl {

  override def prettyprint(out : PpStream) = {
    out << "waLBerla Field" << ' ' << name
    if (levels.isDefined) out << '@' << levels.get
    out << " from" << ' ' << src
  }

  override def addToKnowledge() : Unit = {
    val target = src.asInstanceOf[L3_FieldAccess].target
    val destField = L3_WaLBerlaField(name, target.level, target.datatype, target.numSlots, target.initial, target.boundary)
    L3_WaLBerlaFieldCollection.add(destField)
  }
}
