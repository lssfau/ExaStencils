package exastencils.waLBerla.l3.field

import exastencils.base.l3.L3_Access
import exastencils.base.l3.L3_LevelSpecification
import exastencils.field.l3.L3_FieldAccess
import exastencils.fieldlike.l3.L3_FieldLikeCollection
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.l4.field.L4_WaLBerlaField

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

  override def associatedCollection : L3_FieldLikeCollection[L3_WaLBerlaField, L4_WaLBerlaField] = L3_WaLBerlaFieldCollection
}
