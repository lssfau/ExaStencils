package exastencils.waLBerla.l2.field

import exastencils.base.l2.L2_Access
import exastencils.base.l2.L2_LevelSpecification
import exastencils.field.l2.L2_FieldAccess
import exastencils.fieldlike.l2.L2_FieldLikeCollection
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.l3.field.L3_WaLBerlaField

case class L2_WaLBerlaFieldFromOther(var name : String, var levels : Option[L2_LevelSpecification], var src : L2_Access) extends L2_WaLBerlaFieldDecl {

  override def prettyprint(out : PpStream) = {
    out << "waLBerla Field" << ' ' << name
    if (levels.isDefined) out << '@' << levels.get
    out << " from" << ' ' << src
  }

  override def addToKnowledge() : Unit = {
    val target = src.asInstanceOf[L2_FieldAccess].target
    val destField = L2_WaLBerlaField(name, target.level, target.datatype, target.numSlots, target.initial, target.boundary)
    L2_WaLBerlaFieldCollection.add(destField)
  }

  override def associatedCollection : L2_FieldLikeCollection[L2_WaLBerlaField, L3_WaLBerlaField] = L2_WaLBerlaFieldCollection
}
