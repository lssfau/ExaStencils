package exastencils.waLBerla.l4.field

import exastencils.base.l4.L4_Access
import exastencils.base.l4.L4_DeclarationLevelSpecification
import exastencils.baseExt.l4.L4_MatShape
import exastencils.boundary.l4.L4_BoundaryCondition
import exastencils.boundary.l4.L4_NoBC
import exastencils.field.l4.L4_FieldDecl
import exastencils.prettyprinting.PpStream

case class L4_WaLBerlaBaseFieldDecl(
    var name : String,
    var levels : Option[L4_DeclarationLevelSpecification],
    var fieldLayout : L4_Access,
    var boundary : Option[L4_BoundaryCondition],
    var numSlots : Int,
    var matShape : Option[L4_MatShape] = None
) extends L4_WaLBerlaFieldDecl {

  override def prettyprint(out : PpStream) : Unit = {
    out << "waLBerla Field " << name << "< " << fieldLayout.name << "," << boundary.getOrElse(L4_NoBC) << ">"
    if (numSlots > 1) out << '[' << numSlots << ']'
    if (levels.isDefined) out << '@' << levels.get
    if (matShape.isDefined) out << matShape.get.toString()
  }

  override def addToKnowledge() : Unit = {
    val index = L4_FieldDecl.runningIndex
    L4_FieldDecl.runningIndex += 1

    val wbField = L4_WaLBerlaField(name, levels.get.resolveLevel, index, fieldLayout.asInstanceOf[L4_WaLBerlaFieldLayoutAccess].target, numSlots, boundary.getOrElse(L4_NoBC), matShape)
    L4_WaLBerlaFieldCollection.add(wbField)
  }
}
