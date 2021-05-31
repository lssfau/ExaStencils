package exastencils.waLBerla.l4

import exastencils.base.l4.L4_Access
import exastencils.base.l4.L4_DeclarationLevelSpecification
import exastencils.baseExt.l4.L4_MatShape
import exastencils.field.l4.L4_FieldDecl
import exastencils.prettyprinting.PpStream

case class L4_WaLBerlaFieldDecl(
    var name : String,
    var levels : Option[L4_DeclarationLevelSpecification],
    var fieldLayout : L4_Access,
    var matShape : Option[L4_MatShape] = None
) extends L4_FieldDecl {


  override def prettyprint(out : PpStream) : Unit = {
    out << "waLBerla Field " << name << "< " << fieldLayout.name << ">"
    if (levels.isDefined) out << '@' << levels.get
    if (matShape.isDefined) out << matShape.get.toString()
  }

  override def addToKnowledge() : Unit = {
    val index = L4_FieldDecl.runningIndex
    L4_FieldDecl.runningIndex += 1

    L4_WaLBerlaFieldCollection.add(
      L4_WaLBerlaField(
        name,
        levels.get.resolveLevel,
        index,
        fieldLayout.asInstanceOf[L4_WaLBerlaFieldLayoutAccess].target,
        matShape
      ))
  }
}