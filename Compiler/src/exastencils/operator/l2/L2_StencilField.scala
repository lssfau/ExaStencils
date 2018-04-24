package exastencils.operator.l2

import exastencils.field.l2.L2_Field
import exastencils.knowledge.l2._
import exastencils.operator.l3.L3_StencilField
import exastencils.prettyprinting.PpStream

/// L2_StencilField

case class L2_StencilField(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var stencil : L2_Stencil, // linked stencil
    var field : L2_Field // linked coefficient field
) extends L2_LeveledKnowledgeObject[L3_StencilField] {

  override def createDuplicate() = L2_StencilField(name, level, stencil, field)

  override def prettyprintDecl(out : PpStream) : Unit = {
    out << "Operator " << name << "@" << level << " from StencilTemplate on " << field.localization << " of " << field.domain << " {\n"
    stencil.entries.foreach(out << _.asStencilOffsetEntry.offset << " => \n")
    out << "}"
  }

  override def progressImpl() = L3_StencilField(name, level, stencil.getProgressedObj(), field.getProgressedObj())
}
