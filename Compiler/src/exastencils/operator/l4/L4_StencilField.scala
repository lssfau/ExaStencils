package exastencils.operator.l4

import exastencils.field.l4.L4_Field
import exastencils.knowledge.l4._
import exastencils.operator.ir.IR_StencilField
import exastencils.prettyprinting.PpStream

/// L4_StencilField

case class L4_StencilField(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var stencil : L4_Stencil, // linked stencil
    var field : L4_Field // linked coefficient field
) extends L4_LeveledKnowledgeObject[IR_StencilField] {

  override def createDuplicate() = L4_StencilField(name, level, stencil, field)

  override def prettyprintDecl(out : PpStream) = {
    out << "StencilField " << name << "< " << field.name << " => " << stencil.name << " >" << '@' << level
  }

  override def progressImpl() = IR_StencilField(name, level, stencil.getProgressedObj(), field.getProgressedObj())
}
