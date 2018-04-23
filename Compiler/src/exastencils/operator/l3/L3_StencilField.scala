package exastencils.operator.l3

import exastencils.field.l3.L3_Field
import exastencils.knowledge.l3._
import exastencils.operator.l4.L4_StencilField
import exastencils.prettyprinting.PpStream

/// L3_StencilField

case class L3_StencilField(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var stencil : L3_Stencil, // linked stencil
    var field : L3_Field // linked coefficient field
) extends L3_LeveledKnowledgeObject[L4_StencilField] {

  override def createDuplicate() = L3_StencilField(name, level, stencil, field)

  override def prettyprintDecl(out : PpStream) : Unit = ???

  override def progressImpl() = L4_StencilField(name, level, stencil.getProgressedObj(), field.getProgressedObj())
}
