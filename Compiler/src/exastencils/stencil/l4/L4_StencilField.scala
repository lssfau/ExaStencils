package exastencils.stencil.l4

import exastencils.field.l4.L4_Field
import exastencils.knowledge.l4.L4_KnowledgeObjectWithLevel
import exastencils.prettyprinting._
import exastencils.stencil.ir.IR_StencilField

/// L4_StencilField

object L4_StencilField {
  exastencils.core.Duplicate.dontCloneHierarchy(this.getClass)
}

case class L4_StencilField(
    var name : String, // will be used to find the operator
    var level : Int, // the level the operator lives on
    var field : L4_Field, // linked coefficient field
    var stencil : L4_Stencil // linked stencil template // TODO: var stencil : L4_StencilTemplate
) extends L4_KnowledgeObjectWithLevel[IR_StencilField] {

  def prettyprintDecl(out : PpStream) = {
    out << "StencilField " << name <<
      "< " << field.name << " => " << stencil.name << " >" << "@" << level << "\n"
  }

  override def progressImpl() = IR_StencilField(name, level, field.getProgressedObject, stencil.getProgressedObject)
}
