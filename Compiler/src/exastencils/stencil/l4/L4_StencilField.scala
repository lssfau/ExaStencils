package exastencils.stencil.l4

import exastencils.field.l4.L4_Field
import exastencils.knowledge.l4.L4_KnowledgeObjectWithLevel
import exastencils.logger.Logger
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
    var stencil : L4_Stencil // FIXME: L4_StencilTemplate // linked stencil template
) extends L4_KnowledgeObjectWithLevel {

  def prettyprintDecl(out : PpStream) = {
    out << "StencilField " << name <<
      "< " << field.name << " => " << stencil.name << " >" << "@" << level << "\n"
  }

  override def progress = {
    progressed = Some(IR_StencilField(name, level, field.getProgressedObject, stencil.getProgressedObject))
    progressed.get
  }

  var progressed : Option[IR_StencilField] = None
  override def getProgressedObject = {
    if (progressed.isEmpty)
      Logger.warn(s"Trying to access invalid progressed object of type ${ this.getClass.getName } with name ${ name }")
    progressed.get
  }
}
