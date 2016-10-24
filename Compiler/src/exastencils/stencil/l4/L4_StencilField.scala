package exastencils.stencil.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4.L4_Index
import exastencils.field.l4.L4_Field
import exastencils.knowledge.l4.L4_KnowledgeObjectWithLevel
import exastencils.operator.ir.IR_StencilField
import exastencils.prettyprinting._

/// L4_StencilField

object L4_StencilField {
  exastencils.core.Duplicate.dontCloneHierarchy(this.getClass)
}

case class L4_StencilField(
    var name : String, // will be used to find the operator
    var level : Int, // the level the operator lives on
    var field : L4_Field, // linked coefficient field
    var stencilTemplateName : String, // name of the linked stencil (template)
    var offsets : ListBuffer[L4_Index]  // offsets of the stencil (template)
) extends L4_KnowledgeObjectWithLevel[IR_StencilField] {

  def prettyprintDecl(out : PpStream) =
    out << "StencilField " << name << "< " << field.name << " => " << stencilTemplateName << " >" << "@" << level

  override def progressImpl() = IR_StencilField(name, level, field.getProgressedObject(), offsets.map(_.progress.toExpressionIndex))
}
