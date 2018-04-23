package exastencils.operator.ir

import exastencils.base.ir._
import exastencils.field.ir.IR_Field
import exastencils.knowledge.ir._

/// IR_StencilField

case class IR_StencilField(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var stencil : IR_Stencil, // linked stencil
    var field : IR_Field // linked coefficient field
) extends IR_LeveledKnowledgeObject {

  override def createDuplicate() = IR_StencilField(name, level, stencil, field)

  def findStencilEntryIndex(offset : IR_ConstIndex) : Option[Int] = stencil.findStencilEntryIndex(offset)
}
