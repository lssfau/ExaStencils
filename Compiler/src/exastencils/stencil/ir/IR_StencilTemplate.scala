package exastencils.stencil.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Index
import exastencils.domain.ir.IR_Domain
import exastencils.knowledge.ir.IR_KnowledgeObjectWithLevel

/// IR_StencilTemplate

object IR_StencilTemplate {
  exastencils.core.Duplicate.dontCloneHierarchy(this.getClass)
}

case class IR_StencilTemplate(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var domain : IR_Domain, // domain the stencil lives on
    var offsets : ListBuffer[IR_Index]) extends IR_KnowledgeObjectWithLevel {}
