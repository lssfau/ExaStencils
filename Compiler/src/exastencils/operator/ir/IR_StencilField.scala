package exastencils.operator.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ExpressionIndex
import exastencils.config.Knowledge
import exastencils.field.ir.IR_Field
import exastencils.knowledge.ir._
import exastencils.logger.Logger

/// IR_StencilField

case class IR_StencilField(
    var name : String,
    var level : Int,
    var field : IR_Field,
    var offsets : ListBuffer[IR_ExpressionIndex]) extends IR_LeveledKnowledgeObject {

  def findOffsetIndex(offset : IR_ExpressionIndex) : Option[Int] = {
    for (i <- offsets.indices) {
      val hit = Knowledge.dimensions.map(dim => offset(dim) == offsets(i)(dim)).reduce(_ & _)
      if (hit)
        return Some(i)
    }

    Logger.warn(s"Trying to find stencil entry for invalid offset ${ offset.prettyprint() } in stencil template:\n" +
      offsets.map(offset => s"\t${ offset.prettyprint : String } -> ").mkString("\n"))

    None
  }
}
