package exastencils.communication.ir

import scala.collection.mutable._

import exastencils.base.ir.IR_ExpressionIndex
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.datastructures._
import exastencils.field.ir._

/// TODO: IR_SplitLoopsForCommunication

/// IR_GatherFieldAccessOffsets

object IR_GatherFieldAccessOffsets extends QuietDefaultStrategy("Gather field access offsets honoring reference offsets") {
  var accesses = HashMap[String, ListBuffer[IR_ExpressionIndex]]()

  def addAccess(key : String, index : IR_ExpressionIndex) = {
    if (!accesses.contains(key)) accesses.put(key, ListBuffer())
    accesses(key) += index
  }

  this += new Transformation("Gather", {
    case fa : IR_FieldAccess        =>
      addAccess(fa.field.codeName, fa.index - IR_LoopOverDimensions.defIt(fa.index.length))
      fa
    case dfa : IR_DirectFieldAccess =>
      addAccess(dfa.field.codeName, dfa.index - dfa.field.referenceOffset - IR_LoopOverDimensions.defIt(dfa.index.length))
      dfa
  })
}
