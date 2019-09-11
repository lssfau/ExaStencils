package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_FieldIteratorAccess
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.field.ir.IR_Field

/// IR_Communicate

case class IR_Communicate(
    // FIXME: incorporate component accesses
    var field : IR_Field,
    var slot : IR_Expression,
    var op : String,
    var targets : ListBuffer[IR_CommunicateTarget],
    var condition : Option[IR_Expression]) extends IR_Statement with IR_SpecialExpandable {

  // shift all index accesses in condition as later functions will generate direct field accesses and according loop bounds
  // TODO: extract to separate transformation
  if (condition.isDefined) ShiftIndexAccesses.applyStandalone(IR_ExpressionStatement(condition.get))

  // TODO: extract strategy - field package?
  object ShiftIndexAccesses extends QuietDefaultStrategy("Shifting index accesses") {
    this += new Transformation("SearchAndReplace", {
      case access : IR_VariableAccess =>
        var ret : IR_Expression = access
        val numDims = field.fieldLayout.numDimsData
        for (dim <- 0 until numDims)
          if (IR_FieldIteratorAccess(dim) == access)
            ret = IR_FieldIteratorAccess(dim) - field.referenceOffset(dim)
        ret
    }, false)
  }

}

/// IR_CommunicateTarget

// FIXME: IR_ExpressionIndex -> IR_Index
case class IR_CommunicateTarget(var target : String, var begin : Option[IR_ExpressionIndex], var end : Option[IR_ExpressionIndex]) extends IR_Node {
  if (begin.isDefined && !end.isDefined) // create end if only one 'index' is to be communicated
    end = Some(Duplicate(begin.get) + IR_ConstIndex(Array.fill(begin.get.length)(1)))
}
