package exastencils.cuda

import scala.collection.mutable._

import exastencils.base.ir._
import exastencils.datastructures._
import exastencils.field.ir._

object CUDA_GatherFieldAccess extends QuietDefaultStrategy("Gather local FieldAccess nodes") {
  var fieldAccesses = HashMap[String, IR_MultiDimFieldAccess]()
  var inWriteOp = false

  def mapFieldAccess(access : IR_MultiDimFieldAccess) = {
    val field = access.fieldSelection.field
    var identifier = field.codeName

    identifier = (if (inWriteOp) "write_" else "read_") + identifier

    // TODO: array fields
    if (field.numSlots > 1) {
      access.fieldSelection.slot match {
        case IR_SlotAccess(_, offset) => identifier += s"_o$offset"
        case IR_IntegerConstant(slot) => identifier += s"_s$slot"
        case _                        => identifier += s"_s${ access.fieldSelection.slot.prettyprint }"
      }
    }

    fieldAccesses.put(identifier, access)
  }

  this += new Transformation("Searching", {
    case assign : IR_Assignment          =>
      inWriteOp = true
      CUDA_GatherFieldAccess.applyStandalone(IR_ExpressionStatement(assign.dest))
      inWriteOp = false
      if ("=" != assign.op) // add compound assignments as read and write accesses
        CUDA_GatherFieldAccess.applyStandalone(IR_ExpressionStatement(assign.dest))
      CUDA_GatherFieldAccess.applyStandalone(IR_ExpressionStatement(assign.src))
      assign
    case access : IR_MultiDimFieldAccess =>
      mapFieldAccess(access)
      access
  }, false)
}
