package exastencils.parallelization.api.cuda

import scala.collection.mutable.HashMap

import exastencils.base.ir._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.deprecated.ir._
import exastencils.field.ir._

/// CUDA_GatherLinearizedFieldAccess

object CUDA_GatherLinearizedFieldAccess extends QuietDefaultStrategy("Gather local LinearizedFieldAccess nodes") {
  var fieldAccesses = HashMap[String, IR_LinearizedFieldAccess]()

  def clear() = fieldAccesses.clear()

  def mapFieldAccess(access : IR_LinearizedFieldAccess) = {
    val field = access.fieldSelection.field
    var identifier = field.codeName

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
    case access : IR_LinearizedFieldAccess =>
      mapFieldAccess(access)
      access
  }, false)
}

/// CUDA_ReplaceLinearizedFieldAccess

object CUDA_ReplaceLinearizedFieldAccess extends QuietDefaultStrategy("Replace local LinearizedFieldAccess nodes") {
  var fieldAccesses = HashMap[String, IR_LinearizedFieldAccess]()

  def extractIdentifier(access : IR_LinearizedFieldAccess) = {
    val field = access.fieldSelection.field
    var identifier = field.codeName

    // TODO: array fields
    if (field.numSlots > 1) {
      access.fieldSelection.slot match {
        case IR_SlotAccess(_, offset) => identifier += s"_o$offset"
        case IR_IntegerConstant(slot) => identifier += s"_s$slot"
        case _                        => identifier += s"_s${ access.fieldSelection.slot.prettyprint }"
      }
    }

    IR_VariableAccess(identifier, IR_PointerDatatype(field.resolveDeclType))
  }

  this += new Transformation("Searching", {
    case access : IR_LinearizedFieldAccess =>
      val identifier = extractIdentifier(access)
      IR_ArrayAccess(identifier, access.index)
  })
}

/// CUDA_GatherLinearizedFieldAccessWrites

object CUDA_GatherLinearizedFieldAccessWrites extends QuietDefaultStrategy("Gather local write accesses to LinearizedFieldAccess nodes for read-only cache usage") {
  var writtenFieldAccesses = HashMap[String, IR_LinearizedFieldAccess]()

  def mapFieldAccess(access : IR_LinearizedFieldAccess) = {
    val field = access.fieldSelection.field
    var identifier = field.codeName

    // TODO: array fields
    if (field.numSlots > 1) {
      access.fieldSelection.slot match {
        case IR_SlotAccess(_, offset) => identifier += s"_o$offset"
        case IR_IntegerConstant(slot) => identifier += s"_s$slot"
        case _                        => identifier += s"_s${ access.fieldSelection.slot.prettyprint }"
      }
    }

    writtenFieldAccesses.put(identifier, access)
  }

  this += new Transformation("Searching", {
    case stmt @ IR_Assignment(access @ IR_LinearizedFieldAccess(fieldSelection : IR_FieldSelection, index : IR_Expression), _, _) =>
      mapFieldAccess(access)
      stmt
  })
}
