package exastencils.knowledge

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.field.ir._
import exastencils.stencil.ir._
import exastencils.util._

object FindStencilConvolutions extends DefaultStrategy("FindStencilConvolutions") {
  var changed : Boolean = false

  def transformMultiplication(exp : IR_MultiplicationExpression) : IR_MultiplicationExpression = {
    val facts : ListBuffer[IR_Expression] = exp.factors
    var result = new ListBuffer[IR_Expression]()
    var prev : IR_Expression = null

    // check for StencilLike * FieldLike
    result = ListBuffer[IR_Expression]()
    prev = null
    for (f <- facts)
      (prev, f) match {
        case (IR_StencilAccess(stencil), fieldAccess : IR_FieldAccess)                  =>
          result += IR_StencilConvolution(stencil, fieldAccess)
          prev = null
        case (stencilFieldAccess : IR_StencilFieldAccess, fieldAccess : IR_FieldAccess) =>
          result += IR_StencilFieldConvolution(stencilFieldAccess, fieldAccess)
          prev = null
        case _                                                                          =>
          if (prev != null) result += prev
          prev = f
      }
    if (prev != null)
      result += prev
    changed |= facts.length != result.length
    if (facts.length != result.length)
      return IR_MultiplicationExpression(result)

    // check for StencilLike * Stencil
    result = ListBuffer[IR_Expression]()
    prev = null
    for (f <- facts)
      (prev, f) match {
        case (IR_StencilAccess(stencilLeft), IR_StencilAccess(stencilRight))       =>
          result += IR_StencilStencilConvolution(stencilLeft, stencilRight)
          prev = null
        case (stencilLeft : IR_StencilFieldAccess, IR_StencilAccess(stencilRight)) =>
          result += IR_StencilFieldStencilConvolution(stencilLeft, stencilRight)
          prev = null
        case _                                                                     =>
          if (prev != null) result += prev
          prev = f
      }
    if (prev != null)
      result += prev
    changed |= facts.length != result.length
    if (facts.length != result.length)
      return IR_MultiplicationExpression(result)

    // check for other convolutions
    result = ListBuffer[IR_Expression]()
    prev = null
    for (f <- facts)
      (prev, f) match {
        case (IR_StencilAccess(stencilLeft), stencilRight : IR_StencilFieldAccess)       =>
          ??? // TODO
        case (stencilLeft : IR_StencilFieldAccess, stencilRight : IR_StencilFieldAccess) =>
          ??? // TODO
        case _                                                                           =>
          if (prev != null) result += prev
          prev = f
      }
    if (prev != null)
      result += prev
    changed |= facts.length != result.length
    if (facts.length != result.length)
      return IR_MultiplicationExpression(result)

    exp
  }

  this += new Transformation("SearchAndMark", {
    case exp : IR_MultiplicationExpression => {
      val newMult = transformMultiplication(exp)
      newMult.factors.size match {
        case 0 => IR_NullExpression
        case 1 => newMult.factors.head
        case _ => newMult
      }
    }
  })
}

object MapStencilAssignments extends DefaultStrategy("MapStencilAssignments") {
  this += new Transformation("SearchAndMark", {
    case IR_Assignment(stencilFieldAccess : IR_StencilFieldAccess, IR_StencilAccess(stencil), op) => {
      var statements : ListBuffer[IR_Statement] = ListBuffer()

      val stencilRight = stencil
      val stencilLeft = stencilFieldAccess.stencilFieldSelection.stencil

      val flipEntries = false

      for (idx <- stencilLeft.entries.indices) {
        val fieldSelection = stencilFieldAccess.stencilFieldSelection.toFieldSelection
        fieldSelection.arrayIndex = Some(idx)
        val fieldIndex = Duplicate(stencilFieldAccess.index)
        fieldIndex(Knowledge.dimensionality) = idx
        var coeff : IR_Expression = 0
        for (e <- stencilRight.entries) {
          if (flipEntries) {
            if ((0 until Knowledge.dimensionality).map(dim =>
              SimplifyExpression.evalIntegral(e.offset(dim)) == -SimplifyExpression.evalIntegral(stencilLeft.entries(idx).offset(dim)))
              .reduceLeft((a, b) => a && b))
              coeff += e.coefficient
          } else {
            if (e.offset == stencilLeft.entries(idx).offset)
              coeff += e.coefficient
          }
        }

        if (flipEntries)
          for (dim <- 0 until Knowledge.dimensionality)
            fieldIndex(dim) -= stencilLeft.entries(idx).offset(dim)

        statements += IR_Assignment(IR_FieldAccess(fieldSelection, fieldIndex), coeff, op)
      }

      statements
    }
  })
}
