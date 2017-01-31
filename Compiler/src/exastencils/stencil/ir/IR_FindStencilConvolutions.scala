package exastencils.stencil.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.field.ir._

/// IR_FindStencilConvolutions

object IR_FindStencilConvolutions extends DefaultStrategy("Find and mark stencil-stencil and stencil-field convolutions") {
  var changed : Boolean = false

  def transformMultiplication(exp : IR_Multiplication) : IR_Multiplication = {
    val facts : ListBuffer[IR_Expression] = exp.factors
    var result = new ListBuffer[IR_Expression]()
    var prev : IR_Expression = null

    // check for StencilLike * FieldLike
    result = ListBuffer[IR_Expression]()
    prev = null
    for (f <- facts)
      (prev, f) match {
        case (left : IR_StencilAccess, right : IR_FieldAccess)      =>
          result += IR_StencilConvolution(left, right)
          prev = null
        case (left : IR_StencilFieldAccess, right : IR_FieldAccess) =>
          result += IR_StencilFieldConvolution(left, right)
          prev = null
        case _                                                      =>
          if (prev != null) result += prev
          prev = f
      }
    if (prev != null)
      result += prev
    changed |= facts.length != result.length
    if (facts.length != result.length)
      return IR_Multiplication(result)

    // check for StencilLike * Stencil
    result = ListBuffer[IR_Expression]()
    prev = null
    for (f <- facts)
      (prev, f) match {
        case (left : IR_StencilAccess, right : IR_StencilAccess)      =>
          result += IR_StencilStencilConvolution(left, right)
          prev = null
        case (left : IR_StencilFieldAccess, right : IR_StencilAccess) =>
          result += IR_StencilFieldStencilConvolution(left, right)
          prev = null
        case _                                                        =>
          if (prev != null) result += prev
          prev = f
      }
    if (prev != null)
      result += prev
    changed |= facts.length != result.length
    if (facts.length != result.length)
      return IR_Multiplication(result)

    // check for other convolutions
    result = ListBuffer[IR_Expression]()
    prev = null
    for (f <- facts)
      (prev, f) match {
        case (left : IR_StencilAccess, right : IR_StencilFieldAccess)      =>
          ??? // TODO
        case (left : IR_StencilFieldAccess, right : IR_StencilFieldAccess) =>
          ??? // TODO
        case _                                                             =>
          if (prev != null) result += prev
          prev = f
      }
    if (prev != null)
      result += prev
    changed |= facts.length != result.length
    if (facts.length != result.length)
      return IR_Multiplication(result)

    exp
  }

  this += new Transformation("SearchAndMark", {
    case exp : IR_Multiplication =>
      val newMult = transformMultiplication(exp)
      newMult.factors.size match {
        case 0 => IR_NullExpression
        case 1 => newMult.factors.head
        case _ => newMult
      }
  })
}
