package exastencils.stencil.ir

import exastencils.base.ir._
import exastencils.datastructures._
import exastencils.operator.ir._

/// IR_MapStencilAssignments

object IR_MapStencilAssignments extends DefaultStrategy("Map assignments to stencils and stencil fields") {
  this += new Transformation("SearchAndMark", {
    case IR_Assignment(stencilFieldAccess : IR_StencilFieldAccess, stencilAccess : IR_StencilAccess, op) =>
      ???
    /// FIXME
//      var statements : ListBuffer[IR_Statement] = ListBuffer()
//
//      val stencilRight = stencilAccess.target
//      val offsetsLeft = stencilFieldAccess.target.offsets
//
//      val flipEntries = false
//
//      for (idx <- offsetsLeft.indices) {
//        val fieldSelection = stencilFieldAccess.target.toFieldSelection
//        val fieldIndex = Duplicate(stencilFieldAccess.index.toExpressionIndex)
//        fieldIndex.indices :+= (idx : IR_Expression)
//        var coeff : IR_Expression = 0
//        for (e <- stencilRight.entries) {
//          if (flipEntries) {
//            if (Knowledge.dimensions.map(dim =>
//              IR_SimplifyExpression.evalIntegral(e.offset(dim)) == -IR_SimplifyExpression.evalIntegral(offsetsLeft(idx)(dim)))
//              .reduceLeft((a, b) => a && b))
//              coeff += e.coefficient
//          } else {
//            if (e.offset == offsetsLeft(idx))
//              coeff += e.coefficient
//          }
//        }
//
//        if (flipEntries)
//          for (dim <- 0 until Knowledge.dimensionality)
//            fieldIndex(dim) -= offsetsLeft(idx)(dim)
//
//        statements += IR_Assignment(IR_FieldAccess(fieldSelection, fieldIndex), coeff, op)
//      }
//
//      statements
  })
}
