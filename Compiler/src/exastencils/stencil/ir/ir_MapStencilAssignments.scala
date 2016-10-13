package exastencils.stencil.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.field.ir._
import exastencils.optimization.ir.IR_SimplifyExpression

/// IR_MapStencilAssignments

object IR_MapStencilAssignments extends DefaultStrategy("Map assignments to stencils and stencil fields") {
  this += new Transformation("SearchAndMark", {
    case IR_Assignment(stencilFieldAccess : IR_StencilFieldAccess, IR_StencilAccess(stencil), op) =>
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
              IR_SimplifyExpression.evalIntegral(e.offset(dim)) == -IR_SimplifyExpression.evalIntegral(stencilLeft.entries(idx).offset(dim)))
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
  })
}
