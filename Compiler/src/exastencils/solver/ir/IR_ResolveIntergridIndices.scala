package exastencils.solver.ir

import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.core.collectors.IRLevelCollector
import exastencils.datastructures._
import exastencils.field.ir.IR_FieldAccess
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.stencil.ir.IR_StencilFieldAccess
import exastencils.util.ir.IR_ReplaceVariableAccess

/// IR_ResolveIntergridIndices

object IR_ResolveIntergridIndices extends DefaultStrategy("Resolve indices in operations between two grid levels") {
  val collector = new IRLevelCollector
  this.register(collector)

  // TODO: checking for being inside a valid level scope is currently required for setting up geometric information of grids with varying cell sizes
  // TODO: think about if this case (access outside of a loop) should be supported

  this += new Transformation("ModifyIndices", {
    case fct : IR_FunctionCall if "changeLvlAndIndices" == fct.name =>
      // extract information from special function call
      val fieldAccess = fct.arguments(0).asInstanceOf[IR_FieldAccess]
      Logger.warn("Performing index adaptation for " + fieldAccess.fieldSelection.field.codeName)

      // adapt per dimension / (n+1)d is reserved
      for (dim <- 0 until Knowledge.dimensionality) {
        val idxAdaption = fct.arguments(2 + dim)

        // insert old index into index adaptation function
        IR_ReplaceVariableAccess.toReplace = "i"
        IR_ReplaceVariableAccess.replacement = Duplicate(fieldAccess.index(dim))
        val newIdx = Duplicate(idxAdaption)
        IR_ReplaceVariableAccess.applyStandalone(newIdx)

        // overwrite old index
        fieldAccess.index(dim) = newIdx
      }

      fieldAccess

    case access : IR_FieldAccess if collector.inLevelScope &&
      IR_SimplifyExpression.evalIntegral(access.fieldSelection.level) < collector.getCurrentLevel =>
      val fieldAccess = Duplicate(access)
      for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
        fieldAccess.index(i) = fieldAccess.index(i) / 2
      fieldAccess

    case access : IR_FieldAccess if collector.inLevelScope &&
      IR_SimplifyExpression.evalIntegral(access.fieldSelection.level) > collector.getCurrentLevel =>
      val fieldAccess = Duplicate(access)
      for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
        fieldAccess.index(i) = 2 * fieldAccess.index(i)
      fieldAccess

    case access : IR_StencilFieldAccess if collector.inLevelScope &&
      IR_SimplifyExpression.evalIntegral(access.stencilFieldSelection.level) < collector.getCurrentLevel =>
      val stencilFieldAccess = Duplicate(access)
      for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
        stencilFieldAccess.index(i) = stencilFieldAccess.index(i) / 2
      stencilFieldAccess

    case access : IR_StencilFieldAccess if collector.inLevelScope &&
      IR_SimplifyExpression.evalIntegral(access.stencilFieldSelection.level) > collector.getCurrentLevel =>
      val stencilFieldAccess = Duplicate(access)
      for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
        stencilFieldAccess.index(i) = 2 * stencilFieldAccess.index(i)
      stencilFieldAccess
  }, false /* don't do this recursively -> avoid double adaptation for cases using special functions */)
}

