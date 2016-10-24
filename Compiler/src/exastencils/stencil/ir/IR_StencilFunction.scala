package exastencils.stencil.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.core.collectors.StackCollector
import exastencils.datastructures._
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.field.ir.IR_FieldAccess
import exastencils.logger.Logger

/// IR_StencilFunctionAccess

case class IR_StencilFunctionAccess(var name : String, var datatype : IR_Datatype) extends IR_FunctionAccess {}

/// IR_ResolveStencilFunction

object IR_ResolveStencilFunction extends DefaultStrategy("Resolve stencil functions") {
  var collector = new StackCollector
  this.register(collector)

  this += new Transformation("Resolve", {
    case IR_FunctionCall(IR_StencilFunctionAccess(fctName, _), args) =>
      fctName match {
        // diag function
        case "diag" =>
          args(0) match {
            case access : IR_StencilAccess =>
              // stencil access => find entry with 0 offset and return coefficient
              val centralOffset = IR_ExpressionIndex(Array.fill(Knowledge.dimensionality)(0))
              access.stencil.findStencilEntry(centralOffset).get.coefficient

            case access : IR_StencilFieldAccess =>
              // stencil field access => find entry with 0 offset in linked stencil and compile field access
              val index = Duplicate(access.index)
              val centralOffset = IR_ExpressionIndex(Array.fill(Knowledge.dimensionality)(0))
              index(Knowledge.dimensionality) = access.stencilFieldSelection.stencilField.findOffsetIndex(centralOffset).get

              IR_FieldAccess(IR_FieldSelection(access.stencilFieldSelection.field, access.stencilFieldSelection.level, access.stencilFieldSelection.slot, Some(0), access.stencilFieldSelection.fragIdx), index)

            case _ =>
              Logger.warn("diag with unknown arg " + args(0))
              IR_FunctionCall(fctName, args)
          }

        case _ =>
          Logger.warn("stencil function with unknown name " + fctName)
          IR_FunctionCall(fctName, args)
      }
  })
}
