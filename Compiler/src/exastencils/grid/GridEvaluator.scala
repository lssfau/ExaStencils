package exastencils.grid

import exastencils.base.ir._
import exastencils.config._
import exastencils.field.ir.IR_FieldAccess
import exastencils.logger._

/// GridEvaluator

object GridEvaluator {
  def getEvaluator = {
    if (Knowledge.grid_isAxisAligned) GridEvaluator_AxisAligned
    else Logger.error("Evaluators for non-axis-aligned grids are currently not supported")
  }
}

abstract class GridEvaluator() {
  def invokeEvalResolve(functionName : String, fieldAccess : IR_FieldAccess, interpolation : String) : IR_Expression = {
    val method = this.getClass.getMethods.find(_.getName == functionName)
    if (method.isEmpty) Logger.debug(s"Trying to access invalid method $functionName")
    method.get.invoke(this, fieldAccess, interpolation).asInstanceOf[IR_Expression]
  }

  def invokeIntegrateResolve(functionName : String, exp : IR_Expression) : IR_Expression = {
    val method = this.getClass.getMethods.find(_.getName == functionName)
    if (method.isEmpty) Logger.debug(s"Trying to access invalid method $functionName")
    method.get.invoke(this, exp).asInstanceOf[IR_Expression]
  }
}
