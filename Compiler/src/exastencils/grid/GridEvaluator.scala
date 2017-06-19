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
  // use Integer since object is required
  def invokeEvalResolve(functionName : String, level : Integer, fieldAccess : IR_FieldAccess, interpolation : String, offset : Option[IR_ConstIndex]) : IR_Expression = {
    val method = this.getClass.getMethods.find(_.getName == functionName)
    if (method.isEmpty) Logger.debug(s"Trying to access invalid method $functionName")
    method.get.invoke(this, fieldAccess, level, interpolation, offset).asInstanceOf[IR_Expression]
  }

  def invokeIntegrateResolve(functionName : String, level : Integer, exp : IR_Expression, offset : Option[IR_ConstIndex]) : IR_Expression = {
    val method = this.getClass.getMethods.find(_.getName == functionName)
    if (method.isEmpty) Logger.debug(s"Trying to access invalid method $functionName")
    method.get.invoke(this, exp, level, offset).asInstanceOf[IR_Expression]
  }
}
