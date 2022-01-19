package exastencils.waLBerla.ir

import exastencils.base.ir._
import exastencils.logger.Logger

object IR_WaLBerlaUtil {

  def make_shared(templateDt : String, arg : IR_Expression) =
    IR_FunctionCall(IR_ExternalFunctionReference(s"std::make_shared< $templateDt >"), arg)
  def make_unique(templateDt : String, arg : IR_Expression) =
    IR_FunctionCall(IR_ExternalFunctionReference(s"std::make_unique< $templateDt >"), arg)

  // TODO: this is only a temporary implementation (Q potentially too large, i.e. too many directions)
  def stencilTemplate(numDims : Int) = numDims match {
    case 2 => "stencil::D2Q9"
    case 3 => "stencil::D3Q27"
    case _ => Logger.error("No waLBerla stencil class available for dimension: " + numDims)
  }

  def initCommSchemes = true // TODO: adapt condition

  def memberSuffix = "_gen"
  def getGeneratedName(s : String) : String = s + memberSuffix
}
