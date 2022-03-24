package exastencils.waLBerla.ir

import exastencils.base.ir._

object IR_WaLBerlaUtil {

  def make_shared(templateDt : String, arg : IR_Expression) =
    IR_FunctionCall(IR_ExternalFunctionReference(s"std::make_shared< $templateDt >"), arg)
  def make_unique(templateDt : String, arg : IR_Expression) =
    IR_FunctionCall(IR_ExternalFunctionReference(s"std::make_unique< $templateDt >"), arg)

  def initCommSchemes = true // TODO: adapt condition

  def memberSuffix = "_gen"
  def getGeneratedName(s : String) : String = s + memberSuffix
}
