package exastencils.base.ir

import exastencils.datastructures.Transformation.Output

/// IR_FutureFunction

trait IR_FutureFunction extends IR_FunctionLike with IR_Expandable {
  override def expand() : Output[IR_Function] = {
    val fct = generateFct()

    // carry over parameters
    fct.isHeaderOnly = isHeaderOnly
    fct.allowInlining = allowInlining
    fct.allowFortranInterface = allowFortranInterface
    fct.functionQualifiers = functionQualifiers

    fct
  }

  def generateFct() : IR_Function
}
