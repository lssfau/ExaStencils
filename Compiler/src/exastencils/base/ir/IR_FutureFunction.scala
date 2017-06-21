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

/// IR_FuturePlainFunction

trait IR_FuturePlainFunction extends IR_FutureFunction {
  override def generateFct() : IR_PlainFunction
}

/// IR_FutureLeveledFunction

trait IR_FutureLeveledFunction extends IR_FutureFunction {
  def level : Int
  override def generateFct() : IR_LeveledFunction
}
