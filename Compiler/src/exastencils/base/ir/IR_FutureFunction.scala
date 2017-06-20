package exastencils.base.ir

import exastencils.datastructures.Transformation.Output

/// IR_FutureFunction

trait IR_FutureFunction extends IR_FunctionLike with IR_Expandable {
  override def expand() : Output[IR_Function] = generateFct()
  def generateFct() : IR_Function
}
