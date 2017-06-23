package exastencils.optimization.ir

import exastencils.base.ir.IR_Root
import exastencils.datastructures.Node
import exastencils.logger.Logger

/// IR_GeneralSimplifyWrapper

object IR_GeneralSimplifyWrapper {
  def process[T <: Node](node : T) : T = {
    val wrapped = IR_Root(node)
    IR_GeneralSimplify.doUntilDoneStandalone(wrapped)
    if (wrapped.nodes.length != 1) Logger.warn(s"IR_GeneralSimplify changed number of nodes on $node")
    wrapped.nodes.head.asInstanceOf[T]
  }
}
