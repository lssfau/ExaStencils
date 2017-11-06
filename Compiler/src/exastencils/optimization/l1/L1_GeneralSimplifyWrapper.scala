package exastencils.optimization.l1

import exastencils.base.l1._
import exastencils.logger.Logger

/// L1_GeneralSimplifyWrapper

object L1_GeneralSimplifyWrapper {
  def process[T <: L1_Node](node : T) : T = {
    val wrapped = L1_Root(node)
    L1_GeneralSimplify.doUntilDoneStandalone(wrapped)
    if (wrapped.nodes.length != 1) Logger.warn(s"L1_GeneralSimplify changed number of nodes on $node")
    wrapped.nodes.head.asInstanceOf[T]
  }
}
