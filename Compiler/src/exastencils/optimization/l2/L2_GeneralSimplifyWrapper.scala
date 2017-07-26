package exastencils.optimization.l2

import exastencils.base.l2._
import exastencils.logger.Logger

/// L2_GeneralSimplifyWrapper

object L2_GeneralSimplifyWrapper {
  def process[T <: L2_Node](node : T) : T = {
    val wrapped = L2_Root(node)
    L2_GeneralSimplify.doUntilDoneStandalone(wrapped)
    if (wrapped.nodes.length != 1) Logger.warn(s"L2_GeneralSimplify changed number of nodes on $node")
    wrapped.nodes.head.asInstanceOf[T]
  }
}
