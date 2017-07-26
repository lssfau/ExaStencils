package exastencils.optimization.l4

import exastencils.base.l4._
import exastencils.logger.Logger

/// L4_GeneralSimplifyWrapper

object L4_GeneralSimplifyWrapper {
  def process[T <: L4_Node](node : T) : T = {
    val wrapped = L4_Root(node)
    L4_GeneralSimplify.doUntilDoneStandalone(wrapped)
    if (wrapped.nodes.length != 1) Logger.warn(s"L4_GeneralSimplify changed number of nodes on $node")
    wrapped.nodes.head.asInstanceOf[T]
  }
}
