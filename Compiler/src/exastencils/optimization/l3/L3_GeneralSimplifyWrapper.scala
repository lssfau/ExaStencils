package exastencils.optimization.l3

import exastencils.base.l3._
import exastencils.logger.Logger

/// L3_GeneralSimplifyWrapper

object L3_GeneralSimplifyWrapper {
  def process[T <: L3_Node](node : T) : T = {
    val wrapped = L3_Root(node)
    L3_GeneralSimplify.doUntilDoneStandalone(wrapped)
    if (wrapped.nodes.length != 1) Logger.warn(s"L3_GeneralSimplify changed number of nodes on $node")
    wrapped.nodes.head.asInstanceOf[T]
  }
}
