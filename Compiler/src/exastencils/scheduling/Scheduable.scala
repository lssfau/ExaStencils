package exastencils.scheduling

import exastencils.datastructures.Node

/// Schedulable

trait Schedulable {
  def apply(applyAtNode : Option[Node] = None) : Unit
  def reset() : Unit = {}
}

/// ApplyAtNodeStrategyWrapper

case class ApplyAtNodeStrategyWrapper(var strat : Schedulable, var node : Option[Node]) extends Schedulable {
  override def apply(applyAtNode : Option[Node] = None) : Unit = strat.apply(node)
}

/// NoStrategyWrapper

trait NoStrategyWrapper extends Schedulable {
  def callback : () => Unit

  override def apply(applyAtNode : Option[Node]) : Unit = callback()
}

/// ConditionedStrategyWrapper

case class ConditionedStrategyWrapper(var condition : Boolean, var strats : Schedulable*) extends Schedulable {
  override def apply(applyAtNode : Option[Node] = None) : Unit = {
    if (condition)
      strats.foreach(_.apply())
  }

  override def reset() : Unit = {
    for (strat <- strats)
      strat.reset()
  }
}
