package exastencils.scheduling

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.Node

/// Schedulable

trait Schedulable {
  def apply(applyAtNode : Option[Node] = None) : Unit
  def reset() : Unit = {}
}

/// NoStrategyWrapper

trait NoStrategyWrapper extends Schedulable {
  def callback : () => Unit

  override def apply(applyAtNode : Option[Node]) : Unit = callback()
}

/// ConditionedStrategyWrapper

object ConditionedStrategyWrapper {
  def apply(condition : Boolean, strats : Schedulable*) = new ConditionedStrategyWrapper(condition, strats.to[ListBuffer])
}

case class ConditionedStrategyWrapper(var condition : Boolean, var strats : ListBuffer[Schedulable]) extends Schedulable {
  override def apply(applyAtNode : Option[Node] = None) : Unit = {
    if (condition)
      strats.foreach(_.apply())
  }

  override def reset() : Unit = {
    for (strat <- strats)
      strat.reset()
  }
}
