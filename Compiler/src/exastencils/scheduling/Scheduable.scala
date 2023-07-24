package exastencils.scheduling

import exastencils.datastructures.Node
import exastencils.util.StrategyContainer

/// Schedulable

trait Schedulable {
  def apply(applyAtNode : Option[Node] = None) : Unit
  def reset() : Unit = {}
}

/// StrategyWrapper

case class StrategyWrapper(wrapped : StrategyContainer*) extends Schedulable {
  override def apply(applyAtNode : Option[Node] = None) : Unit = {
    var matches = 0
    do {
      matches = 0
      for (strat <- wrapped) {
        matches += strat.applyAndCountMatches()
      }
    } while (matches > 0)
  }

  override def reset() : Unit = {
    for (strat <- wrapped) {
      strat.reset()
    }
  }
}

/// NoStrategyWrapper

case class NoStrategyWrapper(callback : () => Unit) extends Schedulable {
  override def apply(applyAtNode : Option[Node]) : Unit = callback()
}

/// ConditionedStrategyWrapper

case class ConditionedStrategyWrapper(strat : Schedulable, condition : Boolean) extends Schedulable {
  override def apply(applyAtNode : Option[Node] = None) : Unit = {
    if (condition) {
      strat.apply()
    }
  }

  override def reset() : Unit = {
    strat.reset()
  }
}
