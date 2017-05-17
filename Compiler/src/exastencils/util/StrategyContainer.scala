package exastencils.util

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.DefaultStrategy

/// StrategyContainer

abstract class StrategyContainer {
  var strategies = ListBuffer[DefaultStrategy]()

  def apply() = strategies.foreach(_.apply())

  def applyAndCountMatches() = {
    strategies.map(strategy => {
      strategy.apply()
      strategy.results.last._2.matches
    }).sum
  }
}
