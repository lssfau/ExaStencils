package exastencils.scheduling

import scala.collection.mutable.ListBuffer

/// SchedulerEntry

case class SchedulerEntry(var name : String, var toSchedule : Schedulable)

/// StrategyScheduler

trait StrategyScheduler {
  def register(strat : Schedulable) : Unit
  def invokeStrategies() : Unit
}

case class Scheduler() extends StrategyScheduler {
  var queue : ListBuffer[SchedulerEntry] = ListBuffer()

  override def register(strat : Schedulable) : Unit = {
    queue += SchedulerEntry(strat.getClass.getSimpleName, strat)
  }

  override def invokeStrategies() : Unit = {
    for (strat <- queue)
      strat.toSchedule.apply()
  }
}