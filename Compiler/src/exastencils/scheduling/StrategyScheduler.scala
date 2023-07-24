package exastencils.scheduling

import scala.collection.mutable.ListBuffer

/// SchedulerEntry

case class SchedulerEntry(var name : String, toSchedule : Schedulable)

/// StrategyScheduler

trait StrategyScheduler {
  def register(entry : SchedulerEntry) : Unit
  def invokeStrategies() : Unit
}

case class Scheduler() extends StrategyScheduler {
  var queue : ListBuffer[SchedulerEntry] = ListBuffer()

  override def register(entry : SchedulerEntry) : Unit = {
    queue += entry
  }

  override def invokeStrategies() : Unit = {
    for (strat <- queue)
      strat.toSchedule.apply()
  }
}