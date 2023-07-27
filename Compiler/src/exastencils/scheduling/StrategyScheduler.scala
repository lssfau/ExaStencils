package exastencils.scheduling

import scala.collection.mutable.ListBuffer

import exastencils.logger.Logger

/// SchedulerEntry

case class SchedulerEntry(var name : String, var toSchedule : SingleSchedulable)

/// StrategyScheduler

trait StrategyScheduler {
  def register(strat : Schedulable) : Unit
  def invokeStrategies() : Unit
}

case class Scheduler() extends StrategyScheduler {
  var queue : ListBuffer[SchedulerEntry] = ListBuffer()

  private def convertToSchedulerEntry(strat : SingleSchedulable) : SchedulerEntry = {
    SchedulerEntry(strat.getClass.getSimpleName, strat)
  }

  private def convertToSchedulerEntries(stratContainer : SchedulableContainer) : ListBuffer[SchedulerEntry] = {
    stratContainer.strats.map(s => SchedulerEntry(s.getClass.getSimpleName, s))
  }

  private def convertToSchedulerEntriesWithCond(condStratContainer : ConditionedStrategyContainerWrapper) : ListBuffer[SchedulerEntry] = {
    condStratContainer.strats.map(s => SchedulerEntry(s.getClass.getSimpleName, ConditionedSingleStrategyWrapper(condStratContainer.callbackCondition, s)))
  }

  private def convertToSchedulerEntriesWrapper(strat : Schedulable) : ListBuffer[SchedulerEntry] = {
    strat match {
      case s : SingleSchedulable                     =>
        ListBuffer(convertToSchedulerEntry(s))
      case css : ConditionedStrategyContainerWrapper =>
        convertToSchedulerEntriesWithCond(css)
      case c : SchedulableContainer                  =>
        convertToSchedulerEntries(c)
    }
  }

  private def convertItemListToSchedulerEntries(items : Seq[Schedulable]) : ListBuffer[SchedulerEntry] = {
    items.flatMap(convertToSchedulerEntriesWrapper).to[ListBuffer]
  }

  private def entryExistsInQueue(toFind : SchedulerEntry) : Boolean = {
    queue contains toFind
  }

  private def getIndexInQueue(toFind : SingleSchedulable, from : Int) : Int = {
    val entry = convertToSchedulerEntry(toFind)
    if (entryExistsInQueue(entry))
      Logger.error("Trying to prepend to non-existing item in scheduler queue")

    queue.indexOf(entry, from)
  }

  private def getOccurrencesInQueue(toFind : SingleSchedulable) : Int = {
    val entry = convertToSchedulerEntry(toFind)
    if (entryExistsInQueue(entry))
      Logger.error("Trying to prepend to non-existing item in scheduler queue")

    queue.count(_ == entry)
  }

  private def prependToQueue(findIdx : Int, items : ListBuffer[SchedulerEntry]) : Unit = {
    val (front, back) = queue.splitAt(findIdx)
    queue = front ++ items ++ back
  }

  private def appendToQueue(findIdx : Int, items : ListBuffer[SchedulerEntry]) : Unit = {
    if (findIdx < queue.size - 1) {
      prependToQueue(findIdx + 1, items)
    } else {
      queue.append(items : _*)
    }
  }

  def prependToAllFound(toFind : SingleSchedulable, items : Schedulable*) : Unit = {
    val itemList = convertItemListToSchedulerEntries(items)

    var lastFoundIdx = 0
    for (_ <- 0 until getOccurrencesInQueue(toFind)) {
      // find new index
      lastFoundIdx = getIndexInQueue(toFind, from = lastFoundIdx)

      // append
      prependToQueue(lastFoundIdx, itemList)

      // start looking after index of appended list
      lastFoundIdx += itemList.size + 1
    }
  }

  def prependToFirstFound(toFind : SingleSchedulable, items : Schedulable*) : Unit = {
    prependToQueue(getIndexInQueue(toFind, from = 0), convertItemListToSchedulerEntries(items))
  }

  def appendToAllFound(toFind : SingleSchedulable, items : Schedulable*) : Unit = {
    val itemList = convertItemListToSchedulerEntries(items)

    var lastFoundIdx = 0
    for (_ <- 0 until getOccurrencesInQueue(toFind)) {
      // find new index
      lastFoundIdx = getIndexInQueue(toFind, from = lastFoundIdx)

      // append
      appendToQueue(lastFoundIdx, itemList)

      // start looking after index of appended list
      lastFoundIdx += itemList.size + 1
    }
  }

  def appendToFirstFound(toFind : SingleSchedulable, items : Schedulable*) : Unit = {
    appendToQueue(getIndexInQueue(toFind, from = 0), convertItemListToSchedulerEntries(items))
  }

  override def register(strat : Schedulable) : Unit = {
    queue ++= convertToSchedulerEntriesWrapper(strat)
  }

  override def invokeStrategies() : Unit = {
    for (strat <- queue) {
      println("Invoking: " + strat.name)
      strat.toSchedule.apply()
    }
  }
}