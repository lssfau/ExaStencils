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

  private def matches(toFind : SingleSchedulable)(entry : SingleSchedulable) : Boolean = {
    entry match {
      case c : ConditionedSingleStrategyWrapper if c.strat == toFind => true
      case s : SingleSchedulable if s == toFind                      => true
      case _                                                         => false
    }
  }

  private def indexWhereEntry(toFind : SingleSchedulable, from : Int = 0) : Int = {
    queue.indexWhere(e => matches(toFind)(e.toSchedule), from)
  }

  private def lastIndexWhereEntry(toFind : SingleSchedulable) : Int = {
    queue.lastIndexWhere(e => matches(toFind)(e.toSchedule))
  }

  private def requireIndex(idx : Int, toFind : SingleSchedulable) : Int = {
    if (idx == -1)
      Logger.error("Trying to add to non-existing item in scheduler queue: " + toFind)
    idx
  }

  private def getIndexInQueue(toFind : SingleSchedulable, from : Int) : Int =
    requireIndex(indexWhereEntry(toFind, from), toFind)

  private def getLastIndexInQueue(toFind : SingleSchedulable) : Int =
    requireIndex(lastIndexWhereEntry(toFind), toFind)

  private def entryExistsInQueue(toFind : SingleSchedulable) : Boolean =
    indexWhereEntry(toFind) != -1

  private def getOccurrencesInQueue(toFind : SingleSchedulable) : Int = {
    if (!entryExistsInQueue(toFind))
      Logger.error("Trying to add to non-existing item in scheduler queue: " + toFind)

    queue.map(_.toSchedule).count {
      case c : ConditionedSingleStrategyWrapper if c.strat == toFind => true
      case s : SingleSchedulable if s == toFind                      => true
      case _                                                         => false
    }
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

  def prependToLastFound(toFind : SingleSchedulable, items : Schedulable*): Unit = {
    prependToQueue(getLastIndexInQueue(toFind), convertItemListToSchedulerEntries(items))
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

  def appendToLastFound(toFind : SingleSchedulable, items : Schedulable*): Unit = {
    appendToQueue(getLastIndexInQueue(toFind), convertItemListToSchedulerEntries(items))
  }

  override def register(strat : Schedulable) : Unit = {
    queue ++= convertToSchedulerEntriesWrapper(strat)
  }

  override def invokeStrategies() : Unit = {
    for (strat <- queue)
      strat.toSchedule.apply()
  }
}