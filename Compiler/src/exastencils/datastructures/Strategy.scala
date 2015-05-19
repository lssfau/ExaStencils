package exastencils.datastructures

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.core.collectors.Collector
import exastencils.logger._

object StrategyTimer {
  class Data {
    var start : Long = 0
    var entries : Long = 0
    var count : Long = 0
    var totalDuration : Long = 0
  }

  var data : HashMap[String, Data] = HashMap()

  def startTiming(name : String) = {
    if (!data.contains(name)) data.put(name, new Data)
    val thisData = data.get(name).get

    thisData.entries += 1

    if (1 == thisData.entries) // handle recursive strategy correctly
      thisData.start = System.nanoTime()
  }

  def stopTiming(name : String) = {
    if (!data.contains(name)) data.put(name, new Data)
    val thisData = data.get(name).get

    thisData.entries -= 1

    if (thisData.entries < 0) {
      Logger.warn(s"Trying to stop timing for strategy $name which has not yet been started")
    } else if (0 == thisData.entries) {
      thisData.totalDuration += System.nanoTime() - thisData.start
      thisData.count += 1
    } // otherwise nothing to do due to inclusive regions    
  }

  def print = {
    val totalSum = data.map(_._2.totalDuration).sum
    for (d <- data.toSeq.sortBy(_._2.totalDuration)) {
      val runtime : Double = math.round(d._2.totalDuration / 1e6)
      val share : Double = math.round((d._2.totalDuration * 1000.0) / totalSum) / 10.0

      if (share >= Settings.timeStratPercentThreshold)
        Logger.debug(s"$runtime ms ($share %) were consumed through '${d._1}' (${d._2.count} top level transformation calls)")
    }
  }
}

/**
  * A Strategy encapsulates [[exastencils.datastructures.Transformation]]s to be applied to the program state.
  *
  * @param name name The name of the Strategy. Used for traceability and debugging purposes.
  */
abstract class Strategy(val name : String) {
  protected var token : Option[StateManager.TokenType] = None
  private var collectors = ListBuffer[Collector]()

  /** Opens a new Transaction with StateManager. */
  protected def transaction() : Unit = {
    token = Some(StateManager.transaction(this))
  }

  /** Commits, i.e., ends successfully, the currently running [[exastencils.datastructures.Transformation]] with StateManager. */
  protected def commit() = {
    Logger.info(s"""Committing Strategy "$name"""")
    StateManager.commit(token.get)
  }

  /** Aborts, i.e., ends not successfully, the currently running [[exastencils.datastructures.Transformation]] with StateManager. */
  protected def abort() = {
    Logger.info(s"""Aborting Strategy "$name"""")
    StateManager.abort(token.get)
  }

  /**
    * Registers a [[exastencils.core.collectors.Collector]] with this Strategy.
    *
    * @param c The [[exastencils.core.collectors.Collector]] to be added.
    */
  def register(c : Collector) = { collectors += c }

  /**
    * Notifies the [[exastencils.core.collectors.Collector]]s that a [[exastencils.datastructures.Node]] has been entered.
    *
    * @param node The [[exastencils.datastructures.Node]] that has been entered.
    */
  def notifyEnter(node : Node) = { collectors.foreach(c => c.enter(node)) }

  /**
    * Notifies the [[exastencils.core.collectors.Collector]]s that a [[exastencils.datastructures.Node]] has been left.
    *
    * @param node The [[exastencils.datastructures.Node]] that has been left.
    */
  def notifyLeave(node : Node) = { collectors.foreach(c => c.leave(node)) }

  /**
    * Resets all [[exastencils.core.collectors.Collector]]s of this strategy.
    */
  def resetCollectors() = { collectors.foreach(c => c.reset()) }

  /**
    * Unregister a [[exastencils.core.collectors.Collector]] from this Strategy.
    *
    * @param c The [[exastencils.core.collectors.Collector]] to be removed.
    */
  def unregister(c : Collector) = { collectors -= c }

  /** Unregister all currently registered [[exastencils.core.collectors.Collector]]s from this Strategy. */
  def unregisterAll() = { collectors.clear }

  /**
    *  Executes a given [[exastencils.datastructures.Transformation]].
    *
    *  @param transformation The [[exastencils.datastructures.Transformation]] to be executed.
    *  @param node Specifies the source node where the [[exastencils.datastructures.Transformation]] starts to traverse the program state.
    *  @return Result statistics about the transformation.
    */
  protected def execute(transformation : Transformation, node : Option[Node] = None) : TransformationResult = {
    Logger.info(s"""Executing nested transformation "${transformation.name}" during strategy "${name}"""")
    executeInternal(transformation, node)
  }

  /**
    *  Executes a given [[exastencils.datastructures.Transformation]].
    *
    *  @param transformation The [[exastencils.datastructures.Transformation]] to be executed.
    *  @param node Specifies the source node where the [[exastencils.datastructures.Transformation]] starts to traverse the program state.
    *  @return Result statistics about the transformation.
    */
  protected def executeInternal(transformation : Transformation, node : Option[Node] = None) : TransformationResult = {
    Logger.info(s"""Applying strategy "${name}::${transformation.name}"""")
    if (Settings.timeStrategies)
      StrategyTimer.startTiming(name)

    val n = if (transformation.applyAtNode.isDefined) transformation.applyAtNode else node
    val result = StateManager.apply(token.get, transformation, n)

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming(name)
    Logger.debug(s"""Result of strategy "${name}::${transformation.name}": $result""")
    return result
  }
}
