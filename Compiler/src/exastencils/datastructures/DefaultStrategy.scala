package exastencils.datastructures

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.logger._

/**
  * A Strategy that executes its [[exastencils.datastructures.Transformation]]s sequentially.
  *
  * @param name name The name of the Strategy. Used for traceability and debugging purposes.
  */
class DefaultStrategy(name : String) extends Strategy(name) {
  protected var transformations_ = new ListBuffer[Transformation]
  protected var results_ = new ListBuffer[(Transformation, TransformationResult)]

  /**
    * Add a [[exastencils.datastructures.Transformation]] to this Strategy.
    *
    * @param transformation The [[exastencils.datastructures.Transformation]] to be added.
    */
  def add(transformation : Transformation) = transformations_ += transformation

  /**
    * Add a [[exastencils.datastructures.Transformation]] to this Strategy.
    *
    * @param transformation The [[exastencils.datastructures.Transformation]] to be added.
    */
  def +=(transformation : Transformation) = add(transformation)

  /**
    * Add a list of [[exastencils.datastructures.Transformation]]s to this Strategy.
    *
    * @param transformations The [[exastencils.datastructures.Transformation]]s to be added.
    */
  def ++=(transformations : TraversableOnce[Transformation]) = transformations_.++=(transformations)

  /**
    * Returns the list of [[exastencils.datastructures.Transformation]]s of this Strategy.
    *
    * @return The list of [[exastencils.datastructures.Transformation]]s of this Strategy.
    */
  def transformations = { transformations_.toList }

  /**
    * Returns the list of ([[exastencils.datastructures.Transformation]], [[exastencils.datastructures.TransformationResult]]) of this Strategy.
    *
    * @return The list of ([[exastencils.datastructures.Transformation]], [[exastencils.datastructures.TransformationResult]]) of this Strategy.
    */
  def results = { results_.toList }

  /**
    * Returns the [[exastencils.datastructures.TransformationResult]] of the last [[exastencils.datastructures.Transformation]] that has been executed.
    *
    * @return The list of [[exastencils.datastructures.TransformationResult]]s of the last [[exastencils.datastructures.Transformation]] that has been executed
    */
  def lastResult = { if (!results.isEmpty) Logger.error("No transformation has been executed!"); results_.last._2 }

  /**
    * Returns the [[exastencils.datastructures.TransformationResult]] of the given [[exastencils.datastructures.Transformation]].
    *
    * @param transformation The [[exastencils.datastructures.Transformation]] to look up results for.
    * @return The list of [[exastencils.datastructures.TransformationResult]]s of the given [[exastencils.datastructures.Transformation]].
    */
  def findResults(transformation : Transformation) = {
    results_.filter(p => p._1 == transformation)
  }

  /**
    * Returns the [[exastencils.datastructures.TransformationResult]] of the given [[exastencils.datastructures.Transformation]].
    *
    * @param transformation The [[exastencils.datastructures.Transformation]] to look up results for.
    * @return The list of [[exastencils.datastructures.TransformationResult]]s of the given [[exastencils.datastructures.Transformation]].
    */
  def findResults(transformation : String) = {
    results_.filter(p => p._1.name == transformation)
  }

  /**
    * Resets this Strategy by, e.g., clearing all [[exastencils.datastructures.TransformationResult]]s.
    */
  def reset() = {
    results_.clear()
  }

  /**
    * Executes this Strategy by applying all [[exastencils.datastructures.Transformation]]s sequentially.
    *
    * @param applyAtNode Optional; specifies a source node where the [[exastencils.datastructures.Transformation]] starts to traverse the program state.
    */
  def apply(applyAtNode : Option[Node] = None) : Unit = {
    //    var start : Long = 0
    //    if ("Counting " + "Before" != name && "Counting " + "After" != name) {
    //      (new CountingStrategy("Before")).apply()
    //      start = System.nanoTime()
    //    }

    this.transaction()
    this.resetCollectors()
    Logger.info(s"""Applying strategy "${name}"""")
    try {
      transformations_.foreach(transformation => {
        executeInternal(transformation, applyAtNode)
      })
      this.commit()
    } catch {
      case x : TransformationException => {
        Logger.warn(s"""Strategy "${name}" did not apply successfully""")
        Logger.warn(s"""Error in Transformation ${x.transformation.name}""")
        Logger.warn(s"Message: ${x.msg}")
        Logger.warn(s"Rollback will be performed")
        this.abort()
      }
    }

    //    if ("Counting " + "Before" != name && "Counting " + "After" != name) {
    //      Logger.dbg("Runtime:\t" + math.round((System.nanoTime() - start) / 1e5) / 10.0 + " ms")
    //      (new CountingStrategy("After")).apply()
    //    }
  }

  protected override def executeInternal(transformation : Transformation, node : Option[Node] = None) : TransformationResult = {
    Logger.info(s"""Applying strategy "${name}::${transformation.name}"""")
    if (Settings.timeStrategies)
      StrategyTimer.startTiming(name)

    val n = if (transformation.applyAtNode.isDefined) transformation.applyAtNode else node
    val result = StateManager.apply(token.get, transformation, n)

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming(name)
    Logger.debug(s"""Result of strategy "${name}::${transformation.name}": $result""")
    results_ += ((transformation, result))
    result
  }

  def applyStandalone(node : Node) : Unit = {
    Logger.info(s"""Applying strategy "${name}" in standalone mode""")

    this.resetCollectors()
    try {
      transformations_.foreach(transformation => {
        executeStandaloneInternal(transformation, node)
      })
    } catch {
      case x : TransformationException => {
        Logger.warn(s"""Strategy "${name}" as standalone did not apply successfully""")
        Logger.warn(s"""Error in Transformation ${x.transformation.name}""")
        Logger.warn(s"Message: ${x.msg}")
      }
    }
  }

  def applyStandalone(nodes : Seq[Node]) : Unit = {
    // for (node <- nodes) applyStandalone(node)
    final case class NodeSeqWrapper(var nodes : Seq[Node]) extends Node {}
    applyStandalone(NodeSeqWrapper(nodes)) // FIXME: BUG: modifications of `nodes` directly (e.g. replacing one element of `nodes`) are NOT visible to the caller!
  }

  protected def executeStandaloneInternal(transformation : Transformation, node : Node) : TransformationResult = {
    Logger.info(s"""Applying strategy "${name}::${transformation.name}" in standalone mode""")
    if (Settings.timeStrategies)
      StrategyTimer.startTiming(name)

    val result = StateManager.applyStandalone(this, transformation, node)

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming(name)
    Logger.debug(s"""Result of strategy "${name}::${transformation.name}" in standalone mode: $result""")
    results_ += ((transformation, result))
    result
  }
}

object DefaultStrategy {

  /**
    * A Strategy that executes its [[exastencils.datastructures.Transformation]]s sequentially.
    *
    * @param name name The name of the Strategy. Used for traceability and debugging purposes.
    */
  def apply(name : String) = new DefaultStrategy(name)

  /**
    * A Strategy that executes its [[exastencils.datastructures.Transformation]]s sequentially.
    *
    * @param name name The name of the Strategy. Used for traceability and debugging purposes.
    * @param transformations List of transformations for the strategy.
    */
  def apply(name : String, transformations : List[Transformation]) = {
    val s = new DefaultStrategy(name)
    s ++= transformations
    s
  }
}
