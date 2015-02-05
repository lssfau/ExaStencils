package exastencils.datastructures

import exastencils.core._
import exastencils.logger._
import exastencils.core.collectors.Collector
import scala.collection.mutable.ListBuffer

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
    */
  protected def execute(transformation : Transformation, node : Option[Node] = None) : Unit = {
    Logger.info(s"""Executing nested transformation "${transformation.name}" during strategy "${name}"""")
    executeInternal(transformation, node)
  }

  /**
    *  Executes a given [[exastencils.datastructures.Transformation]].
    *
    *  @param transformation The [[exastencils.datastructures.Transformation]] to be executed.
    *  @param node Specifies the source node where the [[exastencils.datastructures.Transformation]] starts to traverse the program state.
    */
  protected def executeInternal(transformation : Transformation, node : Option[Node] = None) : Unit = {
    Logger.info(s"""Applying strategy "${name}::${transformation.name}"""")
    val n = if (transformation.applyAtNode.isDefined) transformation.applyAtNode else node
    val result = StateManager.apply(token.get, transformation, n)
    Logger.debug(s"""Result of strategy "${name}::${transformation.name}": $result""")
  }
}
