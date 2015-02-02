package exastencils.datastructures

import exastencils.core._
import exastencils.logger._

/**
  * A Strategy encapsulates [[exastencils.datastructures.Transformation]]s to be applied to the program state.
  *
  * @param name name The name of the Strategy. Used for traceability and debugging purposes.
  */
abstract class Strategy(val name : String) {
  protected var token : Option[StateManager.TokenType] = None

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
    *  Executes a given transformation
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
