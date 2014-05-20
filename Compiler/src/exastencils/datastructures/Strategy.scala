package exastencils.datastructures

import exastencils.core.StateManager
import exastencils.core.Logger._
import exastencils.core._

class Strategy(val name : String) {
  protected var token : Option[StateManager.TokenType] = None

  protected def transaction() : Unit = {
    token = Some(StateManager.transaction(this))
  }

  protected def execute(transformation : Transformation, node : Option[Node] = None) : Unit = {
    Logger.info(s"""Executing nested transformation "${transformation.name}" during strategy "${name}"""")
    executeInternal(transformation, node)
  }

  protected def executeInternal(transformation : Transformation, node : Option[Node] = None) : Unit = {
    Logger.info(s"""Applying strategy "${name}::${transformation.name}"""")
    val n = if (transformation.applyAtNode.isDefined) transformation.applyAtNode else node
    val result = StateManager.apply(token.get, transformation, n)
    Logger.debug(s"""Result of strategy "${name}::${transformation.name}": $result""")
  }

  def commit() = {
    Logger.info(s"""Committing Strategy "$name"""")
    StateManager.commit(token.get)
  }

  def abort() = {
    Logger.info(s"""Aborting Strategy "$name"""")
    StateManager.abort(token.get)
  }
}
