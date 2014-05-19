package exastencils.datastructures

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Set
import exastencils.core.StateManager
import exastencils.core.Logger._
import exastencils.core._

class Strategy(val name: String) {
  protected var transformations_ = new ListBuffer[Transformation]
  protected var results_ = new ListBuffer[(Transformation, TransformationResult)]

  def add(transformation: Transformation) = transformations_ += transformation
  def +=(transformation: Transformation) = add(transformation)

  def transformations = { transformations_.toList }
  def results = { results_.toList }

  protected var token: Option[StateManager.TokenType] = None

  def apply(node: Option[Node] = None): Unit = {
    token = Some(StateManager.transaction(this))

    Logger.info(s"""Applying strategy "${name}"""")
    try {
      transformations_.foreach(transformation => {
        executeInternal(transformation, node)
      })
      StateManager.commit(token.get)
    } catch {
      case x: TransformationException => {
        Logger.warn(s"""Strategy "${name}" did not apply successfully""")
        Logger.warn(s"""Error in Transformation ${x.transformation.name}""")
        Logger.warn(s"Message: ${x.msg}")
        Logger.warn(s"Rollback will be performed")
        StateManager.abort(token.get)
      }
    }
  }

  def execute(transformation: Transformation, node: Option[Node] = None): Unit = {
    Logger.info(s"""Executing nested transformation "${transformation.name}" during strategy "${name}"""")
    executeInternal(transformation, node)
  }

  protected def executeInternal(transformation: Transformation, node: Option[Node] = None): Unit = {
    Logger.info(s"""Applying strategy "${name}::${transformation.name}"""")
    val n = if (transformation.applyAtNode.isDefined) transformation.applyAtNode else node
    val result = StateManager.apply(token.get, transformation, n)
    Logger.debug(s"""Result of strategy "${name}::${transformation.name}": $result""")
    results_ += ((transformation, result))
  }

  def applyStandalone(node: Node): Unit = {
    Logger.info(s"""Applying strategy "${name}" in standalone mode""")
    try {
      transformations_.foreach(transformation => {
        executeStandaloneInternal(transformation, node)
      })
    } catch {
      case x: TransformationException => {
        Logger.warn(s"""Strategy "${name}" as standalone did not apply successfully""")
        Logger.warn(s"""Error in Transformation ${x.transformation.name}""")
        Logger.warn(s"Message: ${x.msg}")
      }
    }
  }

  protected def executeStandaloneInternal(transformation: Transformation, node: Node): Unit = {
    Logger.info(s"""Applying strategy "${name}::${transformation.name}" in standalone mode""")
    val result = StateManager.applyStandalone(transformation, node)
    Logger.debug(s"""Result of strategy "${name}::${transformation.name}" in standalone mode: $result""")
    results_ += ((transformation, result))
  }
}

object Strategy {
  def apply(name: String) = new Strategy(name)
  def apply(name: String, transformations: List[Transformation]) = {
    val s = new Strategy(name)
    s.transformations_ ++= transformations
    s
  }
}

class StrategyResult(transformationResults: List[TransformationResult]) {
  def getResults = transformationResults
}
