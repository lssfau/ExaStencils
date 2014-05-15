package exastencils.datastructures

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Set
import exastencils.core.StateManager
import exastencils.core.Logger._
import exastencils.core._

class Strategy(val name : String) {
  protected var transformations_ = new ListBuffer[Transformation]
  protected var results_ = new ListBuffer[(Transformation, TransformationResult)]

  def add(transformation : Transformation) = transformations_ += transformation
  def +=(transformation : Transformation) = add(transformation)

  def transformations = { transformations_.toList }
  def results = { results_.toList }

  // FIXME: quick HACK to realize trafo in trafo functionality
  def apply(hackedApplyAt : Option[Node] = None, hackedToken : Option[StateManager.History.TransactionToken] = None) = {
    val token : StateManager.History.TransactionToken =
      (if (hackedToken.isEmpty)
        StateManager.transaction(this)
      else
        hackedToken.get)
    Logger.debug(s"""Applying strategy "${name}"""")
    try {
      transformations_.foreach(transformation => {
        Logger.info(s"""Applying strategy "${name}::${transformation.name}"""")
        val result = StateManager.apply(token, transformation, hackedApplyAt)
        Logger.debug(s"""Result of strategy "${name}::${transformation.name}": $result""")
        results_ += ((transformation, result))
      })
      if (hackedToken.isEmpty)
        StateManager.commit(token)
    } catch {
      case x : TransformationException => {
        Logger.warn(s"""Strategy "${name}" did not apply successfully""")
        Logger.warn(s"""Error in Transformation ${x.transformation.name}""")
        Logger.warn(s"Message: ${x.msg}")
        Logger.warn(s"Rollback will be performed")
        StateManager.abort(token)
      }

    }
  }
}

object Strategy {
  def apply(name : String) = new Strategy(name)
  def apply(name : String, transformations : List[Transformation]) = {
    val s = new Strategy(name)
    s.transformations_ ++= transformations
    s
  }
}

class StrategyResult(transformationResults : List[TransformationResult]) {
  def getResults = transformationResults
}
