package exastencils.datastructures

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Set
import exastencils.core.StateManager
import exastencils.core.Log._
import exastencils.core._

class Strategy(val name : String) {
  protected var transformations_ = new ListBuffer[Transformation]
  protected var results_ = new ListBuffer[(Transformation, TransformationResult)]

  def add(transformation : Transformation) = transformations_ += transformation
  def +=(transformation : Transformation) = add(transformation)

  def transformations = { transformations_.readOnly }
  def results = { results_.readOnly }

  def apply = {
    val token = StateManager.transaction(this)
    DBG(s"""Applying strategy "${name}"""")
    try {
      transformations_.foreach(transformation => {
        INFO(s"""Applying strategy "${name}::${transformation.name}"""")
        val result = StateManager.apply(token, transformation)
        DBG(s"""Result of strategy "${name}::${transformation.name}": $result""")
        results_ += ((transformation, result))
      })
      StateManager.commit(token)
    } catch {
      case x : TransformationException => {
        WARN(s"""Strategy "${name}" did not apply successfully""")
        WARN(s"""Error in Transformation ${x.transformation.name}""")
        WARN(s"Message: ${x.msg}")
        WARN(s"Rollback will be performed")
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
