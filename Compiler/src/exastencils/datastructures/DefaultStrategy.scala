package exastencils.datastructures

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.util._

class StrategyResult(transformationResults : List[TransformationResult]) {
  def getResults = transformationResults
}

class DefaultStrategy(name : String) extends Strategy(name) {
  protected var transformations_ = new ListBuffer[Transformation]
  protected var results_ = new ListBuffer[(Transformation, TransformationResult)]

  def add(transformation : Transformation) = transformations_ += transformation
  def +=(transformation : Transformation) = add(transformation)
  def ++=(transformations : TraversableOnce[Transformation]) = transformations_.++=(transformations)

  def transformations = { transformations_.toList }
  def results = { results_.toList }

  def apply(node : Option[Node] = None) : Unit = {
    //    var start : Long = 0
    //    if ("Counting " + "Before" != name && "Counting " + "After" != name) {
    //      (new CountingStrategy("Before")).apply()
    //      start = System.nanoTime()
    //    }

    transaction()

    Logger.info(s"""Applying strategy "${name}"""")
    try {
      transformations_.foreach(transformation => {
        executeInternal(transformation, node)
      })
      commit()
    } catch {
      case x : TransformationException => {
        Logger.warn(s"""Strategy "${name}" did not apply successfully""")
        Logger.warn(s"""Error in Transformation ${x.transformation.name}""")
        Logger.warn(s"Message: ${x.msg}")
        Logger.warn(s"Rollback will be performed")
        abort()
      }
    }

    //    if ("Counting " + "Before" != name && "Counting " + "After" != name) {
    //      println("Runtime:\t" + math.round((System.nanoTime() - start) / 1e5) / 10.0 + " ms")
    //      (new CountingStrategy("After")).apply()
    //    }
  }

  protected override def executeInternal(transformation : Transformation, node : Option[Node] = None) : Unit = {
    Logger.info(s"""Applying strategy "${name}::${transformation.name}"""")
    val n = if (transformation.applyAtNode.isDefined) transformation.applyAtNode else node
    val result = StateManager.apply(token.get, transformation, n)
    Logger.debug(s"""Result of strategy "${name}::${transformation.name}": $result""")
    results_ += ((transformation, result))
  }

  def applyStandalone(node : Node) : Unit = {
    Logger.info(s"""Applying strategy "${name}" in standalone mode""")
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

  protected def executeStandaloneInternal(transformation : Transformation, node : Node) : Unit = {
    Logger.info(s"""Applying strategy "${name}::${transformation.name}" in standalone mode""")
    val result = StateManager.applyStandalone(transformation, node)
    Logger.debug(s"""Result of strategy "${name}::${transformation.name}" in standalone mode: $result""")
    results_ += ((transformation, result))
  }
}

object DefaultStrategy {
  def apply(name : String) = new DefaultStrategy(name)
  def apply(name : String, transformations : List[Transformation]) = {
    val s = new DefaultStrategy(name)
    s ++= transformations
    s
  }
}