package exastencils.datastructures

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.util._

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
    * Returns the list of [[exastencils.datastructures.TransformationResult]]s of this Strategy.
    *
    * @return The list of [[exastencils.datastructures.TransformationResult]]s of this Strategy.
    */
  def results = { results_.toList }

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

    transaction()

    Logger.info(s"""Applying strategy "${name}"""")
    try {
      transformations_.foreach(transformation => {
        executeInternal(transformation, applyAtNode)
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