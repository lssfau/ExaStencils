package exastencils.datastructures

import exastencils.logger._

/**
  * A Strategy that executes its [[exastencils.datastructures.Transformation]]s sequentially.
  * It suppresses all debug output, when called in standalone mode.
  *
  * @param name name The name of the Strategy. Used for traceability and debugging purposes.
  */
class QuietDefaultStrategy(name : String) extends DefaultStrategy(name) {

  override def applyStandalone(node : Node) : Unit = {
    val oldLvl = Logger.getLevel
    Logger.setLevel(Logger.WARNING)
    try {
      super.applyStandalone(node)
    } finally {
      Logger.setLevel(oldLvl)
    }
  }
}

object QuietDefaultStrategy {

  /**
    * A Strategy that executes its [[exastencils.datastructures.Transformation]]s sequentially.
    * It suppresses all debug output, when called in standalone mode.
    *
    * @param name name The name of the Strategy. Used for traceability and debugging purposes.
    */
  def apply(name : String) = new QuietDefaultStrategy(name)

  /**
    * A Strategy that executes its [[exastencils.datastructures.Transformation]]s sequentially.
    * It suppresses all debug output, when called in standalone mode.
    *
    *
    * @param name name The name of the Strategy. Used for traceability and debugging purposes.
    * @param transformations List of transformations for the strategy.
    */
  def apply(name : String, transformations : List[Transformation]) = {
    val s = new QuietDefaultStrategy(name)
    s ++= transformations
    s
  }
}