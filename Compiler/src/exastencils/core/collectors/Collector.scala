package exastencils.core.collectors

import exastencils.datastructures._

/**
  * Keeps track of [[exastencils.datastructures.Node]]s that are traversed during the application of [[exastencils.datastructures.Transformation]]s.
  *
  * Collectors follow the Observer-pattern. They are added to the [[exastencils.core.StateManager]], who will notify
  * the observers whenever a [[exastencils.datastructures.Node]] is entered or left during the traversal of the program
  * state. This is a basic trait that must be implemented by concrete implementations.
  */
trait Collector {

  /**
    * Specifies if this collector can be applied concurrently to different functions of the AST. Default is false.
    */
  var isParallel : Boolean = false

  /**
    * Is called when a new [[exastencils.datastructures.Node]] is visited.
    *
    * @param node The [[exastencils.datastructures.Node]] currently visited.
    */
  def enter(node : Node) : Unit

  /**
    * Is called when traversal of a [[exastencils.datastructures.Node]] is done and it is left.
    *
    * @param node The [[exastencils.datastructures.Node]] just left.
    */
  def leave(node : Node) : Unit

  /**
    * Resets the internal state of the Collector.
    *
    * This method resets the internal state of the Collector.
    * [[exastencils.core.StateManager]] calls this method before applying a new [[exastencils.datastructures.Transformation]].
    * This may be used to clear the history of a Collector that keeps a protocol of the visited [[exastencils.datastructures.Node]]s.
    */
  def reset() : Unit
}
