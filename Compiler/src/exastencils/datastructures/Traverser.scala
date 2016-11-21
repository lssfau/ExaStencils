package exastencils.datastructures

import exastencils.core.StateManager
import exastencils.datastructures._
import scala.collection.immutable.List
import scala.collection.mutable.Queue
import scala.annotation.Annotation

/**
  * Defines an interface for an entity that governs traversal of the abstract syntax tree.
  *
  */
abstract trait Traverser {
  /**
    * The enter method is called when a new element is visited.
    * @param element The element currently visited.
    * @param elements Its sub-elements.
    */
  def enter(element : Any, elements : List[scala.reflect.runtime.universe.MethodSymbol]) : Unit

  /**
    * The enter method is called when a element is left.
    * @param element The element left.
    */
  def leave(element : Any) : Unit
}

/*
class DepthTraverser extends Traverser {
  def enter(element : Any, elements : List[scala.reflect.runtime.universe.MethodSymbol]) : Unit = {
    exastencils.logger.Logger.debug(s"Entering ${element.getClass()}")

    elements.foreach(m => {
      var obj = StateManager.invoke(element, m)
      obj match {
        case x : Seq[_] => x.foreach(StateManager.traverse(_, this))
        case x : Node   => StateManager.traverse(x, this)
        case _          =>
      }
    })
  }

  def leave(element : Any) : Unit = {
    exastencils.logger.Logger.debug(s"Leaving  ${element.getClass()}")
  }
}

class BreadthTraverser extends Traverser {
  var queue = Queue[Tuple2[Any, scala.reflect.runtime.universe.MethodSymbol]]()

  def enter(element : Any, elements : List[scala.reflect.runtime.universe.MethodSymbol]) : Unit = {
    exastencils.logger.Logger.debug(s"Just visitied ${element.getClass()}")
    elements.foreach(s => queue += ((element, s)))

    if (!queue.isEmpty) {
      var head = queue.dequeue
      var obj = StateManager.invoke(head._1, head._2)
      obj match {
        case x : Seq[_] => x.foreach(StateManager.traverse(_, this))
        case x : Node   => StateManager.traverse(x, this)
        case _          =>
      }
    }
  }

  def leave(element : Any) : Unit = {
    exastencils.logger.Logger.debug(s"Leaving  ${element.getClass()}")
  }
}
*/
