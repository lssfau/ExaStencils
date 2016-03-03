package exastencils.core.collectors

import scala.collection.mutable.Stack

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.logger._

class FctNameCollector extends Collector {
  private val nameStack = new Stack[String]

  override def enter(node : Node) : Unit = {
    node match {
      case fct : AbstractFunctionStatement => nameStack.push(fct.name)
      case _                               =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case loop : AbstractFunctionStatement => nameStack.pop
      case _                                =>
    }
  }

  override def reset() : Unit = {
    nameStack.clear
  }

  def inFuction : Boolean = !nameStack.isEmpty

  def getCurrentName : String = {
    if (nameStack.isEmpty) {
      Logger.dbg("Trying to access level outside of a valid scope")
      ""
    } else {
      nameStack.head
    }
  }
}
