package exastencils.core.collectors

import scala.collection.mutable.Stack

import exastencils.base.ir.IR_FunctionLike
import exastencils.datastructures._
import exastencils.logger._

class FctNameCollector extends Collector {
  private val nameStack = new Stack[String]

  override def enter(node : Node) : Unit = {
    node match {
      case fct : IR_FunctionLike => nameStack.push(fct.name)
      case _                     =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case loop : IR_FunctionLike => nameStack.pop
      case _                      =>
    }
  }

  override def reset() : Unit = {
    nameStack.clear
  }

  def inFuction : Boolean = nameStack.nonEmpty

  def getCurrentName : String = {
    if (nameStack.isEmpty) {
      Logger.dbg("Trying to access level outside of a valid scope")
      ""
    } else {
      nameStack.head
    }
  }
}
