package exastencils.util.ir

import scala.collection.mutable.ArrayStack

import exastencils.base.ir._
import exastencils.core.collectors.Collector
import exastencils.datastructures.Node

abstract class IR_ScopeCollector[T](init : T) extends Collector {

  private final val START_COND_BLOCK_ANNOT = "FirstElse"

  private final val scopes = new ArrayStack[T]()
  protected final def curScope : T = scopes.top
  this.reset() // initialize

  protected def cloneCurScope() : T

  override def enter(node : Node) : Unit = {
    if (node.removeAnnotation(START_COND_BLOCK_ANNOT).isDefined) { // HACK: check for "switch-info"
      leaveScope()
      enterScope()
    }

    node match {
      case IR_IfCondition(_, trueBody, falseBody) =>
        enterScope()
        if (trueBody.nonEmpty) // HACK: add "switch-info"
          trueBody(0).annotate(START_COND_BLOCK_ANNOT)
        if (falseBody.nonEmpty) // HACK: add "switch-info"
          falseBody(0).annotate(START_COND_BLOCK_ANNOT)

      case _ : IR_Scope
           | _ : IR_ForLoop
           | _ : IR_WhileLoop
           | _ : IR_Function
           | _ : IR_Switch
           | _ : IR_Case =>
        enterScope()

      case _ =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case _ : IR_IfCondition
           | _ : IR_Scope
           | _ : IR_ForLoop
           | _ : IR_WhileLoop
           | _ : IR_Function
           | _ : IR_Switch
           | _ : IR_Case =>
        leaveScope()

      case _ =>
    }
  }

  override def reset() : Unit = {
    scopes.clear()
    scopes.push(init)
    enterScope() // global one
  }

  protected def enterScope() : Unit = {
    scopes.push(cloneCurScope())
  }

  protected def leaveScope() : Unit = {
    scopes.pop()
  }
}
