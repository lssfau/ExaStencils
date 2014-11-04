package exastencils.core.collectors

import scala.collection.mutable.ArrayStack

import exastencils.datastructures.Node
import exastencils.datastructures.ir._

abstract class ScopeCollector[T](init : T) extends Collector {

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
      case ConditionStatement(_, trueBody, falseBody) =>
        enterScope()
        if (!trueBody.isEmpty) // HACK: add "switch-info"
          trueBody(0).annotate(START_COND_BLOCK_ANNOT)
        if (!falseBody.isEmpty) // HACK: add "switch-info"
          falseBody(0).annotate(START_COND_BLOCK_ANNOT)

      case _ : Scope
        | _ : ForLoopStatement
        | _ : WhileLoopStatement
        | _ : FunctionStatement
        | _ : SwitchStatement
        | _ : CaseStatement =>
        enterScope()

      case _ =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case _ : ConditionStatement
        | _ : Scope
        | _ : ForLoopStatement
        | _ : WhileLoopStatement
        | _ : FunctionStatement
        | _ : SwitchStatement
        | _ : CaseStatement =>
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
    scopes.push(cloneCurScope)
  }

  protected def leaveScope() : Unit = {
    scopes.pop()
  }
}
