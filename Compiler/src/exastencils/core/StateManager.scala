package exastencils.core

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import exastencils.core.collectors._
import exastencils.datastructures._
import exastencils.datastructures.l4._

object StateManager {
  protected var root_ : Node = ForStatement(VariableDeclarationStatement(Variable("i", IntegerDatatype()), Some(Constant(1))),
    new Constant(7), new Constant(11), List[Statement]())
  protected var collectors_ = new ListBuffer[Collector]

  protected class LogEntry() // tuple-lile

  protected class ProtocalEntry(treeBefore : Node, appliedStrategy : Strategy)
  protected var history_ = new Stack[ProtocalEntry]
  protected def pushToHistory(treeBefore : Node, appliedStrategy : Strategy) = {
    history_.push(new ProtocalEntry(treeBefore, appliedStrategy))
  }

  def root = root_

  protected def enterNodeNotifyCollectors(node : Node) = { collectors_.foreach(c => c.enter(node)) }
  protected def leaveNodeNotifyCollectors(node : Node) = { collectors_.foreach(c => c.leave(node)) }

  def register(c : Collector) = { collectors_ += c }
  def unregister(c : Collector) = { collectors_ -= c }
  def unregisterAll() = { collectors_.clear }

  val defStack = new StackCollector

  protected def replaceSubnode(node : Node, oldSub : Node, newSub : Node) : Boolean = { // protected: only to be used internally!
    var ret = false
    node.getClass.getDeclaredFields.foreach(f => {
      val a = f.isAccessible()
      f.setAccessible(true)
      if (f.get(node) == oldSub) {
        f.set(node, newSub)
        ret = true
      } else if (f.get(node).isInstanceOf[Some[_]] && f.get(node).asInstanceOf[Some[Object]].get == oldSub) {
        f.set(node, Some(newSub))
        ret = true
      }
      f.setAccessible(a)
    })
    return ret
  }

  protected def apply(node : Node, t : Transformation) : Boolean = { // protected: only to be used internally!
    var ret = true

    if (t.function.isDefinedAt(node)) {
      val fret = t.function(node)
      if ((fret != None) && (fret.get ne node)) {
        ret = replaceSubnode(defStack.head, node, fret.get)
      } // else WARN("did not replace with " + fret)
    }

    if (!t.recursive) return ret

    enterNodeNotifyCollectors(node)
    node.getClass.getDeclaredFields.foreach(n => {
      val method = node.getClass.getDeclaredMethods.find(m => m.getName == n.getName)
      if (method.nonEmpty) {
        var obj : Object = null
        obj = method.get.invoke(node).asInstanceOf[Object]
        if (obj.isInstanceOf[Some[_]]) {
          obj = obj.asInstanceOf[Some[Object]].get
        }
        // FIXME better handling if ret == false in traversal => i.e. abort traversal
        if (obj.isInstanceOf[Node]) ret = apply(obj.asInstanceOf[Node], t) //else println("Found something strange: " + obj)
      }
    })
    leaveNodeNotifyCollectors(node)
    return ret
  }

  def apply(transformation : Transformation) : Unit = { // protected: only to be used internally!
    defStack.reset
    register(defStack)
    apply(root, transformation)
    unregister(defStack)
  }

  def defaultApply(strategy : Strategy) : Boolean = {
    // clone previous state
    val previous = Duplicate(StateManager.root)

    var ret = false
    strategy.transformations.foreach(t => {
      defStack.reset
      register(defStack)
      ret = apply(root, t) // FIXME better handling if ret == false in traversal => i.e. abort traversal
      unregister(defStack)
    })
    if (ret) {
      pushToHistory(previous, strategy)
    } else {
      WARN("Strategy did not apply successfully: " + strategy)
    }
    ret
  }

  def apply(strategy : Strategy) : Boolean = {
    // clone previous state
    val previous = Duplicate(StateManager.root)

    var ret = strategy.apply
    if (ret) {
      pushToHistory(previous, strategy)
    } else {
      WARN("Strategy did not apply successfully: " + strategy)
    }
    ret
  }

}