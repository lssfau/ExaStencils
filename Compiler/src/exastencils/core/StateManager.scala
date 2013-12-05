package exastencils.core

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import exastencils.core.collectors._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import scala.util.control.Exception

object StateManager {
  var root_ : Node = null //= ForStatement(VariableDeclarationStatement(Variable("i", IntegerDatatype()), Some(Constant(1))), new Constant(7), new Constant(11), List[Statement]())
  protected var collectors_ = new ListBuffer[Collector]

  protected object History {
    var current_ : Option[ProtocalEntry] = None
    protected var history_ = new Stack[ProtocalEntry]
    protected class ProtocalEntry(val stateBefore : Node, val appliedStrategy : Strategy)

    def transaction(strategy : Strategy) = {
      if (!current_.isEmpty) throw new Exception("Another transaction currently running!")

      current_ = Some(new ProtocalEntry(Duplicate(StateManager.root_), strategy))
    }

    def commit() = {
      if (current_.isEmpty) throw new Exception("No currently running transaction!")
      history_.push(current_.get)
      current_ = None
    }

    def abort() = {
      if (current_.isEmpty) throw new Exception("No currently running transaction!")
      root_ = current_.get.stateBefore
      current_ = None
    }
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

        if (obj.isInstanceOf[List[_]]) {
          var list = obj.asInstanceOf[List[_]]

          list.foreach(f => {
            if (f.isInstanceOf[Node]) ret = apply(f.asInstanceOf[Node], t) // FIXME this is a hack
          })

        } else {

          if (obj.isInstanceOf[Some[_]]) {
            obj = obj.asInstanceOf[Some[Object]].get
          }
          // FIXME better handling if ret == false in traversal => i.e. abort traversal
          if (obj.isInstanceOf[Node]) ret = apply(obj.asInstanceOf[Node], t) //else println("Found something strange: " + obj)
        }
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
    // start transformation transaction
    History.transaction(strategy)

    var ret = false
    strategy.transformations.foreach(t => {
      defStack.reset
      register(defStack)
      apply(root, t) // FIXME better handling if ret == false in traversal => i.e. abort traversal
      unregister(defStack)
    })
    if (ret) {
      History.commit
    } else {
      WARN("Strategy did not apply successfully: " + strategy.name)
      WARN("Rollback will be performed")
      History.abort
    }
    ret
  }

  def apply(strategy : Strategy) : Boolean = {
    // start transformation transaction
    History.transaction(strategy)

    var ret = strategy.apply
    if (ret) {
      History.commit
    } else {
      WARN("Strategy did not apply successfully: " + strategy.name)
      WARN("Rollback will be performed")
      History.abort
    }
    ret
  }

}