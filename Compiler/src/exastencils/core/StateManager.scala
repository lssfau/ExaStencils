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

  protected def applyAtNode(node : Node, t : Transformation) : Option[Node] = {
    if (t.function.isDefinedAt(node)) t.function(node) else Some(node)
  }

  protected def doReplace[T](node : Node, t : Transformation, field : java.lang.reflect.Field, oldNode : Any) = {
    var subnode = oldNode

    if (subnode.isInstanceOf[Some[_]]) subnode = subnode.asInstanceOf[Some[_]].get

    if (subnode.isInstanceOf[Node]) {
      var newSubnode = applyAtNode(subnode.asInstanceOf[Node], t).get
      if (newSubnode ne subnode.asInstanceOf[Node]) {
        field.set(node, newSubnode)
      }
      replace(newSubnode, t)
    }
  }

  protected def replace(node : Node, t : Transformation) : Unit = {
    enterNodeNotifyCollectors(node)

    node.getClass.getDeclaredFields.foreach(field => {
      val accessible = field.isAccessible()
      field.setAccessible(true)
      var currentSubnode = field.get(node)

      if (currentSubnode.isInstanceOf[Seq[_]]) {
        var list = currentSubnode.asInstanceOf[Seq[_]]
        
        var invalids = 0
        var nodes = 0
        var somenodes = 0
    
        list.foreach(p => p match {
          case n : Node => nodes += 1
          case n : Some[_] => if(n.get.isInstanceOf[Node]) somenodes += 1
          case _ => invalids += 1
        })

        if (invalids <= 0) {
          var newList = list.asInstanceOf[Seq[Node]].map(listitem => applyAtNode(listitem, t).get) // FIXME asof[List[Option[Node]]]
          newList = newList.filterNot(listitem => listitem eq None)
          field.set(node, newList)
          newList.foreach(f => replace(f, t))
        }
      } else {
        doReplace(node, t, field, currentSubnode)
      }
      field.setAccessible(accessible)
    })

    leaveNodeNotifyCollectors(node)
  }

  def defaultApply(strategy : Strategy) : Boolean = {
    // start transformation transaction
    History.transaction(strategy)

    try {
      strategy.transformations.foreach(t => {
        replace(root, t) // FIXME better handling if ret == false in traversal => i.e. abort traversal
      })
      History.commit
      return true
    } catch {
      case x : TransformationException => {
        WARN(f"""Strategy "${strategy.name}" did not apply successfully""")
        WARN(f"Message: ${x.msg}")
        WARN(f"Rollback will be performed")
        History.abort
        return false
      }
    }
  }
  
  def apply(t : Transformation) = { // Hack
    replace(root, t)
  }
}