package exastencils.core

import scala.collection.mutable.ListBuffer
import exastencils.core.collectors._
import exastencils.datastructures._
import exastencils.datastructures.l4._

object TreeManager {
  protected var root_ : Node = AbstractForLoop(new AbstractVariableDeclarationStatement(AbstractVariable("i", IntegerDatatype), Some(new AbstractConstantExpression(1))),
    new AbstractConstantExpression(7), new AbstractConstantExpression(11))
  protected var collectors_ = new ListBuffer[Collector]
  
  def root = root_ // FIXME remove this later

  protected def enterNodeNotifyCollectors(node : Node) = { collectors_.foreach(c => c.enter(node)) }
  protected def leaveNodeNotifyCollectors(node : Node) = { collectors_.foreach(c => c.leave(node)) }

  def register(c : Collector) = { collectors_ += c }
  def unregister(c : Collector) = { collectors_ -= c }
  def unregisterAll() = { collectors_.clear }

  val defStack = new StackCollector
  
  
  protected def replaceSubnode(node : Node, oldSub : Node, newSub : Node) : Unit = {
    node.getClass.getDeclaredFields.foreach(f => {
      val a = f.isAccessible()
      f.setAccessible(true)
      if (f.get(node) == oldSub) {
        f.set(node, newSub)
      }
      f.setAccessible(a)
    })
  }

  protected def apply(node : Node, f : Function[Node, Unit]) : Unit = {
    f(node)

    enterNodeNotifyCollectors(node)
    node.getClass.getDeclaredFields.foreach(n => {
      val method = node.getClass.getDeclaredMethods.find(m => m.getName == n.getName)
      if (method.nonEmpty) {
        var obj : Object = null
        obj = method.get.invoke(node).asInstanceOf[Object]
        if (obj.isInstanceOf[Node]) apply(obj.asInstanceOf[Node], f)
      }
    })
    leaveNodeNotifyCollectors(node)
  }

  protected def apply(node : Node, t : Transformation) : Unit = { // FIXME save previous in recursive call (or custom stack)
    val ret = t.apply(node)
    if ((ret != None) && (ret.get ne node)) {
      replaceSubnode(defStack.head, node, ret.get)
    }

    enterNodeNotifyCollectors(node)
    node.getClass.getDeclaredFields.foreach(n => {
      val method = node.getClass.getDeclaredMethods.find(m => m.getName == n.getName)
      if (method.nonEmpty) {
        var obj : Object = null
        obj = method.get.invoke(node).asInstanceOf[Object]
        if (obj.isInstanceOf[Node]) apply(obj.asInstanceOf[Node], t)
      }
    })
    leaveNodeNotifyCollectors(node)
  }

  def apply(transformation : Transformation) : Unit = {
    defStack.reset
    register(defStack)
    apply(root, transformation)
    unregister(defStack)
  }

  def apply(func : Function[Node, Unit]) : Unit = {
    defStack.reset
    register(defStack)
    apply(root, func)
    unregister(defStack)
  }
}