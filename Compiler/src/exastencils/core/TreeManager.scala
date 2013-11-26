package exastencils.core

import scala.collection.mutable.ListBuffer
import exastencils.core.collectors._
import exastencils.datastructures._

object TreeManager {
  var root : Node = AbstractForLoop(new AbstractVariableDeclarationStatement(AbstractVariable("i", IntegerDatatype), Some(new AbstractConstantExpression(1))),
    new AbstractConstantExpression(7), new AbstractConstantExpression(11))
  protected var collectors_ = new ListBuffer[Collector]

  protected def enterNodeNotifyCollectors(node : Node) = { collectors_.foreach(c => c.enter(node)) }
  protected def leaveNodeNotifyCollectors(node : Node) = { collectors_.foreach(c => c.leave(node)) }

  def register(c : Collector) = { collectors_ += c }
  def unregister(c : Collector) = { collectors_ -= c }
  def unregisterAll() = { collectors_.clear }

  // FIXME function that builds transformations (maybe even adds default case)

  //  def createTrafo(n : Node, f : Unit) : PartialFunction[Node, Unit] = {
  //    val x : PartialFunction[Node, Unit] = { (n, f) }
  //    return x
  //  }

  val defStack = new StackCollector

  protected def applyViaReflection(node : Node, f : Function[Node, Unit]) : Unit = {
    f(node)

    enterNodeNotifyCollectors(node)
    node.getClass.getDeclaredFields.foreach(n => {
      val method = node.getClass.getDeclaredMethods.find(m => m.getName == n.getName)
      if (method.nonEmpty) {
        var obj : Object = null
        obj = method.get.invoke(node).asInstanceOf[Object]
        if (obj.isInstanceOf[Node]) applyViaReflection(obj.asInstanceOf[Node], f)
      }
    })
    leaveNodeNotifyCollectors(node)
  }

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

  protected def applyT(node : Node, t : Transformation) : Unit = { // FIXME save previous in recursive call (or custom stack)
    val ret = t.apply(node)
    if ((ret != None) && (ret.get ne node)) {
      //node = ret.get
      replaceSubnode(defStack.head, node, ret.get)
    }

    enterNodeNotifyCollectors(node)
    node.getClass.getDeclaredFields.foreach(n => {
      val method = node.getClass.getDeclaredMethods.find(m => m.getName == n.getName)
      if (method.nonEmpty) {
        var obj : Object = null
        obj = method.get.invoke(node).asInstanceOf[Object]
        if (obj.isInstanceOf[Node]) applyT(obj.asInstanceOf[Node], t)
      }
    })
    leaveNodeNotifyCollectors(node)
  }

  def applyT(t : Transformation) : Unit = {
    register(defStack)
    applyT(root, t)
    unregister(defStack)
  }

  def apply(f : Function[Node, Unit]) : Unit = {
    applyViaReflection(root, f)
  }

  def apply(n : Node, f : Function[Node, Unit]) : Unit = {
    applyViaReflection(n, f)
  }

  protected def a2(node : Node, f : Function[Node, Node]) : Unit = {
    enterNodeNotifyCollectors(node)
    node.getClass.getDeclaredFields.foreach(n => {
      val method = node.getClass.getDeclaredMethods.find(m => m.getName == n.getName)
      if (method.nonEmpty) {
        var obj : Object = null
        obj = method.get.invoke(node).asInstanceOf[Object]
        if (obj.isInstanceOf[Node]) a2(obj.asInstanceOf[Node], f)
      }
    })
    leaveNodeNotifyCollectors(node)
  }
}