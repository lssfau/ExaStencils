package exastencils.core

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import exastencils.core.collectors._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import scala.util.control.Exception

import scala.reflect.runtime.{ universe => ru }
import scala.reflect.runtime.{ currentMirror => rm }

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

  protected def doReplace[T](node : Node, t : Transformation, method : java.lang.reflect.Method, oldNode : Any) = {
    var subnode = oldNode

    if (subnode.isInstanceOf[Some[_]]) subnode = subnode.asInstanceOf[Some[_]].get

    if (subnode.isInstanceOf[Node]) {
      var newSubnode = applyAtNode(subnode.asInstanceOf[Node], t).get
      if (newSubnode ne subnode.asInstanceOf[Node]) {
        Vars.set(node, method, newSubnode)
      }
      replace(newSubnode, t)
    }
  }

  protected object Vars {
    val setterSuffix = "_$eq"
    val excludeList = List()

    def apply[T](o : AnyRef) : List[java.lang.reflect.Method] = {
      val methods = o.getClass.getMethods
      val getters : Array[java.lang.reflect.Method] = for {
        g <- methods; if (g.getModifiers & java.lang.reflect.Modifier.PUBLIC) == java.lang.reflect.Modifier.PUBLIC &&
          g.getParameterTypes.size == 0 && !excludeList.contains(g.getName)
        s <- methods; if s.getName == g.getName + setterSuffix &&
          (s.getModifiers & java.lang.reflect.Modifier.PUBLIC) == java.lang.reflect.Modifier.PUBLIC &&
          s.getParameterTypes.size == 1 && s.getParameterTypes()(0) == g.getReturnType
      } yield g

      getters.toList
    }

    def get[T](o : AnyRef, method : java.lang.reflect.Method) : AnyRef = {
      method.invoke(o)
    }

    def set[T](o : AnyRef, method : java.lang.reflect.Method, value : AnyRef) : Boolean = {
      if (!method.getName.endsWith(setterSuffix)) {
        set(o, method.getName, value)
      } else {
        println("value: " + value)
        if(!method.getParameterTypes()(0).getClass.isAssignableFrom(value.getClass)) {
          val from = method.getParameterTypes()(0)
          val to = value.getClass
          throw new TransformationException(f"""Invalid assignment: Cannot assign from $from to $to for "$o"""")
        }
        method.invoke(o, value)
        true
      }
    }

    def set[T](o : AnyRef, method : String, value : AnyRef) : Boolean = {
      var methodname = method
      if (!methodname.endsWith(setterSuffix)) {
        methodname += setterSuffix
      }
      val m = o.getClass.getMethods.find(p => p.getName == methodname)
      if (m == None) false
      set(o, m.get, value)
    }
  }

  protected def replace(node : Node, t : Transformation) : Unit = {
    enterNodeNotifyCollectors(node)

    Vars(node).foreach(field => {
      val currentSubnode = Vars.get(node, field)

      if (currentSubnode.isInstanceOf[Seq[_]]) {
        var list = currentSubnode.asInstanceOf[Seq[_]]
        val invalids = list.filter(p => !(p.isInstanceOf[Node] || p.isInstanceOf[Some[_]] && p.asInstanceOf[Some[Object]].get.isInstanceOf[Node]))
        if (invalids.size <= 0) {
          var newList = list.asInstanceOf[Seq[Node]].map(listitem => applyAtNode(listitem, t).get) // FIXME asof[List[Option[Node]]]
          //          newList = newList.filterNot(listitem => listitem eq None) // FIXME
          Vars.set(node, field, newList)
          newList.foreach(f => replace(f, t))
        }

      } else if (currentSubnode.isInstanceOf[Array[_]]) {
        var list = currentSubnode.asInstanceOf[Array[_]]

        val invalids = list.filter(p => !(p.isInstanceOf[Node] || p.isInstanceOf[Some[_]] && p.asInstanceOf[Some[Object]].get.isInstanceOf[Node]))
        if (invalids.size <= 0) {
          var newList = list.asInstanceOf[Array[Node]].map(listitem => applyAtNode(listitem, t).get) // FIXME asof[List[Option[Node]]]
          //          newList = newList.filterNot(listitem => listitem eq None) // FIXME
          Vars.set(node, field, newList)
          newList.foreach(f => replace(f, t))
        }
      } else {
        doReplace(node, t, field, currentSubnode)
      }

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