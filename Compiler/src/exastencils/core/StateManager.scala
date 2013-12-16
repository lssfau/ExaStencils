package exastencils.core

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import scala.util.control.Exception
import java.lang.reflect.Method
import exastencils.core.collectors._
import exastencils.datastructures._
import exastencils.datastructures.l4._

import scala.reflect.runtime.{ universe => ru }
import scala.reflect.runtime.{ currentMirror => rm }

object StateManager {
  def root = root_
  var root_ : Node = null //= ForStatement(VariableDeclarationStatement(Variable("i", IntegerDatatype()), Some(Constant(1))), new Constant(7), new Constant(11), List[Statement]())

  protected object History {
    var current_ : Option[ProtocalEntry] = None
    protected var history_ = new Stack[ProtocalEntry]
    protected class ProtocalEntry(val stateBefore : Node, val appliedStrategy : Strategy)

    def transaction(strategy : Strategy) = {
      if (!current_.isEmpty) throw new RuntimeException("Another transaction currently running!")

      current_ = Some(new ProtocalEntry(Duplicate(StateManager.root_), strategy))
    }

    def commit() = {
      if (current_.isEmpty) throw new RuntimeException("No currently running transaction!")
      history_.push(current_.get)
      current_ = None
    }

    def abort() = {
      if (current_.isEmpty) throw new RuntimeException("No currently running transaction!")
      root_ = current_.get.stateBefore
      current_ = None
    }
  }

  protected object Collectors {
    protected var collectors_ = new ListBuffer[Collector]

    def notifyEnter(node : Node) = { collectors_.foreach(c => c.enter(node)) }
    def notifyLeave(node : Node) = { collectors_.foreach(c => c.leave(node)) }

    def register(c : Collector) = { collectors_ += c }
    def unregister(c : Collector) = { collectors_ -= c }
    def unregisterAll() = { collectors_.clear }
  }

  def register(c : Collector) = { Collectors.register(c) }
  def unregister(c : Collector) = { Collectors.unregister(c) }
  def unregisterAll() = { Collectors.unregisterAll }
  
  protected class TransformationProgress {
    protected var matches = 0
    protected var replacements = 0
    def getMatches = matches
    def getReplacements = replacements
    def didMatch = matches += 1
    def didReplace = replacements += 1
  }
  protected val progresses_ = new Stack[TransformationProgress]

  protected def applyAtNode(node : Node, transformation : Transformation) : Option[Node] = {
    if (transformation.function.isDefinedAt(node)) {
      progresses_.head.didMatch
      transformation.function(node)
    } else {
      Some(node)
    }
  }

  protected def doReplace(node : Node, transformation : Transformation, method : Method, oldNode : Any) = {
    var subnode = oldNode

    if (subnode.isInstanceOf[Some[_]]) subnode = subnode.asInstanceOf[Some[_]].get

    if (subnode.isInstanceOf[Node]) {
      var newSubnode = applyAtNode(subnode.asInstanceOf[Node], transformation).get
      if (newSubnode ne subnode.asInstanceOf[Node]) {
        if(Vars.set(node, method, newSubnode)) progresses_.head.didReplace
      }
      if(transformation.recursive) replace(newSubnode, transformation)
    }
  }

  protected def replace(node : Node, transformation : Transformation) : Unit = {
    Collectors.notifyEnter(node)

    Vars(node).foreach(field => {
      val currentSubnode = Vars.get(node, field)

      if (currentSubnode.isInstanceOf[Seq[_]]) {
        var list = currentSubnode.asInstanceOf[Seq[_]]
        val invalids = list.filter(p => !(p.isInstanceOf[Node] || p.isInstanceOf[Some[_]] && p.asInstanceOf[Some[Object]].get.isInstanceOf[Node]))
        if (invalids.size <= 0) {
          var newList = list.asInstanceOf[Seq[Node]].map(listitem => applyAtNode(listitem, transformation).get) // FIXME asof[List[Option[Node]]]
          //          newList = newList.filterNot(listitem => listitem eq None) // FIXME
          Vars.set(node, field, newList)
          newList.foreach(f => replace(f, transformation))
        }

      } else if (currentSubnode.isInstanceOf[Array[_]]) {
        var list = currentSubnode.asInstanceOf[Array[_]]
        val arrayType = list.getClass().getComponentType()
        val invalids = list.filter(p => !(p.isInstanceOf[Node] || p.isInstanceOf[Some[_]] && p.asInstanceOf[Some[Object]].get.isInstanceOf[Node]))
        if (invalids.size <= 0) {
          var tmpArray = list.asInstanceOf[Array[Node]].map(listitem => applyAtNode(listitem, transformation).get)
          var newArray = java.lang.reflect.Array.newInstance(arrayType, tmpArray.length)
          System.arraycopy(tmpArray, 0, newArray, 0, tmpArray.length)
          Vars.set(node, field, newArray)
          newArray.asInstanceOf[Array[Node]].foreach(f => replace(f, transformation))
        }

      } else {
        doReplace(node, transformation, field, currentSubnode)
      }

    })
    Collectors.notifyLeave(node)
  }

  def defaultApply(strategy : Strategy) : Option[StrategyResult] = {
    // start transformation transaction
    History.transaction(strategy)
    DBG(s"""Applying stragety "${strategy.name}"""")
    try {
      var results = new ListBuffer[TransformationResult]
      strategy.transformations.foreach(transformation => {
        progresses_.push(new TransformationProgress)
        INFO(s"""Applying stragety "${strategy.name}" :: ${transformation.name}""")
        replace(root, transformation)
        var progress = progresses_.pop
        var result = new TransformationResult(true, progress.getMatches, progress.getReplacements)
        results.append(result)
      })
      History.commit
      return Some(new StrategyResult(results.toList))
    } catch {
      case x : StrategyException => {
        WARN(f"""Strategy "${strategy.name}" did not apply successfully""")
        WARN(f"Message: ${x.msg}")
        WARN(f"Rollback will be performed")
        History.abort
        return None
      }
      case x : TransformationException => {
        WARN(f"""Strategy "${strategy.name}" did not apply successfully""")
        WARN(f"""Error in Transformation ${x.transformation.name}""")
        WARN(f"Message: ${x.msg}")
        WARN(f"Rollback will be performed")
        History.abort
        return None
      }
    }
  }

  def apply(transformation : Transformation) = { // Hack
    replace(root, transformation)
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
      if (o == value) return true
      if (!method.getName.endsWith(setterSuffix)) {
        set(o, method.getName, value)
      } else {
        if (!method.getParameterTypes()(0).getClass.isAssignableFrom(value.getClass)) {
          val from = method.getParameterTypes()(0)
          val to = value.getClass
          throw new ValueSetException(s"""Invalid assignment: Cannot assign to $to from $from for "$o"""")
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
}