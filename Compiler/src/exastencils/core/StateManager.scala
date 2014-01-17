package exastencils.core

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import scala.util.control.Exception
import java.lang.reflect.Method
import exastencils.core.collectors._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.datastructures.Transformation._

import scala.reflect.runtime.{ universe => ru }
import scala.reflect.runtime.{ currentMirror => rm }

object StateManager {
  def root = root_ // FIXME remove this
  var root_ : Node = null // FIXME make this protected

  protected object History {
    protected var currentEntry : Option[ProtocalEntry] = None
    protected var history = new Stack[ProtocalEntry]
    protected var currentToken : Option[TransactionToken] = None
    protected var currentTokenCounter = 0
    protected class ProtocalEntry(val stateBefore : Node, val appliedStrategy : Strategy)
    final class TransactionToken(val id : Int)

    def transaction(strategy : Strategy) : TransactionToken = {
      if (currentToken != None || !currentEntry.isEmpty) throw new RuntimeException("Another transaction currently running!")
      currentEntry = Some(new ProtocalEntry(Duplicate(StateManager.root_), strategy))
      val ret = new TransactionToken(currentTokenCounter)
      currentToken = Some(ret)
      return ret
    }

    def commit(token : TransactionToken) : Unit = {
      if (currentToken == None || currentEntry.isEmpty) throw new RuntimeException("No currently running transaction!")
      if (!isValid(token)) throw new RuntimeException("Wrong token supplied, transaction not committed!")
      history.push(currentEntry.get)
      currentEntry = None
      currentToken = None
    }

    def abort(token : TransactionToken) : Unit = {
      if (currentToken == None || currentEntry.isEmpty) throw new RuntimeException("No currently running transaction!")
      if (!isValid(token)) throw new RuntimeException("Wrong token supplied, transaction not committed!")
      root_ = currentEntry.get.stateBefore
      currentEntry = None
      currentToken = None
    }

    def isValid(token : TransactionToken) = { currentToken != None && token == currentToken.get }
  }
  def transaction(strategy : Strategy) = History.transaction(strategy)
  def commit(token : History.TransactionToken) = History.commit(token)
  def abort(token : History.TransactionToken) = History.abort(token)

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
    override def toString = { s"Transformation Progress: $matches match(es), $replacements replacement(s)" }
  }
  protected val progresses_ = new HashMap[Transformation, TransformationProgress]

  protected def applyAtNode(node : Node, transformation : Transformation) : Transformation.Output[_] = {
    if (transformation.function.isDefinedAt(node)) {
      progresses_(transformation).didMatch
      val ret = transformation.function(node)

      def processResult[O <: Output[_]](o : O) : Unit = o.inner match {
        case n : Node      => if (n != node) progresses_(transformation).didReplace
        case l : List[_]   => progresses_(transformation).didReplace // FIXME count of list?!
        case n : None.type => if (node != None) progresses_(transformation).didReplace
        case _             =>
      }

      processResult(ret)

      return ret
    } else {
      Output(node)
    }
  }

  protected def isTransformable(o : Any) : Boolean = o match {
    case x : Node                    => true
    case x : Some[_] if (!x.isEmpty) => isTransformable(x.get)
    case _                           => false
  }

  protected def processOutput[O <: Output[_]](o : O) : List[Node] = o.inner match {
    case n : Node      => List(n)
    case l : List[_]   => l.filter(p => p.isInstanceOf[Node]).asInstanceOf[List[Node]]
    case n : None.type => List()
    case _             => ERROR(o); List()
  }

  protected def replace(node : Node, transformation : Transformation) : Unit = {
    Collectors.notifyEnter(node)

    Vars(node).foreach(field => {
      val currentSubnode = Vars.get(node, field)
      val previousReplacements = progresses_(transformation).getReplacements

      currentSubnode match {
        case list : Seq[_] => {
          val invalids = list.filter(p => !(p.isInstanceOf[Node] || p.isInstanceOf[Some[_]] && p.asInstanceOf[Some[Object]].get.isInstanceOf[Node]))
          if (invalids.size <= 0) {
            var newList = list.asInstanceOf[Seq[Node]].flatMap(listitem => processOutput(applyAtNode(listitem, transformation)))
            var changed = newList.diff(list)
            if (changed.size > 0) {
              if (!Vars.set(node, field, newList)) {
                ERROR(s"Could not set $field")
              }
            }
            if (transformation.recursive || (!transformation.recursive && changed.size <= 0)) newList.foreach(f => replace(f, transformation))
          }
        }
        case map : Map[_, _] => {
          val invalids = map.filterNot(p => isTransformable(p._2))
          if (invalids.size <= 0) {

            def processOutput[O <: Output[_]](o : O) : Node = o.inner match {
              case n : Node      => n
              case l : List[_]   =>
                ERROR("FIXME"); null //l.filter(p => p.isInstanceOf[Node]).asInstanceOf[List[Node]]
              case n : None.type =>
                ERROR("FIXME"); null //List()
              case _             => ERROR(o); null
            }

            import scala.language.existentials
            var newMap = map.asInstanceOf[Map[_, Node]].map({ case (k, listitem) => (k, processOutput(applyAtNode(listitem, transformation))) })
            val changed = newMap.values.toList.diff(map.values.toList)
            if (changed.size > 0) {
              if (!Vars.set(node, field, newMap)) {
                ERROR(s"Could not set $field")
              }
            }
          }
        }
        case list : Array[_] => {
          val arrayType = list.getClass().getComponentType()
          val invalids = list.filter(p => !(p.isInstanceOf[Node] || p.isInstanceOf[Some[_]] && p.asInstanceOf[Some[Object]].get.isInstanceOf[Node]))
          if (invalids.size <= 0) {
            var tmpArray = list.asInstanceOf[Array[Node]].flatMap(listitem => processOutput(applyAtNode(listitem, transformation)))
            var changed = tmpArray.diff(list)
            if (changed.size > 0) {
              var newArray = java.lang.reflect.Array.newInstance(arrayType, tmpArray.length)
              System.arraycopy(tmpArray, 0, newArray, 0, tmpArray.length)
              if (!Vars.set(node, field, newArray)) {
                ERROR(s"Could not set $field")
              }
            }
            if (transformation.recursive || (!transformation.recursive && changed.size <= 0)) tmpArray.asInstanceOf[Array[Node]].foreach(f => replace(f, transformation))
          }
        }
        case node : Node => {
          var subnode = node
          var nodeIsOption = false
          if (subnode.isInstanceOf[Some[_]]) {
            nodeIsOption = true
            var somenode = subnode.asInstanceOf[Some[_]].get
            if (somenode.isInstanceOf[Node]) subnode = somenode.asInstanceOf[Node]
          }

          if (subnode.isInstanceOf[Node]) {
            def processResult[O <: Output[_]](o : O) = o.inner match {
              case n : Node => {
                if (nodeIsOption) { // node is an Option[T] => set with Some() wrapped
                  if (!Vars.set(node, field, Some(n))) {
                    ERROR(s"Could not set $field")
                  }
                } else { // node is not an Option[T] => set directly
                  if (!Vars.set(node, field, n)) {
                    ERROR(s"Could not set $field")
                  }
                }
                if (transformation.recursive) replace(n, transformation)
              }
              case l : List[_] => {
                // FIXME
              }
              case n : None.type => {
                ERROR(s"Could not set $field to an empty node") // FIXME think of better way => e.g. empty dummy node
              }
            }
            var newSubnode = applyAtNode(subnode.asInstanceOf[Node], transformation)
            processResult(newSubnode)
          }
        }
        case _ =>
      }
    })
    Collectors.notifyLeave(node)
  }

  def apply(token : History.TransactionToken, transformation : Transformation, node : Option[Node] = None) : TransformationResult = {
    if (!History.isValid(token)) {
      throw new RuntimeException("Invalid transaction token for transformation ${transformation.name}")
    }
    try {
      progresses_.+=((transformation, new TransformationProgress))
      replace(node.getOrElse(root), transformation)
      return new TransformationResult(true, progresses_(transformation).getMatches, progresses_(transformation).getReplacements)
    } catch {
      case x : TransformationException => {
        WARN(f"""Error in Transformation ${x.transformation.name}""")
        WARN(f"Message: ${x.msg}")
        WARN(f"Rollback will be performed")
        abort(token)
        throw x
      }
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
      if (o == value) return true
      if (!method.getName.endsWith(setterSuffix)) {
        set(o, method.getName, value)
      } else {
        if (!method.getParameterTypes()(0).isAssignableFrom(value.getClass)) {
          val from = method.getParameterTypes()(0)
          val to = value.getClass
          throw new ValueSetException(s"""Invalid assignment: Cannot assign to $to from $from for "$o", method "${method.getName}"""")
        }
        method.invoke(o, value)
        //WARN("successfully set " + method.getName())
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