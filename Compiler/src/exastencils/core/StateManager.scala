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
import scala.reflect.ClassTag

object StateManager {
  def root = root_ // FIXME remove this
  var root_ : Node = null // FIXME make this protected

  /* HACK: used to be protected */ object History {
    protected var currentEntry : Option[ProtocalEntry] = None
    protected var history = new Stack[ProtocalEntry]
    /* HACK: used to be protected */ var currentToken : Option[TransactionToken] = None
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
        case n : None.type => progresses_(transformation).didReplace
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
    case _             => ERROR(o)
  }

  def doRecursiveMatch(thisnode : Any, node : Node, field : java.lang.reflect.Method, transformation : Transformation) = {
    var subnode = thisnode
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
              ERROR(s"Could not set $field in transformation ${transformation.name}")
            }
          } else { // node is not an Option[T] => set directly
            if (!Vars.set(node, field, n)) {
              ERROR(s"Could not set $field in transformation ${transformation.name}")
            }
          }
          if (transformation.recursive) replace(n, transformation)
        }
        case l : List[_] => {
          ERROR(s"Could not replace single node by List in transformation ${transformation.name}")
        }
        case n : None.type => {
          ERROR(s"Could not set $field to an empty node in transformation ${transformation.name}") // FIXME think of better way => e.g. empty dummy node
        }
      }
      val previousReplacements = progresses_(transformation).getReplacements
      var newSubnode = applyAtNode(subnode.asInstanceOf[Node], transformation)
      if(previousReplacements <= progresses_(transformation).getReplacements) processResult(newSubnode)
    }
  }

  protected def replace(node : Node, transformation : Transformation) : Unit = {
    Collectors.notifyEnter(node)

    Vars(node).foreach(field => {
      val currentSubnode = Vars.get(node, field)
      val previousReplacements = progresses_(transformation).getReplacements
      INFO(s"Statemanager::replace: node = $node, field = $field, currentSubnode = $currentSubnode")

      currentSubnode match {
        case thisnode : Node => {
          doRecursiveMatch(thisnode, node, field, transformation)
        }
        case Some(thisnode) => {
          if (thisnode.isInstanceOf[Node]) {
            doRecursiveMatch(Some(thisnode.asInstanceOf[Node]), node, field, transformation) // FIXME not very elegant
          }
        }
        case set : scala.collection.mutable.Set[_] => {
          val invalids = set.filterNot(p => isTransformable(p))
          if (invalids.size <= 0) {
            def processOutput[O <: Output[_]](o : O) : Node = o.inner match {
              case n : Node => n
              case l : List[_] =>
                ERROR("FIXME") //l.filter(p => p.isInstanceOf[Node]).asInstanceOf[List[Node]]
              case n : None.type =>
                ERROR("FIXME") //List()
              case _ => ERROR(o); null
            }

            import scala.language.existentials
            var newSet = set.asInstanceOf[scala.collection.mutable.Set[Node]].map({ case item => processOutput(applyAtNode(item, transformation)) })
            if (previousReplacements <= progresses_(transformation).getReplacements && !Vars.set(node, field, newSet)) {
              ERROR(s"Could not set $field in transformation ${transformation.name}")
            }
            if (transformation.recursive || (!transformation.recursive && previousReplacements >= progresses_(transformation).getReplacements)) newSet.foreach(f => replace(f, transformation))
          }
        }
        case set : scala.collection.immutable.Set[_] => {
          val invalids = set.filterNot(p => isTransformable(p))
          if (invalids.size <= 0) {
            def processOutput[O <: Output[_]](o : O) : Node = o.inner match {
              case n : Node => n
              case l : List[_] =>
                ERROR("FIXME") //l.filter(p => p.isInstanceOf[Node]).asInstanceOf[List[Node]]
              case n : None.type =>
                ERROR("FIXME") //List()
              case _ => ERROR(o); null
            }

            import scala.language.existentials
            var newSet = set.asInstanceOf[scala.collection.immutable.Set[Node]].map({ case item => processOutput(applyAtNode(item, transformation)) })
            if (previousReplacements <= progresses_(transformation).getReplacements && !Vars.set(node, field, newSet)) {
              ERROR(s"Could not set $field in transformation ${transformation.name}")
            }
            if (transformation.recursive || (!transformation.recursive && previousReplacements >= progresses_(transformation).getReplacements)) newSet.foreach(f => replace(f, transformation))
          }
        }
        case list : Seq[_] => {
          val invalids = list.filter(p => !(p.isInstanceOf[Node] || p.isInstanceOf[Some[_]] && p.asInstanceOf[Some[Object]].get.isInstanceOf[Node]))
          if (invalids.size <= 0) {
            var newList = list.asInstanceOf[Seq[Node]].flatMap(listitem => processOutput(applyAtNode(listitem, transformation)))
            if (previousReplacements <= progresses_(transformation).getReplacements && !Vars.set(node, field, newList)) {
              ERROR(s"Could not set $field in transformation ${transformation.name}")
            }
            if (transformation.recursive || (!transformation.recursive && previousReplacements >= progresses_(transformation).getReplacements)) newList.foreach(f => replace(f, transformation))
          }
        }
        case map : scala.collection.mutable.Map[_, _] => {
          val invalids = map.filterNot(p => isTransformable(p._2))
          if (invalids.size <= 0) {

            def processOutput[O <: Output[_]](o : O) : Node = o.inner match {
              case n : Node => n
              case l : List[_] =>
                ERROR("FIXME") //l.filter(p => p.isInstanceOf[Node]).asInstanceOf[List[Node]]
              case n : None.type =>
                ERROR("FIXME") //List()
              case _ => ERROR(o); null
            }

            import scala.language.existentials
            var newMap = map.asInstanceOf[scala.collection.mutable.Map[_, Node]].map({ case (k, listitem) => (k, processOutput(applyAtNode(listitem, transformation))) })
            if (previousReplacements <= progresses_(transformation).getReplacements && !Vars.set(node, field, newMap)) {
              ERROR(s"Could not set $field in transformation ${transformation.name}")
            }
            if (transformation.recursive || (!transformation.recursive && previousReplacements >= progresses_(transformation).getReplacements)) newMap.values.foreach(f => replace(f, transformation))
          }
        }
        case map : scala.collection.immutable.Map[_, _] => {
          val invalids = map.filterNot(p => isTransformable(p._2))
          if (invalids.size <= 0) {

            def processOutput[O <: Output[_]](o : O) : Node = o.inner match {
              case n : Node => n
              case l : List[_] =>
                ERROR("FIXME") //l.filter(p => p.isInstanceOf[Node]).asInstanceOf[List[Node]]
              case n : None.type =>
                ERROR("FIXME") //List()
              case _ => ERROR(o); null
            }

            import scala.language.existentials
            var newMap = map.asInstanceOf[scala.collection.immutable.Map[_, Node]].map({ case (k, listitem) => (k, processOutput(applyAtNode(listitem, transformation))) })
            if (previousReplacements <= progresses_(transformation).getReplacements && !Vars.set(node, field, newMap)) {
              ERROR(s"Could not set $field in transformation ${transformation.name}")
            }
            if (transformation.recursive || (!transformation.recursive && previousReplacements >= progresses_(transformation).getReplacements)) newMap.values.foreach(f => replace(f, transformation))
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
                ERROR(s"Could not set $field in transformation ${transformation.name}")
              }
            }
            if (transformation.recursive || (!transformation.recursive && changed.size <= 0)) tmpArray.asInstanceOf[Array[Node]].foreach(f => replace(f, transformation))
          }
        }
        case _ =>
      }
    })
    Collectors.notifyLeave(node)
  }

  //  def applyTransformationAtElement(element : Node, transformation : Transformation) : Transformation.Output[_] = {
  //    if (transformation.function.isDefinedAt(element)) {
  //      progresses_(transformation).didMatch
  //      val ret = transformation.function(element)
  //
  //      def processResult[O <: Output[_]](o : O) : Unit = o.inner match {
  //        case n : Node      => if (n != element) progresses_(transformation).didReplace
  //        case l : List[_]   => progresses_(transformation).didReplace // FIXME count of list?!
  //        case n : None.type => if (element != None) progresses_(transformation).didReplace
  //        case _             =>
  //      }
  //
  //      ret.inner match {
  //        case n : Node      => List(n)
  //        case l : List[_]   => l.filter(p => p.isInstanceOf[Node]).asInstanceOf[List[Node]]
  //        case n : None.type => List()
  //        case _             => ERROR(ret)
  //      }
  //    } else {
  //      Output(element)
  //    }
  //  }  
  //  def traverse(element : Any, transformation : Transformation) : Any = {
  //    if (element.isInstanceOf[Node]) Collectors.notifyEnter(element.asInstanceOf[Node])
  //    Vars(element).foreach(field => {
  //      val currentSubnode = Vars.get(element, field)
  //      currentSubnode match {
  //        case n : Node                     => Vars.set(element, field, applyTransformationAtElement(n, transformation).inner)
  //        case Some(n)                      => Vars.set(element, field, Some(traverse(n, transformation)))
  //        case traversable : Traversable[_] => Vars.set(element, field, traversable.map(traverse(_, transformation)))
  //        //        case map : Map[_, _]              => 
  //        case (a, b)                       => (traverse(a, transformation), traverse(b, transformation))
  //        case a : Array[_]                 => // FIXME
  //        case _                            => currentSubnode
  //      }
  //    })
  //    if (element.isInstanceOf[Node]) Collectors.notifyLeave(element.asInstanceOf[Node])
  //  }

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

  def layerChange(token : History.TransactionToken, newRoot : Node) : Unit = {
    if (!History.isValid(token)) {
      throw new RuntimeException("Invalid transaction token for layer change")
    }
    this.root_ = newRoot
  }

  def findFirst[T : ClassTag]() : Option[T] = findFirst(root)
  
  def findFirst[T : ClassTag](node : Node) : Option[T] = {
    findFirst[T]({ x : Any => x match { case _ : T => true; case _ => false } })
  }

  def findFirst[T : ClassTag](check : Any => Boolean, node : Node = root) : Option[T] = {
    var retVal : Option[T] = None
    var t = new Transformation("StatemanagerInternalFindFirst", {
      case hit : T if check(hit) =>
        retVal = Some(hit)
        new Output(hit)
    }, false)

    progresses_.+=((t, new TransformationProgress))
    replace(node, t)

    retVal
  }

  protected object Vars {
    val setterSuffix = "_$eq"
    val excludeList = List()

    def apply[T](o : Any) : List[java.lang.reflect.Method] = {
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

    def get[T](o : Any, method : java.lang.reflect.Method) : Any = {
      method.invoke(o)
    }

    def set[T](o : Any, method : java.lang.reflect.Method, value : Any) : Boolean = {
      INFO(s"Statemananger::set: $o, " + method.getName() + s" to $value")
      if (o == value) return true
      if (!method.getName.endsWith(setterSuffix)) {
        set(o, method.getName, value)
      } else {
        if (!method.getParameterTypes()(0).isAssignableFrom(value.getClass)) {
          val from = method.getParameterTypes()(0)
          val to = value.getClass
          throw new ValueSetException(s"""Invalid assignment: Cannot assign to $to from $from for "$o", method "${method.getName}"""")
        }
        method.invoke(o, value.asInstanceOf[AnyRef])
        true
      }
    }

    def set[T](o : Any, method : String, value : Any) : Boolean = {
      var methodname = method
      if (!methodname.endsWith(setterSuffix)) {
        methodname += setterSuffix
      }
      INFO(s"Setting $o :: $method to $value")
      val m = o.getClass.getMethods.find(p => p.getName == methodname)
      m match {
        case Some(x) => set(o, x, value)
        case None    => false
      }
    }
  }
}