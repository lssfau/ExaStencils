package exastencils.core

import scala.collection.GenTraversableOnce
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import scala.language.existentials
import scala.reflect.ClassTag
import exastencils.core.collectors.Collector
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import java.lang.reflect.Method

object StateManager {
  def root = root_ // FIXME remove this
  var root_ : Node = null // FIXME make this protected

  type CheckpointIdentifier = String
  protected var checkpoints_ = new HashMap[CheckpointIdentifier, Node]
  def checkpoint(id : CheckpointIdentifier) : Unit = {
    Logger.debug(s"""Creating checkpoint "$id"""")
    var c = Duplicate(StateManager.root_)
    checkpoints_ += ((id, c))
  }
  def restore(id : CheckpointIdentifier) : Unit = {
    Logger.debug(s"""Restoring to checkpoint "$id"""")
    root_ = checkpoints_.getOrElse(id, { throw CheckpointException(s"""Could not restore to checkpoint "$id": Not found!""") })
  }
  def listCheckpoints() = checkpoints_.keys

  protected object History {
    protected var history = new Stack[ProtocalEntry]
    protected var currentToken : Option[TransactionToken] = None
    protected var currentTokenCounter = 0
    protected class ProtocalEntry(val stateBefore : Node, val appliedStrategy : Strategy)
    final class TransactionToken(val id : Int)

    def transaction(strategy : Strategy) : TransactionToken = {
      if (currentToken != None) throw new TransactionException(s"""Strategy "${strategy.name}": Another transaction is currently running!""")
      val ret = new TransactionToken(currentTokenCounter)
      currentToken = Some(ret)
      return ret
    }

    def commit(token : TransactionToken) : Unit = {
      if (currentToken == None) throw new TransactionException("No currently running transaction!")
      if (!isValid(token)) throw new TransactionException("Wrong token supplied, transaction not committed!")
      currentToken = None
    }

    def abort(token : TransactionToken) : Unit = {
      if (currentToken == None) throw new TransactionException("No currently running transaction!")
      if (!isValid(token)) throw new TransactionException("Wrong token supplied, transaction not committed!")
      currentToken = None
      Logger.warning("Transaction has been aborted")
    }

    def isValid(token : TransactionToken) = { currentToken != None && token == currentToken.get }
  }
  def transaction(strategy : Strategy) = History.transaction(strategy)
  def commit(token : History.TransactionToken) = History.commit(token)
  def abort(token : History.TransactionToken) = History.abort(token)
  type TokenType = History.TransactionToken

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
    def getMatches = matches
    def didMatch = matches += 1
    override def toString = { s"Transformation Progress: $matches match(es)" }
  }
  protected val progresses_ = new HashMap[Transformation, TransformationProgress]

  protected case object NoMatch extends Node
  protected def NoMatchFunction(node : Node) : Transformation.OutputType = {
    return NoMatch
  }

  protected def applyAtNode(node : Node, transformation : Transformation) : Transformation.OutputType = {
    val output = transformation.function.applyOrElse(node, NoMatchFunction)
    output.inner match {
      case NoMatch =>
      case _       => progresses_(transformation).didMatch
    }
    output
  }

  def doRecursiveMatch(thisnode : Any, node : Node, pair : (Method, Method), transformation : Transformation) = {
    val getter = pair._1
    val setter = pair._2
    var subnode = thisnode
    var nodeIsOption = false
    if (subnode.isInstanceOf[Some[_]]) {
      nodeIsOption = true
      var somenode = subnode.asInstanceOf[Some[_]].get
      if (somenode.isInstanceOf[Node]) subnode = somenode.asInstanceOf[Node]
    }

    if (subnode.isInstanceOf[Node]) {
      def processResult[O <: OutputType](o : O) = o.inner match {
        case NoMatch => {
          // no match => do nothing, i.e., do not replace node
          if (transformation.recursive) {
            replace(subnode.asInstanceOf[Node], transformation) // FIXME reihenfolge ...
          }
        }
        case n : Node => {
          if (nodeIsOption) { // node is an Option[T] => set with Some() wrapped
            if (!Vars.set(node, setter, Some(n))) {
              Logger.error(s"""Could not set "$getter" in transformation ${transformation.name}""")
            }
          } else { // node is not an Option[T] => set directly
            if (!Vars.set(node, setter, n)) {
              Logger.error(s"""Could not set "$getter" in transformation ${transformation.name}""")
            }
          }
          if (transformation.recursive) {
            replace(n, transformation) // FIXME reihenfolge ...
          }
        }
        case l : NodeList => {
          Logger.error(s"Could not replace single node by List in transformation ${transformation.name}")
        }
        case None => {
          if (nodeIsOption) {
            if (!Vars.set(node, setter, None)) {
              Logger.error(s"""Could not set "$getter" to "None" in transformation ${transformation.name}""")
            }
          }
        }
      }
      val previousMatches = progresses_(transformation).getMatches
      var newSubnode = applyAtNode(subnode.asInstanceOf[Node], transformation)
      if (previousMatches <= progresses_(transformation).getMatches) processResult(newSubnode)
    }
  }

  protected def replace(node : Node, transformation : Transformation) : Unit = {
    Collectors.notifyEnter(node)

    Vars(node).foreach(pair => {
      val getter = pair._1 // extract getter method to read element from node
      val setter = pair._2 // extract setter method to save element to node
      val currentSubnode = Vars.get(node, getter) // the current element we are working with
      val previousMatches = progresses_(transformation).getMatches // the previous number of matches for comparison

      Logger.info(s"""Statemanager::replace: node = "$node", field = "$getter", currentSubnode = "$currentSubnode"""")

      currentSubnode match {
        // ###############################################################################################
        // #### Standard nodes ###########################################################################
        // ###############################################################################################
        case n : Node => {
          doRecursiveMatch(n, node, pair, transformation) // FIXME improve recursion
        }
        case Some(n) => {
          if (n.isInstanceOf[Node]) { // need to check separately for instanceOf[Node] because of type erasure
            doRecursiveMatch(Some(n.asInstanceOf[Node]), node, pair, transformation) // FIXME improve recursion
          }
        }
        // ###############################################################################################
        // #### Sets #####################################################################################
        // ###############################################################################################
        case set : scala.collection.mutable.Set[_] => {
          var newSet = set.flatMap(f => f match {
            case n : Node => applyAtNode(n, transformation).inner match {
              case NoMatch         => List(n) // no match occured => use old element 
              case newN : Node     => List(newN) // element of type Node was returned => use it
              case newN : NodeList => newN.nodes // elements of type Node were returned => use them
            }
            case _ => List(f) // current element "f" is not of interest to us - put it back into (new) set
          })

          if (previousMatches <= progresses_(transformation).getMatches && !Vars.set(node, setter, newSet)) {
            Logger.error(s"Could not set $getter in transformation ${transformation.name}")
          }

          // Apply transformation to sub-elements
          if (transformation.recursive || (!transformation.recursive && previousMatches >= progresses_(transformation).getMatches)) {
            newSet.foreach(f => f match {
              case n : Node => replace(n, transformation)
              case _        =>
            })
          }
        }
        case set : scala.collection.immutable.Set[_] => {
          var newSet = set.flatMap(f => f match {
            case n : Node => applyAtNode(n, transformation).inner match {
              case NoMatch         => List(n) // no match occured => use old element 
              case newN : Node     => List(newN) // element of type Node was returned => use it
              case newN : NodeList => newN.nodes // elements of type Node were returned => use them
            }
            case _ => List(f)
          })

          if (previousMatches <= progresses_(transformation).getMatches && !Vars.set(node, setter, newSet)) {
            Logger.error(s"Could not set $getter in transformation ${transformation.name}")
          }

          if (transformation.recursive || (!transformation.recursive && previousMatches >= progresses_(transformation).getMatches)) {
            newSet.foreach(f => f match {
              case n : Node => replace(n, transformation)
              case _        =>
            })
          }
        }
        // ###############################################################################################
        // #### Lists, ListBuffers #######################################################################
        // ###############################################################################################
        case seq : Seq[_] => {
          var newSeq = seq.flatMap(f => f match {
            case n : Node => applyAtNode(n, transformation).inner match {
              case NoMatch         => List(n) // no match occured => use old element 
              case newN : Node     => List(newN) // element of type Node was returned => use it
              case newN : NodeList => newN.nodes // elements of type Node were returned => use them
            }
            case _ => List(f)
          })

          if (previousMatches <= progresses_(transformation).getMatches && !Vars.set(node, setter, newSeq)) {
            Logger.error(s"Could not set $getter in transformation ${transformation.name}")
          }

          if (transformation.recursive || (!transformation.recursive && previousMatches >= progresses_(transformation).getMatches)) {
            newSeq.foreach(f => f match {
              case n : Node => replace(n, transformation)
              case _        =>
            })
          }
        }
        // ###############################################################################################
        // #### Maps #####################################################################################
        // ###############################################################################################
        case map : scala.collection.mutable.Map[_, _] => {
          var newMap = map.map(f => f match {
            case n : Node => applyAtNode(n, transformation).inner match {
              case NoMatch         => n // no match occured => use old element 
              case newN : Node     => newN // element of type Node was returned => use it
              case newN : NodeList => Logger.error("fixme")
            }
            case _ => f
          })

          if (previousMatches <= progresses_(transformation).getMatches && !Vars.set(node, setter, newMap)) {
            Logger.error(s"Could not set $getter in transformation ${transformation.name}")
          }

          if (transformation.recursive || (!transformation.recursive && previousMatches >= progresses_(transformation).getMatches)) {
            newMap.foreach(f => f match {
              case n : Node => replace(n, transformation)
              case _        =>
            })
          }
        }
        // Unfortunately mutable and immutable set have no common supertype
        case map : scala.collection.immutable.Map[_, _] => {
          var newMap = map.map(f => f match {
            case n : Node => applyAtNode(n, transformation).inner match {
              case NoMatch         => n // no match occured => use old element 
              case newN : Node     => newN // element of type Node was returned => use it
              case newN : NodeList => Logger.error("fixme")
            }
            case _ => f
          })

          if (previousMatches <= progresses_(transformation).getMatches && !Vars.set(node, setter, newMap)) {
            Logger.error(s"Could not set $getter in transformation ${transformation.name}")
          }

          if (transformation.recursive || (!transformation.recursive && previousMatches >= progresses_(transformation).getMatches)) {
            newMap.foreach(f => f match {
              case n : Node => replace(n, transformation)
              case _        =>
            })
          }
        }
        //        case list : Array[_] => {
        //          val arrayType = list.getClass().getComponentType()
        //          val invalids = list.filter(p => !(p.isInstanceOf[Node] || p.isInstanceOf[Some[_]] && p.asInstanceOf[Some[Object]].get.isInstanceOf[Node]))
        //          if (invalids.size <= 0) {
        //            var tmpArray = list.asInstanceOf[Array[Node]].flatMap(listitem => processOutput(applyAtNode(listitem, transformation)))
        //            var changed = tmpArray.diff(list)
        //            if (changed.size > 0) {
        //              var newArray = java.lang.reflect.Array.newInstance(arrayType, tmpArray.length)
        //              System.arraycopy(tmpArray, 0, newArray, 0, tmpArray.length)
        //              if (!Vars.set(node, setter, newArray)) {
        //                Logger.error(s"""Could not set "$getter" in transformation ${transformation.name}""")
        //              }
        //            }
        //            if (transformation.recursive || (!transformation.recursive && changed.size <= 0)) tmpArray.asInstanceOf[Array[Node]].foreach(f => replace(f, transformation))
        //          }
        //        }
        case _ =>
      }
    })
    Collectors.notifyLeave(node)
  }


  def apply(token : History.TransactionToken, transformation : Transformation, node : Option[Node] = None) : TransformationResult = {
    if (!History.isValid(token)) {
      throw new RuntimeException(s"Invalid transaction token for transformation ${transformation.name}")
    }
    try {
      progresses_.+=((transformation, new TransformationProgress))
      replace(node.getOrElse(root), transformation)
      return new TransformationResult(true, progresses_(transformation).getMatches)
    } catch {
      case x : TransformationException => {
        Logger.warn(s"""Error in Transformation ${x.transformation.name}""")
        Logger.warn(s"Message: ${x.msg}")
        Logger.warn(s"Rollback will be performed")
        abort(token)
        throw x
      }
    }
  }

  def applyStandalone(transformation : Transformation, node : Node) : TransformationResult = {
    try {
      progresses_.+=((transformation, new TransformationProgress))
      replace(node, transformation)
      return new TransformationResult(true, progresses_(transformation).getMatches)
    } catch {
      case x : TransformationException => {
        Logger.warn(s"""Error in standalone Transformation ${x.transformation.name}""")
        Logger.warn(s"Message: ${x.msg}")
        throw x
      }
    }
  }

  def findFirst[T <: AnyRef : ClassTag]() : Option[T] = findFirst(root)

  def findFirst[T <: AnyRef : ClassTag](node : Node) : Option[T] = {
    findFirst[T]({ x : Any => x match { case _ : T => true; case _ => false } }, node)
  }

  def findFirst[T <: AnyRef : ClassTag](check : T => Boolean, node : Node = root) : Option[T] = {
    var retVal : Option[T] = None
    var t = new Transformation("StatemanagerInternalFindFirst", {
      case hit : T if check(hit) =>
        retVal = Some(hit)
        new Output(hit)
    }) //, false) -> FIXME: Christian, currently it only works when enabling recursive matching

    progresses_.+=((t, new TransformationProgress))
    replace(node, t)

    retVal
  }

  protected object Vars {
    protected var cache = new HashMap[Class[_ <: AnyRef], List[(java.lang.reflect.Method, java.lang.reflect.Method)]]
    protected val setterSuffix = "_$eq"
    protected val excludeList = List()

    def apply[T](o : AnyRef) : List[(java.lang.reflect.Method, java.lang.reflect.Method)] = {
      cache.getOrElseUpdate(o.getClass(), {

        val methods = o.getClass.getMethods
        val vars : Array[(java.lang.reflect.Method, java.lang.reflect.Method)] = for {
          g <- methods; if (g.getModifiers & java.lang.reflect.Modifier.PUBLIC) == java.lang.reflect.Modifier.PUBLIC &&
            g.getParameterTypes.size == 0 && !excludeList.contains(g.getName)
          s <- methods; if s.getName.startsWith(g.getName) &&
            (s.getModifiers & java.lang.reflect.Modifier.PUBLIC) == java.lang.reflect.Modifier.PUBLIC &&
            s.getParameterTypes.size == 1 && s.getParameterTypes()(0) == g.getReturnType && s.getName == g.getName + setterSuffix
        } yield (g, s)

        Logger.info(s"""StateManager::Vars: Caching ${vars.length} members of class "${o.getClass.getName()}"""")
        vars.toList
      })
    }

    def get[T](o : AnyRef, method : java.lang.reflect.Method) : AnyRef = {
      method.invoke(o)
    }

    def set[T](o : AnyRef, method : java.lang.reflect.Method, value : AnyRef) : Boolean = {
      Logger.info(s"Statemananger::set: $o, " + method.getName() + s" to $value")
      method.invoke(o, value.asInstanceOf[AnyRef])
      true
    }
  }
}