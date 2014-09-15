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

  // ###############################################################################################
  // #### Checkpointing ############################################################################
  // ###############################################################################################

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

  // ###############################################################################################
  // #### Collectors ###############################################################################
  // ###############################################################################################

  protected object Collectors {
    protected var collectors_ = new ListBuffer[Collector]

    def notifyEnter(node : Node) = { collectors_.foreach(c => c.enter(node)) }
    def notifyLeave(node : Node) = { collectors_.foreach(c => c.leave(node)) }

    def register(c : Collector) = { collectors_ += c }
    def unregister(c : Collector) = { collectors_ -= c }
    def unregisterAll() = { collectors_.clear }
  }

  /**
    * Register a Collector with StateManager
    *
    *  @param c The Collector to be added
    */
  def register(c : Collector) = { Collectors.register(c) }

  /**
    * Unregister a Collector from StateManager
    *
    *  @param c The Collector be removed
    */
  def unregister(c : Collector) = { Collectors.unregister(c) }

  /** Unregister all currently registered Collectors from StateManager */
  def unregisterAll() = { Collectors.unregisterAll }

  // ###############################################################################################
  // #### Transformationen & Matching ##############################################################
  // ###############################################################################################

  /** Class that holds statistics about a Transformation */
  protected class TransformationProgress {
    /** Number of times a Transformation could be matched and applied */
    protected var matches = 0

    /**
      * Returns the number of matches
      *
      *  @return The number of matches
      */
    def getMatches = matches

    /** Increases the number of matches by 1 */
    def didMatch = matches += 1

    override def toString = { s"Transformation Progress: $matches match(es)" }
  }
  protected val progresses_ = new HashMap[Transformation, TransformationProgress]

  /** Dummy node that is used internally to signal that a Transformation did not match a given node */
  protected case object NoMatch extends Node

  /**
    * Function that is called by applyAtNode in case there was no match
    *
    *  @param node Not used. Its existence, however, is enforced by Scala.
    *  @return An Output instance carrying the dummy node
    */
  protected def NoMatchFunction(node : Node) : Transformation.OutputType = {
    return NoMatch // return dummy node
  }

  /**
    * Apply the transformation function to a given node
    *
    *  @param node The node the Transformation is to be applied to
    *  @param transformation The Transformation to be applied
    *  @return An Output instance carrying the result of the transformation or NoMatch is the transformation could not be applied
    */
  protected def applyAtNode(node : Node, transformation : Transformation) : Transformation.OutputType = {
    val output = transformation.function.applyOrElse(node, NoMatchFunction) // use applyOrElse because Scala's documentation claims it is implement more efficiently
    output.inner match {
      case NoMatch =>
      case _       => progresses_(transformation).didMatch
    }
    output
  }

  /**
    * The main Transformation & replacement function
    *
    *  @param node The node the Transformation is to be applied to
    *  @param transformation The Transformation to be applied
    */
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
          val ret = applyAtNode(n, transformation).inner
          var nextNode = ret

          ret match {
            case NoMatch => nextNode = n // do nothing, but set next node for recursive matching
            case m : Node => {
              if (!Vars.set(node, setter, m)) {
                Logger.error(s"""Could not set "$getter" in transformation ${transformation.name}""")
              }
            }
            case m : NodeList if m.nodes.size == 1 => { // Only valid if list contains a single element
              if (!Vars.set(node, setter, m.nodes.toSeq(0))) {
                Logger.error(s"""Could not set "$getter" in transformation ${transformation.name}""")
              }
            }
            case None         => Logger.error("not possible")
          }

          // Apply transformation to sub-elements
          if (transformation.recursive || (!transformation.recursive && previousMatches >= progresses_(transformation).getMatches)) {
            replace(nextNode.asInstanceOf[Node], transformation) // can safely downcast because of type matching above erroring out for unfitting types
          }
        }
        case Some(any) => any match {
          case n : Node => {
            val ret = applyAtNode(n, transformation).inner
            var nextNode = ret

            ret match {
              case NoMatch => nextNode = n // do nothing, but set next node for recursive matching
              case m : Node => {
                if (!Vars.set(node, setter, Some(m))) {
                  Logger.error(s"""Could not set "$getter" in transformation ${transformation.name}""")
                }
              }
              case m : NodeList if m.nodes.size == 1 => { // Only valid if list contains a single element
                if (!Vars.set(node, setter, Some(m.nodes.toSeq(0)))) {
                  Logger.error(s"""Could not set "$getter" in transformation ${transformation.name}""")
                }
              }
              case None         => Logger.error("not possible")
            }

            // Apply transformation to sub-elements
            if (transformation.recursive || (!transformation.recursive && previousMatches >= progresses_(transformation).getMatches)) {
              replace(nextNode.asInstanceOf[Node], transformation) // can safely downcast because of type matching above erroring out for unfitting types
            }
          }
          case _ => // "any" is not of type Node, thus not interesting to us
        }
        // ###############################################################################################
        // #### Sets #####################################################################################
        // ###############################################################################################
        case set : scala.collection.mutable.Set[_] => {
          var newSet = set.flatMap(f => f match {
            case n : Node => applyAtNode(n, transformation).inner match {
              case NoMatch =>
                replace(n, transformation); List(n) // no match occured => use old element 
              case newN : Node => {
                if (transformation.recursive || (!transformation.recursive && previousMatches >= progresses_(transformation).getMatches)) {
                  replace(newN, transformation) // Recursive call for new element
                }
                List(newN) // element of type Node was returned => use it
              }
              case newN : NodeList => {
                if (transformation.recursive || (!transformation.recursive && previousMatches >= progresses_(transformation).getMatches)) {
                  newN.nodes.foreach(replace(_, transformation)) // recursive call for new elements
                }
                newN.nodes // elements of type Node were returned => use them
              }
            }
            case _ => List(f) // current element "f" is not of interest to us - put it back into (new) set
          })

          if (previousMatches <= progresses_(transformation).getMatches && !Vars.set(node, setter, newSet)) {
            Logger.error(s"Could not set $getter in transformation ${transformation.name}")
          }
        }
        case set : scala.collection.immutable.Set[_] => {
          var newSet = set.flatMap(f => f match {
            case n : Node => applyAtNode(n, transformation).inner match {
              case NoMatch =>
                replace(n, transformation); List(n) // no match occured => use old element 
              case newN : Node => {
                if (transformation.recursive || (!transformation.recursive && previousMatches >= progresses_(transformation).getMatches)) {
                  replace(newN, transformation) // Recursive call for new element
                }
                List(newN) // element of type Node was returned => use it
              }
              case newN : NodeList => {
                if (transformation.recursive || (!transformation.recursive && previousMatches >= progresses_(transformation).getMatches)) {
                  newN.nodes.foreach(replace(_, transformation)) // recursive call for new elements
                }
                newN.nodes // elements of type Node were returned => use them
              }
            }
            case _ => List(f)
          })

          if (previousMatches <= progresses_(transformation).getMatches && !Vars.set(node, setter, newSet)) {
            Logger.error(s"Could not set $getter in transformation ${transformation.name}")
          }
        }
        // ###############################################################################################
        // #### Lists, ListBuffers #######################################################################
        // ###############################################################################################
        case seq : Seq[_] => {
          var newSeq = seq.flatMap(f => f match {
            case n : Node => applyAtNode(n, transformation).inner match {
              case NoMatch =>
                replace(n, transformation); List(n) // no match occured => use old element 
              case newN : Node => {
                if (transformation.recursive || (!transformation.recursive && previousMatches >= progresses_(transformation).getMatches)) {
                  replace(newN, transformation) // Recursive call for new element
                }
                List(newN) // element of type Node was returned => use it
              }
              case newN : NodeList => {
                if (transformation.recursive || (!transformation.recursive && previousMatches >= progresses_(transformation).getMatches)) {
                  newN.nodes.foreach(replace(_, transformation)) // recursive call for new elements
                }
                newN.nodes // elements of type Node were returned => use them
              }
            }
            case _ => List(f)
          })

          if (previousMatches <= progresses_(transformation).getMatches && !Vars.set(node, setter, newSeq)) {
            Logger.error(s"Could not set $getter in transformation ${transformation.name}")
          }
        }
        // ###############################################################################################
        // #### Maps #####################################################################################
        // ###############################################################################################
        case map : scala.collection.mutable.Map[_, _] => {
          var newMap = map.map(f => f match {
            case n : Node => applyAtNode(n, transformation).inner match {
              case NoMatch =>
                replace(n, transformation); n // no match occured => use old element 
              case newN : Node => {
                if (transformation.recursive || (!transformation.recursive && previousMatches >= progresses_(transformation).getMatches)) {
                  replace(newN, transformation) // Recursive call for new element
                }
                newN // element of type Node was returned => use it
              }
              case newN : NodeList => Logger.error("fixme")
            }
          })

          if (previousMatches <= progresses_(transformation).getMatches && !Vars.set(node, setter, newMap)) {
            Logger.error(s"Could not set $getter in transformation ${transformation.name}")
          }
        }
        // Unfortunately mutable and immutable set have no common supertype
        case map : scala.collection.immutable.Map[_, _] => {
          var newMap = map.map(f => f match {
            case n : Node => applyAtNode(n, transformation).inner match {
              case NoMatch =>
                replace(n, transformation); n // no match occured => use old element 
              case newN : Node => {
                if (transformation.recursive || (!transformation.recursive && previousMatches >= progresses_(transformation).getMatches)) {
                  replace(newN, transformation) // Recursive call for new element
                }
                newN // element of type Node was returned => use it
              }
              case newN : NodeList => Logger.error("fixme")
            }
          })

          if (previousMatches <= progresses_(transformation).getMatches && !Vars.set(node, setter, newMap)) {
            Logger.error(s"Could not set $getter in transformation ${transformation.name}")
          }
        }
        case list : Array[_] => {
          Logger.warn("Arrays are currently not supported for matching!")
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
        case _ => // 
      }
    })
    Collectors.notifyLeave(node)
  }

  /**
    * Apply a Transformation to the current program state
    *
    *  @param token A TransactionToken to lock the program state for other Transformations
    *  @param transformation The Transformation to be applied
    *  @param node An optional node that is treated as the starting point ("root") for the Transaction
    *  @return Statistics about matches
    */
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

  /**
    * Apply a Transformation to the current program state without supplying a TransactionToken
    *
    *  Warning: This is dangerous and not encouraged!
    *
    *  @param transformation The Transformation to be applied
    *  @param node An optional node that is treated as the starting point ("root") for the Transaction
    *  @return Statistics about matches
    */
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