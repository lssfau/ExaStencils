package exastencils.core.collectors

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

class L4ValueCollector extends Collector {
  private var values = new ListBuffer[HashMap[String, Expression]]()
  private var globalVals = new HashMap[String, Expression]()
  private var insideGlobals = false

  override def enter(node : Node) : Unit = {
    println("enter " + node)
    node match {
      case x : GlobalDeclarationStatement => insideGlobals = true
      case x : FunctionStatement          => values.clear()
      case x : LoopOverFragmentsStatement => values.+=((new HashMap[String, Expression]()))
      case x : LoopOverPointsStatement    => values.+=((new HashMap[String, Expression]()))
      case x : RepeatTimesStatement       => values.+=((new HashMap[String, Expression]()))
      case x : RepeatUntilStatement       => values.+=((new HashMap[String, Expression]()))
      case x : ConditionalStatement       => values.+=((new HashMap[String, Expression]()))
      case x : ValueDeclarationStatement => {
        x.identifier match {
          case v : LeveledIdentifier => if (insideGlobals) globalVals += ((v.name + "_" + v.level, x.expression)); else values.last += ((v.name + "_" + v.level, x.expression))
          case _                     => if (insideGlobals) globalVals += ((x.identifier.name, x.expression)); else values.last += ((x.identifier.name, x.expression))
        }
      }
      case _ =>
    }
  }

  override def leave(node : Node) : Unit = {
    println("leave " + node)
    node match {
      case x : GlobalDeclarationStatement => insideGlobals = false
      case x : FunctionStatement          => values.clear()
      case x : LoopOverFragmentsStatement => values.trimEnd(1)
      case x : LoopOverPointsStatement    => values.trimEnd(1)
      case x : RepeatTimesStatement       => values.trimEnd(1)
      case x : RepeatUntilStatement       => values.trimEnd(1)
      case x : ConditionalStatement       => values.trimEnd(1)
      case _                              =>
    }
  }

  override def reset() : Unit = {
    values.clear()
  }

  def getValue(name : String) : Option[Expression] = {
    var exp : Option[Expression] = None
    var i = values.length - 1
    while (i >= 0 && exp.isEmpty) {
      exp = values(i).get(name) // Local Vals will shadow global Vals
      i = i - 1
    }
    if (exp.isEmpty) {
      exp = globalVals.get(name)
    }
    exp
  }

  override def toString = "[L4ValueCollector]: " + values.toString()
}
