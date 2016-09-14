package exastencils.core.collectors

import scala.collection.mutable.{ Node => _, _ }

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.datastructures.l4._

class L4ValueCollector extends Collector {
  private var values = new ListBuffer[HashMap[String, L4_Expression]]()
  private var insideGlobals = false

  override def enter(node : Node) : Unit = {
    node match {
      case x : GlobalDeclarationStatement => insideGlobals = true
      case x : FunctionStatement          => { values.clear(); values.+=((new HashMap[String, L4_Expression]())) }
      case x : LoopOverFragmentsStatement => values.+=((new HashMap[String, L4_Expression]()))
      case x : LoopOverPointsStatement    => values.+=((new HashMap[String, L4_Expression]()))
      case x : RepeatTimesStatement       => values.+=((new HashMap[String, L4_Expression]()))
      case x : L4_UntilLoop               => values.+=((new HashMap[String, L4_Expression]()))
      case x : L4_IfCondition             => values.+=((new HashMap[String, L4_Expression]()))
      case x : ValueDeclarationStatement  => {
        x.identifier match { // ignore Values in Globals
          case v : LeveledIdentifier => if (!insideGlobals) values.last += ((v.name + "@@" + v.level, x.expression))
          case _                     => if (!insideGlobals) values.last += ((x.identifier.name, x.expression))
        }
      }
      case _                              =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case x : GlobalDeclarationStatement => insideGlobals = false
      case x : FunctionStatement          => values.clear()
      case x : LoopOverFragmentsStatement => values.trimEnd(1)
      case x : LoopOverPointsStatement    => values.trimEnd(1)
      case x : RepeatTimesStatement       => values.trimEnd(1)
      case x : L4_UntilLoop               => values.trimEnd(1)
      case x : L4_IfCondition             => values.trimEnd(1)
      case _                              =>
    }
  }

  override def reset() : Unit = {
    values.clear()
  }

  def getValue(name : String) : Option[L4_Expression] = {
    var exp : Option[L4_Expression] = None
    var i = values.length - 1
    while (i >= 0 && exp.isEmpty) {
      exp = values(i).get(name) // Local Vals will shadow global Vals
      i = i - 1
    }
    exp
  }

  override def toString = "[L4ValueCollector]: " + values.toString()
}
