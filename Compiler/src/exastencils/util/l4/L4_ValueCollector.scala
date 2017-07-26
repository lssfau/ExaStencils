package exastencils.util.l4

import scala.collection.mutable._

import exastencils.base.l4._
import exastencils.baseExt.l4._
import exastencils.core.collectors.Collector
import exastencils.datastructures._

class L4_ValueCollector extends Collector {
  private var values = new ListBuffer[HashMap[String, L4_Expression]]()
  private var insideGlobals = false

  override def enter(node : Node) : Unit = {
    node match {
      case _ : L4_GlobalSection     => insideGlobals = true
      case _ : L4_Function          => values.clear(); values.+=(new HashMap[String, L4_Expression]())
      case _ : L4_LoopOverFragments => values.+=(new HashMap[String, L4_Expression]())
      case _ : L4_LoopOverField     => values.+=(new HashMap[String, L4_Expression]())
      case _ : L4_ForLoop           => values.+=(new HashMap[String, L4_Expression]())
      case _ : L4_UntilLoop         => values.+=(new HashMap[String, L4_Expression]())
      case _ : L4_IfCondition       => values.+=(new HashMap[String, L4_Expression]())
      case x : L4_ValueDeclaration  =>
        x.identifier match { // ignore Values in Globals
          case v : L4_LeveledIdentifier => if (!insideGlobals) values.last += ((v.name + "@@" + v.level, x.initialValue))
          case _                        => if (!insideGlobals) values.last += ((x.identifier.name, x.initialValue))
        }
      case _                        =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case _ : L4_GlobalSection     => insideGlobals = false
      case _ : L4_Function          => values.clear()
      case _ : L4_LoopOverFragments => values.trimEnd(1)
      case _ : L4_LoopOverField     => values.trimEnd(1)
      case _ : L4_ForLoop           => values.trimEnd(1)
      case _ : L4_UntilLoop         => values.trimEnd(1)
      case _ : L4_IfCondition       => values.trimEnd(1)
      case _                        =>
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
