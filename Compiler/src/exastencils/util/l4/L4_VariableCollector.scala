package exastencils.util.l4

import scala.collection.mutable._

import exastencils.base.l4._
import exastencils.baseExt.l4._
import exastencils.core.collectors.Collector
import exastencils.datastructures._

class L4_VariableCollector extends Collector {
  private var values = new ListBuffer[HashMap[String, L4_Datatype]]()
  this.reset()

  override def enter(node : Node) : Unit = {
    node match {
      case _ : L4_Function            => values.+=(new HashMap[String, L4_Datatype]())
      case _ : L4_LoopOverFragments   => values.+=(new HashMap[String, L4_Datatype]())
      case _ : L4_LoopOverField       => values.+=(new HashMap[String, L4_Datatype]())
      case _ : L4_ForLoop             => values.+=(new HashMap[String, L4_Datatype]())
      case _ : L4_UntilLoop           => values.+=(new HashMap[String, L4_Datatype]())
      case _ : L4_IfCondition         => values.+=(new HashMap[String, L4_Datatype]())
      case x : L4_VariableDeclaration =>
        x.identifier match { // ignore Values in Globals
          case v : L4_LeveledIdentifier => values.last += ((v.name + "@@" + v.level, x.datatype))
          case _                        => values.last += ((x.identifier.name, x.datatype))
        }
      case x : L4_Function.Argument   => values.last += ((x.name, x.datatype))
      case _                          =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case _ : L4_Function          => values.trimEnd(1)
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
    // get globals
    values.+=(new HashMap[String, L4_Datatype]())
    exastencils.core.StateManager.findAll[L4_GlobalSection]().foreach(_.variableDeclarations.foreach(v => values.head.+=((v.identifier match {
      case vv : L4_LeveledIdentifier => vv.name + "@@" + vv.level;
      case _                         => v.identifier.name
    }, v.datatype))))
  }

  def getValue(name : String) : Option[L4_Datatype] = {
    val it = values.reverseIterator

    while (it.hasNext) {
      val dt = it.next.get(name)
      if (dt.isDefined) return dt
    }

    None
  }

  def exists(name : String) = getValue(name).isDefined

  override def toString = "[L4VariableCollector]: " + values.toString()
}
