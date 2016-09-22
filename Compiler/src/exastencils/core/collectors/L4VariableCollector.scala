package exastencils.core.collectors

import scala.collection.mutable.{ Node => _, _ }

import exastencils.base.l4._
import exastencils.baseExt.l4._
import exastencils.datastructures._

class L4VariableCollector extends Collector {
  private var values = new ListBuffer[HashMap[String, L4_Datatype]]()
  this.reset()

  override def enter(node : Node) : Unit = {
    node match {
      case x : L4_Function            => values.+=((new HashMap[String, L4_Datatype]()))
      case x : L4_LoopOverFragments   => values.+=((new HashMap[String, L4_Datatype]()))
      case x : L4_LoopOverField       => values.+=((new HashMap[String, L4_Datatype]()))
      case x : L4_ForLoop             => values.+=((new HashMap[String, L4_Datatype]()))
      case x : L4_UntilLoop           => values.+=((new HashMap[String, L4_Datatype]()))
      case x : L4_IfCondition         => values.+=((new HashMap[String, L4_Datatype]()))
      case x : L4_VariableDeclaration => {
        x.identifier match { // ignore Values in Globals
          case v : L4_LeveledIdentifier => values.last += ((v.name + "@@" + v.level, x.datatype))
          case _                        => values.last += ((x.identifier.name, x.datatype))
        }
      }
      case _                          =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case x : L4_Function          => values.trimEnd(1)
      case x : L4_LoopOverFragments => values.trimEnd(1)
      case x : L4_LoopOverField     => values.trimEnd(1)
      case x : L4_ForLoop           => values.trimEnd(1)
      case x : L4_UntilLoop         => values.trimEnd(1)
      case x : L4_IfCondition       => values.trimEnd(1)
      case _                        =>
    }
  }

  override def reset() : Unit = {
    values.clear()
    // get globals
    values.+=((new HashMap[String, L4_Datatype]()))
    exastencils.core.StateManager.findAll[L4_GlobalSection]().foreach(_.variableDeclarations.foreach(v => values.head.+=((v.identifier match {
      case vv : L4_LeveledIdentifier => vv.name + "@@" + vv.level;
      case _                         => v.identifier.name
    }, v.datatype))))
  }

  def getValue(name : String) : Option[L4_Datatype] = {
    var dt : Option[L4_Datatype] = None
    var i = values.length - 1
    while (i >= 0 && dt.isEmpty) {
      dt = values(i).get(name) // Local Vars will shadow global Vars
      i = i - 1
    }
    dt
  }

  def exists(name : String) = getValue(name).isDefined

  override def toString = "[L4VariableCollector]: " + values.toString()
}
