package exastencils.core.collectors

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import scala.collection.mutable.HashMap

class L4ValueCollector extends Collector {
  private var values = new HashMap[String, Expression]()
  private var globalVals = new HashMap[String, Expression]()

  override def enter(node : Node) : Unit = {
    node match {
      case x : GlobalDeclarationStatement => {
        x.values.foreach(v => v.identifier match {
          case vl : LeveledIdentifier => globalVals += ((v.identifier.name + "_" + vl.level, v.expression))
          case _                      => globalVals += ((v.identifier.name, v.expression))
        })
      }
      case x : FunctionStatement => values.clear()
      case x : ValueDeclarationStatement => {
        x.identifier match {
          case v : LeveledIdentifier => values += ((v.name + "_" + v.level, x.expression))
          case _                     => values += ((x.identifier.name, x.expression))
        }
      }
      case _ =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case x : FunctionStatement => values.clear()
      case _                     =>
    }
  }

  override def reset() : Unit = {
    values.clear()
  }

  def getValue(name : String) : Option[Expression] = {
    var exp : Option[Expression] = None

    exp = values.get(name) // Local Vals will shadow global Vals
    if (!exp.isDefined) {
      exp = globalVals.get(name)
    }
    exp
  }

  override def toString = "[L4ValueCollector]: " + values.toString()
}
