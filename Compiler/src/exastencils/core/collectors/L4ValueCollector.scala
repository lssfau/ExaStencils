package exastencils.core.collectors

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import scala.collection.mutable.HashMap

class L4ValueCollector extends Collector {
  //  private var curLevel = -1
  private var global : GlobalDeclarationStatement = null
  private var values = new HashMap[String, Expression]()

  override def enter(node : Node) : Unit = {
    node match {
      case x : GlobalDeclarationStatement => global = x
      case x : FunctionStatement          => values.clear()
      case x : ValueDeclarationStatement  => values.+=((x.identifier.name, x.expression))
      //      case FunctionStatement(LeveledIdentifier(_, SingleLevelSpecification(level)), _, _, _) => curLevel = level
      case _                              =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case x : FunctionStatement => values.clear()
      //      case FunctionStatement(LeveledIdentifier(_, SingleLevelSpecification(level)), _, _, _) => // due to duplication of functions, functions can be left that were never entered
      case _                     =>
    }
  }

  override def reset() : Unit = {
    //    curLevel = -1
    values.clear()
  }

  def getValue(name : String) : Option[Expression] = {
    var decl : Option[ValueDeclarationStatement] = None
    if (global != null) {
      decl = global.values.find(_.identifier.name == name)
    }
    if (decl.isDefined) {
      Some(decl.get.expression)
    } else {
      values.get(name)
    }
  }

  //  def getCurrentLevel : Int = {
  //    if (curLevel < 0)
  //      Logger.dbg("Trying to access level outside of a valid scope")
  //    curLevel
  //  }
}
