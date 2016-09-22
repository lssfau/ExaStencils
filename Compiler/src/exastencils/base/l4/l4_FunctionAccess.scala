package exastencils.base.l4

import scala.collection.mutable.HashMap

import exastencils.base.ir._
import exastencils.core.collectors.Collector
import exastencils.datastructures._
import exastencils.datastructures.l4.UnresolvedAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_FunctionAccess

trait L4_FunctionAccess extends L4_Access {
  def name : String
  def level : Option[Int]
  def datatype : L4_Datatype

  override def prettyprint(out : PpStream) = {
    out << name
    if (level.isDefined) out << "@" << level.get
  }

  override def progress : IR_FunctionAccess = {
    val resolvedName =
      if (level.isDefined)
        L4_LeveledIdentifier(name, L4_SingleLevel(level.get)).fullName
      else
        L4_BasicIdentifier(name).fullName

    IR_FunctionAccess(resolvedName, datatype.progress)
  }
}

/// L4_UserFunctionAccess

object L4_UserFunctionAccess {
  def apply(name : String, datatype : L4_Datatype) =
    new L4_UserFunctionAccess(name, None, datatype)
  def apply(name : String, level : Int, datatype : L4_Datatype) =
    new L4_UserFunctionAccess(name, Some(level), datatype)
}

case class L4_UserFunctionAccess(var name : String, level : Option[Int], var datatype : L4_Datatype) extends L4_FunctionAccess

/// L4_FunctionDeclCollector

object L4_FunctionDeclCollector extends Collector {
  private var functions = HashMap[String, L4_Datatype]()

  override def enter(node : Node) : Unit = {
    node match {
      case fct @ L4_Function(L4_BasicIdentifier(fctName), returntype, _, _, _)                          =>
        functions += ((fctName, returntype))
      case fct @ L4_Function(L4_LeveledIdentifier(fctName, L4_SingleLevel(level)), returntype, _, _, _) =>
        functions += ((fctName + "@@" + level, returntype))
      case fct : L4_Function                                                                            =>
        Logger.warn("Encountered l4 function with unsupported identifier " + fct.identifier)
      case _                                                                                            =>
    }
  }

  override def leave(node : Node) : Unit = {
    // no need to remove information from collections
  }

  override def reset() : Unit = {
    functions.clear()
  }

  def getValue(name : String) : Option[L4_Datatype] = functions.get(name)
  def exists(name : String) = getValue(name).isDefined
}

/// L4_ResolveFunctionAccesses

object L4_ResolveFunctionAccesses extends DefaultStrategy("Resolve function accesses") {
  val collector = L4_FunctionDeclCollector
  this.register(collector)

  this += new Transformation("Collecting function declarations", PartialFunction.empty)

  this += new Transformation("Resolve function accesses", {
    case access @ UnresolvedAccess(accessName, _, None, _, _, _) if collector.exists(accessName)                                       =>
      L4_UserFunctionAccess(accessName, collector.getValue(accessName).get)
    case access @ UnresolvedAccess(accessName, _, Some(L4_SingleLevel(level)), _, _, _) if collector.exists(accessName + "@@" + level) =>
      L4_UserFunctionAccess(accessName, level, collector.getValue(accessName + "@@" + level).get)
  })
}
