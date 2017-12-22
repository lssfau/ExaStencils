package exastencils.base.l3

import scala.collection.mutable._

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.core.collectors.Collector
import exastencils.datastructures._
import exastencils.prettyprinting._

/// L3_Function

object L3_Function {

  object Argument {
    // generate declaration corresponding to given access
    def apply(access : L3_VariableAccess) = new Argument(access.name, access.datatype)
  }

  case class Argument(var name : String, var datatype : L3_Datatype) extends L3_Node with PrettyPrintable with L3_Progressable {
    override def prettyprint(out : PpStream) = out << name << " : " << datatype
    override def progress = ProgressLocation(L4_Function.Argument(name, datatype.progress))
  }

}

trait L3_Function extends L3_Statement {
  def name : String
  def datatype : L3_Datatype
}

/// L3_PlainFunction

case class L3_PlainFunction(
    var name : String,
    var datatype : L3_Datatype,
    var parameters : ListBuffer[L3_Function.Argument],
    var body : ListBuffer[L3_Statement]) extends L3_Function {

  override def prettyprint(out : PpStream) = {
    out << "Function " << name
    if (parameters.nonEmpty) out << " ( " <<< (parameters, ", ") << " )"
    if (datatype != L3_UnitDatatype) out << " : " << datatype
    out << " {\n" <<< (body, "\n") << "\n}"
  }

  override def progress = ProgressLocation(L4_PlainFunction(name, datatype.progress, parameters.map(_.progress), body.map(_.progress)))
}

/// L3_LeveledFunction

case class L3_LeveledFunction(
    var name : String,
    var level : Int,
    var datatype : L3_Datatype,
    var parameters : ListBuffer[L3_Function.Argument],
    var body : ListBuffer[L3_Statement]) extends L3_Function {

  override def prettyprint(out : PpStream) = {
    out << "Function " << name << '@' << level
    if (parameters.nonEmpty) out << " ( " <<< (parameters, ", ") << " )"
    if (datatype != L3_UnitDatatype) out << " : " << datatype
    out << " {\n" <<< (body, "\n") << "\n}"
  }

  override def progress = ProgressLocation(L4_LeveledFunction(name, level, datatype.progress, parameters.map(_.progress), body.map(_.progress)))
}

/// L3_FunctionCollector

object L3_FunctionCollector extends Collector {
  var plainFunctions = HashMap[String, L3_Datatype]()
  var leveledFunctions = HashMap[(String, Int), L3_Datatype]()

  override def enter(node : Node) : Unit = {
    node match {
      case fct : L3_PlainFunction   => plainFunctions += (fct.name -> fct.datatype)
      case fct : L3_LeveledFunction => leveledFunctions += ((fct.name, fct.level) -> fct.datatype)

      case _ =>
    }
  }

  override def leave(node : Node) : Unit = {
    // no need to remove information from collections
  }

  override def reset() : Unit = {
    plainFunctions.clear()
    leveledFunctions.clear()
  }

  def exists(name : String) = plainFunctions.contains(name) || leveledFunctions.keys.exists(_._1 == name)
  def existsPlain(name : String) = plainFunctions.contains(name)
  def existsLeveled(name : String, level : Int) = leveledFunctions.contains((name, level))

  def getDatatype(name : String) = plainFunctions(name)
  def getDatatype(name : String, level : Int) = leveledFunctions((name, level))
}
