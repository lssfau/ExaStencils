package exastencils.base.l4

import scala.collection.mutable._

import exastencils.base.ir._
import exastencils.core.collectors.Collector
import exastencils.datastructures._
import exastencils.prettyprinting._

/// L4_Function

object L4_Function {

  object Argument {
    // generate declaration corresponding to given access
    def apply(access : L4_VariableAccess) = new L4_Function.Argument(access.name, access.datatype)
  }

  case class Argument(var name : String, var datatype : L4_Datatype) extends L4_Node with PrettyPrintable with L4_Progressable {
    override def prettyprint(out : PpStream) = out << name << " : " << datatype
    override def progress = IR_FunctionArgument(name, datatype.progress)
  }

}

trait L4_Function extends L4_Statement {
  def name : String
  def datatype : L4_Datatype
  def parameters : ListBuffer[L4_Function.Argument]
  var body : ListBuffer[L4_Statement]

  override def progress : IR_Function
}

/// L4_PlainFunction

case class L4_PlainFunction(
    var name : String,
    var datatype : L4_Datatype,
    var parameters : ListBuffer[L4_Function.Argument],
    var body : ListBuffer[L4_Statement],
    var allowInlining : Boolean = true) extends L4_Function {

  override def prettyprint(out : PpStream) = {
    out << "Function " << name
    if (parameters.nonEmpty) out << " ( " <<< (parameters, ", ") << " )"
    if (datatype != L4_UnitDatatype) out << " : " << datatype
    out << " {\n" <<< (body, "\n") << "\n}"
  }

  override def progress = {
    val fct = IR_Function(datatype.progress, name, parameters.map(_.progress), body.map(_.progress))
    fct.allowInlining = allowInlining
    fct
  }
}

/// L4_LeveledFunction

case class L4_LeveledFunction(
    var name : String,
    var level : Int,
    var datatype : L4_Datatype,
    var parameters : ListBuffer[L4_Function.Argument],
    var body : ListBuffer[L4_Statement],
    var allowInlining : Boolean = true) extends L4_Function {

  override def prettyprint(out : PpStream) = {
    out << "Function " << name << '@' << level
    if (parameters.nonEmpty) out << " ( " <<< (parameters, ", ") << " )"
    if (datatype != L4_UnitDatatype) out << " : " << datatype
    out << " {\n" <<< (body, "\n") << "\n}"
  }

  override def progress = {
    val fct = IR_Function(datatype.progress, name + "_" + level, parameters.map(_.progress), body.map(_.progress))
    fct.allowInlining = allowInlining
    fct
  }
}

/// L4_FunctionCollector

object L4_FunctionCollector extends Collector {
  var plainFunctions = HashMap[String, L4_Datatype]()
  var leveledFunctions = HashMap[(String, Int), L4_Datatype]()

  override def enter(node : Node) : Unit = {
    node match {
      case fct : L4_PlainFunction   => plainFunctions += (fct.name -> fct.datatype)
      case fct : L4_LeveledFunction => leveledFunctions += ((fct.name, fct.level) -> fct.datatype)

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
