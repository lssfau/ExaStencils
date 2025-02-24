//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.base.l4

import scala.collection.mutable._

import exastencils.base.ProgressLocation
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
    override def progress = ProgressLocation(IR_FunctionArgument(name, datatype.progress))
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

  override def progress = ProgressLocation {
    val fct = IR_PlainFunction(name, datatype.progress, parameters.map(_.progress), body.map(_.progress))
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

  override def progress = ProgressLocation {
    val fct = IR_LeveledFunction(name, level, datatype.progress, parameters.map(_.progress), body.map(_.progress))
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
      case fct : L4_PlainFunction                          => plainFunctions += (fct.name -> fct.datatype)
      case fct : L4_LeveledFunction                        => leveledFunctions += ((fct.name, fct.level) -> fct.datatype)
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
