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

package exastencils.base.ir

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.prettyprinting._

/// IR_FunctionArgument

object IR_FunctionArgument {
  // generate declaration corresponding to given access
  def apply(access : IR_VariableAccess) = new IR_FunctionArgument(access.name, access.datatype)
}

case class IR_FunctionArgument(var name : String, var datatype : IR_Datatype) extends IR_Node with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << datatype << ' ' << name

  def access = IR_VariableAccess(name, Duplicate(datatype))
}

trait IR_Function extends IR_FunctionLike {

  // TODO: move to IR_FunctionLike ? 
  var datatype : IR_Datatype
  var parameters : ListBuffer[IR_FunctionArgument]
  var body : ListBuffer[IR_Statement]

  override def prettyprint(out : PpStream) : Unit = {
    if (!functionQualifiers.isEmpty) out << functionQualifiers << ' '
    out << datatype << ' ' << name << ' ' << '(' <<< (parameters, ", ") << ") {\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }

  override def prettyprint_decl() : String = {
    var decl = ""
    if (!functionQualifiers.isEmpty) decl += functionQualifiers + ' '
    decl += datatype.prettyprint + ' ' + name + '(' + parameters.map(_.prettyprint()).mkString(", ") + ");\n"
    decl
  }

  def withNoInline() : IR_Function = {
    allowInlining = false
    this
  }
}

/// IR_PlainFunction

object IR_PlainFunction {
  // empty function
  def apply(name : String, datatype : IR_Datatype) =
    new IR_PlainFunction(name, datatype, ListBuffer(), ListBuffer())

  // no function arguments
  def apply(name : String, datatype : IR_Datatype, body : ListBuffer[IR_Statement]) =
    new IR_PlainFunction(name, datatype, ListBuffer(), body)

  // single statement body
  def apply(name : String, datatype : IR_Datatype, arguments : ListBuffer[IR_FunctionArgument], body : IR_Statement) =
    new IR_PlainFunction(name, datatype, arguments, ListBuffer(body))

  // only one function argument
  def apply(name : String, datatype : IR_Datatype, arguments : IR_FunctionArgument, body : ListBuffer[IR_Statement]) =
    new IR_PlainFunction(name, datatype, ListBuffer(arguments), body)

  // only one function argument and single statement body
  def apply(name : String, datatype : IR_Datatype, arguments : IR_FunctionArgument, body : IR_Statement) =
    new IR_PlainFunction(name, datatype, ListBuffer(arguments), ListBuffer(body))
}

case class IR_PlainFunction(
    var name : String,
    override var datatype : IR_Datatype,
    override var parameters : ListBuffer[IR_FunctionArgument],
    override var body : ListBuffer[IR_Statement]
) extends IR_PlainFunctionLike {

}

/// IR_LeveledFunction

object IR_LeveledFunction {
  // empty function
  def apply(name : String, level : Int, datatype : IR_Datatype) =
    new IR_LeveledFunction(name, level, datatype, ListBuffer(), ListBuffer())

  // no function arguments
  def apply(name : String, level : Int, datatype : IR_Datatype, body : ListBuffer[IR_Statement]) =
    new IR_LeveledFunction(name, level, datatype, ListBuffer(), body)

  // single statement body
  def apply(name : String, level : Int, datatype : IR_Datatype, arguments : ListBuffer[IR_FunctionArgument], body : IR_Statement) =
    new IR_LeveledFunction(name, level, datatype, arguments, ListBuffer(body))

  // only one function argument
  def apply(name : String, level : Int, datatype : IR_Datatype, arguments : IR_FunctionArgument, body : ListBuffer[IR_Statement]) =
    new IR_LeveledFunction(name, level, datatype, ListBuffer(arguments), body)

  // only one function argument and single statement body
  def apply(name : String, level : Int, datatype : IR_Datatype, arguments : IR_FunctionArgument, body : IR_Statement) =
    new IR_LeveledFunction(name, level, datatype, ListBuffer(arguments), ListBuffer(body))
}

case class IR_LeveledFunction(
    var baseName : String,
    var level : Int,
    override var datatype : IR_Datatype,
    override var parameters : ListBuffer[IR_FunctionArgument],
    override var body : ListBuffer[IR_Statement]
) extends IR_LeveledFunctionLike {

  override var name = baseName + '_' + level
}
