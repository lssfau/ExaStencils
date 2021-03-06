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

package exastencils.domain.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.util.ir.IR_UtilFunctions

object IR_ReadLineFromFile {
  var name = "readLine"

  def arg1 = IR_VariableAccess("ifs", IR_SpecialDatatype("std::ifstream&"))
  def arg2 = IR_VariableAccess("iss", IR_SpecialDatatype("std::istringstream&"))

  def addToUtil = {
    IR_GlobalCollection.get.internalDependencies += IR_UtilFunctions.defHeader
    IR_GlobalCollection.get.internalDependencies = IR_GlobalCollection.get.internalDependencies.distinct
    IR_UtilFunctions.get.externalDependencies ++= ListBuffer("fstream", "sstream", "algorithm")
    IR_UtilFunctions.get.externalDependencies = IR_UtilFunctions.get.externalDependencies.distinct
    if (!IR_UtilFunctions.get.functions.exists(_.name == name)) {
      IR_UtilFunctions.get.functions += IR_ReadLineFromFile(arg1, arg2)
    }
  }
}

case class IR_ReadLineFromFile(ifs : IR_VariableAccess, iss : IR_VariableAccess) extends IR_FuturePlainFunction {
  override var name = IR_ReadLineFromFile.name
  override def prettyprint_decl() = prettyprint

  def delete_comments(str : IR_VariableAccess) = {
    var body = ListBuffer[IR_Statement]()
    val commentSign : IR_StringConstant = IR_StringConstant("#")

    body += IR_Comment("Delete comments and spaces at beginning and end of string")
    // delete comment
    body += IR_IfCondition(IR_Neq(IR_MemberFunctionCall(str, "find", commentSign), "std::string::npos"),
      IR_MemberFunctionCall(str, "erase", IR_MemberFunctionCall(str, "find", commentSign)))

    // delete spaces at the beginning and end of string
    body += IR_MemberFunctionCall(str, "erase",
      IR_MemberFunctionCall(str, "begin"),
      IR_FunctionCall("std::find_if", IR_MemberFunctionCall(str, "begin"), IR_MemberFunctionCall(str, "end"),
        IR_FunctionCall("std::not1", IR_FunctionCall("std::ptr_fun<int,int>", "std::isspace"))))

    body += IR_MemberFunctionCall(str, "erase",
      IR_MemberFunctionCall(IR_FunctionCall("std::find_if", IR_MemberFunctionCall(str, "rbegin"), IR_MemberFunctionCall(str, "rend"),
        IR_FunctionCall("std::not1", IR_FunctionCall("std::ptr_fun<int,int>", "std::isspace"))), "base"),
      IR_MemberFunctionCall(str, "end"))

  }

  def get_line(ifs : IR_VariableAccess, line : IR_VariableAccess) = {
    var body = ListBuffer[IR_Statement]()

    val std_get_line = IR_FunctionCall("std::getline", ifs, line)

    body += std_get_line
    body ++= delete_comments(line)

    var while_body = ListBuffer[IR_Statement]()
    while_body += IR_IfCondition(IR_Negation(std_get_line), IR_Return(false))
    while_body ++= delete_comments(line)

    body += IR_WhileLoop(IR_EqEq(IR_MemberFunctionCall(line, "size"), 0),
      while_body
    )
  }

  override def generateFct() = {
    var fctArgs : ListBuffer[IR_FunctionArgument] = ListBuffer()
    fctArgs += IR_FunctionArgument("ifs", IR_SpecialDatatype("std::ifstream&"))
    fctArgs += IR_FunctionArgument("iss", IR_SpecialDatatype("std::istringstream&"))

    var body = ListBuffer[IR_Statement]()

    // return an array with all information separated
    //val file = IR_VariableAccess("file", IR_SpecialDatatype("std::ifstream"))   // I think I could also use val here
    val line = IR_VariableAccess("line", IR_SpecialDatatype("std::string"))
    body += IR_VariableDeclaration(line)
    //body += IR_FunctionCall("std::getline", ifs, line)
    body ++= get_line(ifs, line)
    body += IR_MemberFunctionCall(iss, "clear")
    body += IR_MemberFunctionCall(iss, "str", line)
    body += IR_Return(true)

    // FIXME: move to app
    //body += IR_FunctionCall(IR_AllocateDataFunction.fctName)

    IR_PlainFunction(name, IR_BooleanDatatype, fctArgs, body)
  }
}
