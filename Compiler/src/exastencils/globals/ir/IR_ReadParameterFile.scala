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

package exastencils.globals.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_UserFunctions
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.domain.ir.IR_ReadLineFromFile
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi._
import exastencils.util.ir.IR_Read

/// IR_ReadParameterFile

case class IR_ReadParameterFile() extends IR_FuturePlainFunction {
  override var name = "readParameterFile"
  override def prettyprint_decl() = prettyprint

  val issVal = IR_VariableAccess("issVal", IR_SpecialDatatype("std::istringstream"))

  def overwriteParams(v : IR_VariableDeclaration) = {
    var body = ListBuffer[IR_Statement]()

    //val buf = IR_VariableAccess("buf", v.datatype)
    //body += IR_VariableDeclaration(buf)
    body += IR_Read(issVal, IR_VariableAccess(v))
    //body += IR_Assignment(IR_VariableAccess(v), buf)
    body += IR_Continue()
  }

  def bcastParams(variables : ListBuffer[IR_VariableDeclaration]) = {
    var bcastStmts = ListBuffer[IR_Statement]()

    variables.foreach(v => {
      bcastStmts += MPI_Bcast(IR_AddressOf(IR_VariableAccess(v)), 1, v.datatype, 0)
    })

    bcastStmts
  }

  override def generateFct() = {
    var body = ListBuffer[IR_Statement]()

    def globalParams = IR_GlobalCollection.get.variables.filter(_.datatype.isInstanceOf[IR_ScalarDatatype])

    if (Knowledge.mpi_enabled) {
      body += IR_IfCondition(IR_Neq(MPI_IV_MpiRank, IR_IntegerConstant(0)),
        bcastParams(globalParams) ++ ListBuffer[IR_Statement](IR_Return())
      )
    }

    val file = IR_VariableAccess("file", IR_SpecialDatatype("std::ifstream"))

    def fileName = IR_VariableAccess("fileName", IR_StringDatatype)

    body += IR_VariableDeclaration(file)
    body += IR_MemberFunctionCall(file, "open", fileName)
    body += IR_Assert(IR_MemberFunctionCall(file, "is_open"), ListBuffer("\"Unable to open file \"", fileName), IR_FunctionCall("exit", 1))

    val iss = IR_VariableAccess("iss", IR_SpecialDatatype("std::istringstream"))
    body += IR_VariableDeclaration(iss)
    val paramName = IR_VariableAccess("name", IR_StringDatatype)
    val paramVal = IR_VariableAccess("value", IR_StringDatatype)
    body += IR_VariableDeclaration(paramName)
    body += IR_VariableDeclaration(paramVal)

    body += IR_WhileLoop(
      IR_FunctionCall(IR_ReadLineFromFile.name, file, iss),
      ListBuffer[IR_Statement](
        IR_Read(iss, paramName, paramVal),
        IR_ObjectInstantiation(issVal, paramVal)
      ) ++ globalParams.map(v => IR_IfCondition(IR_EqEq(paramName, IR_StringConstant(v.name)), overwriteParams(v)))
    )

    // mpi broadcast
    if (Knowledge.mpi_enabled) {
      body ++= bcastParams(globalParams)
    }

    IR_PlainFunction(name, IR_UnitDatatype, IR_FunctionArgument(fileName), body)
  }
}

/// IR_ResolveReadParameters

object IR_ResolveReadParameters extends DefaultStrategy("ResolveReadParameters") {
  this += new Transformation("ResolveFunctionCalls", {
    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("readParameterFile", _), args)) =>
      if (args.size != 1
        || !(args.head.isInstanceOf[IR_StringConstant]
        || (args.head.isInstanceOf[IR_VariableAccess] && args.head.asInstanceOf[IR_VariableAccess].datatype == IR_StringDatatype))) {
        Logger.error("Malformed call to readParameterFile; usage: readParameterFile ( \"filename\" )")
      }

      if (!IR_GlobalCollection.get.functions.exists(_.name == "readParameterFile")) {
        IR_ReadLineFromFile.addToUtil
        IR_UserFunctions.get.internalDependencies += IR_GlobalCollection.defHeader
        IR_UserFunctions.get.internalDependencies = IR_UserFunctions.get.internalDependencies.distinct
        IR_GlobalCollection.get.functions += IR_ReadParameterFile()
        IR_GlobalCollection.get.externalDependencies += "iostream"
        IR_GlobalCollection.get.externalDependencies = IR_GlobalCollection.get.externalDependencies.distinct
      }

      IR_ExpressionStatement(IR_FunctionCall(IR_PlainInternalFunctionReference("readParameterFile", IR_UnitDatatype), args))
  })
}
