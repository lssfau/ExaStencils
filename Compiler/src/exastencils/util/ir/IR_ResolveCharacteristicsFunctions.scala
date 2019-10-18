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

package exastencils.util.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.datastructures._
import exastencils.parallelization.api.mpi.MPI_IsRootProc

/// IR_ResolveCharacteristicsFunctions

object IR_ResolveCharacteristicsFunctions extends DefaultStrategy("ResolveCharacteristicsFunctions") {
  this += new Transformation("ResolveFunctionCalls", {

    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("clearCharacteristics", _), args)) =>
      var stmts = ListBuffer[IR_Statement]()

      stmts += IR_VariableDeclaration(IR_SpecialDatatype("std::ofstream"), "outFile")
      stmts += IR_MemberFunctionCall(IR_VariableAccess("outFile", IR_UnknownDatatype), "open", "\"" + Settings.characteristicsFile + "\"")
      stmts += IR_MemberFunctionCall(IR_VariableAccess("outFile", IR_UnknownDatatype), "close")

      if (Knowledge.mpi_enabled)
        IR_IfCondition(MPI_IsRootProc(), stmts)
      else
        IR_Scope(stmts)

    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("logCharacteristics", _), args)) =>
      var stmts = ListBuffer[IR_Statement]()

      stmts += IR_VariableDeclaration(IR_SpecialDatatype("std::ofstream"), "outFile")
      stmts += IR_MemberFunctionCall(IR_VariableAccess("outFile", IR_UnknownDatatype), "open", "\"" + Settings.characteristicsFile + "\"", "std::ofstream::app")

      args.foreach(a => stmts += IR_Print(IR_VariableAccess("outFile", IR_UnknownDatatype), ListBuffer(a, IR_StringConstant(Settings.csvSeparator))))
      stmts += IR_Print(IR_VariableAccess("outFile", IR_UnknownDatatype), IR_StringConstant("\\n"))

      stmts += IR_MemberFunctionCall(IR_VariableAccess("outFile", IR_UnknownDatatype), "close")

      if (Knowledge.mpi_enabled)
        IR_IfCondition(MPI_IsRootProc(), stmts)
      else
        IR_Scope(stmts)
  })
}
