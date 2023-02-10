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

package exastencils.parallelization.api.cuda

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.core.ObjectWithState
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir.StatementList
import exastencils.util.ir.IR_RawPrint

/// CUDA_CheckError

object CUDA_CheckError extends ObjectWithState {
  var counter : Int = 0

  override def clear() = { counter = 0 }
}

case class CUDA_CheckError(var exp : IR_Expression) extends CUDA_HostStatement with IR_Expandable {

  import CUDA_CheckError._

  override def expand() : Output[StatementList] = {
    val statusName = s"cudaStatus_$counter"
    counter += 1

    def status = IR_VariableAccess(statusName, IR_SpecialDatatype("cudaError_t"))

    def print = IR_RawPrint("\"CUDA error in file (\"", "__FILE__", "\"), line (\"", "__LINE__", "\"): \"", status,
      "\" -> \"", IR_FunctionCall(IR_ExternalFunctionReference("cudaGetErrorString"), status), "std::endl")

    def printAndExit : ListBuffer[IR_Statement] = ListBuffer(print, IR_FunctionCall(IR_ExternalFunctionReference("exit"), 1))

    ListBuffer(
      IR_VariableDeclaration(status, exp),
      IR_IfCondition("cudaSuccess" Neq status,
        printAndExit,
        ListBuffer(
          IR_Assignment(status, IR_FunctionCall(IR_ExternalFunctionReference("cudaGetLastError"))),
          IR_IfCondition("cudaSuccess" Neq status,
            printAndExit
          ))))
  }
}
