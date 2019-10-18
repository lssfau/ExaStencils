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

package exastencils.parallelization.api.mpi

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.config.Knowledge
import exastencils.parallelization.ir.IR_PotentiallyCritical
import exastencils.util.ir.IR_RawPrint

/// MPI_WaitForRequest

case object MPI_WaitForRequest extends IR_FuturePlainFunction {
  exastencils.core.Duplicate.registerConstant(this)

  override var name = "waitForMPIReq"
  allowInlining = false

  override def prettyprint_decl() : String = prettyprint

  def request = IR_VariableAccess("request", IR_PointerDatatype(IR_SpecialDatatype("MPI_Request")))
  def stat = IR_VariableAccess("stat", IR_SpecialDatatype("MPI_Status"))
  def flag = IR_VariableAccess("flag", IR_IntegerDatatype)
  def result = IR_VariableAccess("result", IR_IntegerDatatype)

  def msg = IR_VariableAccess("msg", IR_ArrayDatatype(IR_SpecialDatatype("char"), 64 * 1024))
  def len = IR_VariableAccess("len", IR_IntegerDatatype)

  def checkError = {
    IR_IfCondition("MPI_ERR_IN_STATUS" EqEq result, ListBuffer[IR_Statement](
      IR_VariableDeclaration(msg),
      IR_VariableDeclaration(len),
      IR_FunctionCall(MPI_FunctionReference("MPI_Error_string", IR_IntegerDatatype),
        IR_MemberAccess(stat, "MPI_ERROR"), msg, IR_AddressOf(len)),
      IR_RawPrint("\"MPI Error encountered (\"", msg, "\")\"")))
  }

  override def generateFct() = {
    val fct = IR_PlainFunction(name, IR_UnitDatatype, IR_FunctionArgument(request), ListBuffer[IR_Statement]())

    // add declarations for local variables
    fct.body += IR_VariableDeclaration(stat)
    fct.body += IR_VariableDeclaration(result)

    if (Knowledge.mpi_useBusyWait) {
      // busy wait => generate while loop polling through MPI_Test
      fct.body += IR_VariableDeclaration(flag, 0)
      fct.body += IR_WhileLoop(0 EqEq flag,
        IR_PotentiallyCritical(IR_Assignment(result, IR_FunctionCall("MPI_Test",
          request, IR_AddressOf(flag), IR_AddressOf(stat)))),
        checkError)
    } else {
      fct.body += IR_PotentiallyCritical(IR_Assignment(result, IR_FunctionCall("MPI_Wait", request, IR_AddressOf(stat))))
      fct.body += checkError
    }
    fct.body += IR_Assignment(IR_DerefAccess(request), IR_FunctionCall("MPI_Request"))

    fct
  }

  def generateFctAccess() = IR_PlainInternalFunctionReference(name, IR_UnitDatatype)
}
