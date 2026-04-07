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

package exastencils.timing.ir

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation.Output
import exastencils.prettyprinting.PpStream

/// IR_AssignNowToTimer

case class IR_AssignNowToTimer(var lhs : IR_Expression) extends IR_Statement with IR_Expandable {
  override def expand() : Output[IR_Statement] = {
    Knowledge.timer_type match {
      case "Chrono"       => IR_Assignment(lhs, IR_FunctionCall("std::chrono::high_resolution_clock::now"))
      case "QPC"          => IR_Scope(ListBuffer[IR_Statement](
        IR_VariableDeclaration("LARGE_INTEGER", "now"),
        IR_FunctionCall("QueryPerformanceCounter", IR_AddressOf("now")),
        IR_Assignment(lhs, IR_MemberAccess(IR_VariableAccess("now", "LARGE_INTEGER"), "QuadPart"))))
      case "WIN_TIME"     => IR_Assignment(lhs, IR_Cast(IR_DoubleDatatype, IR_FunctionCall("clock")) / "CLOCKS_PER_SEC")
      case "UNIX_TIME"    => IR_Scope(
        IR_VariableDeclaration("timeval", "timePoint"),
        IR_FunctionCall("gettimeofday", IR_AddressOf("timePoint"), "NULL"),
        IR_Assignment(lhs,
          IR_Cast(IR_DoubleDatatype, IR_MemberAccess(IR_VariableAccess("timePoint", "timeval"), "tv_sec") * 1e3
            + IR_Cast(IR_DoubleDatatype, IR_MemberAccess(IR_VariableAccess("timePoint", "timeval"), "tv_usec") * 1e-3))))
      case "MPI_TIME"     => IR_Assignment(lhs, IR_FunctionCall("MPI_Wtime"))
      case "WINDOWS_RDSC" => IR_Assignment(lhs, IR_FunctionCall("__rdtsc"))
      case "RDSC"         => IR_Assignment(lhs, IR_FunctionCall("__rdtsc"))
    }
  }
}

/// IR_ZeroTimerValue

case class IR_ZeroTimerValue() extends IR_Expression {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = {
    out << (Knowledge.timer_type match {
      case "Chrono"       => "std::chrono::nanoseconds::zero()"
      case "QPC"          => "0"
      case "WIN_TIME"     => "0.0"
      case "UNIX_TIME"    => "0.0"
      case "MPI_TIME"     => "0.0"
      case "WINDOWS_RDSC" => "0"
      case "RDSC"         => "0"
    })
  }
}

/// IR_ReturnConvertToMS

case class IR_ReturnConvertToMS(var time : IR_Expression) extends IR_Statement with IR_Expandable {
  override def expand() : Output[IR_Statement] = {
    Knowledge.timer_type match {
      case "Chrono"       => IR_Return(Some(
        IR_DoubleConstant(1e-6) *
        IR_Cast(
          IR_DoubleDatatype,
          IR_MemberFunctionCall(IR_FunctionCall("std::chrono::duration_cast<std::chrono::nanoseconds>", time), "count"))))
      case "QPC"          => IR_Scope(ListBuffer[IR_Statement](
        IR_VariableDeclaration(IR_SpecialDatatype("static LARGE_INTEGER"), "s_frequency"),
        IR_VariableDeclaration(IR_SpecialDatatype("static BOOL"), "s_use_qpc", IR_FunctionCall("QueryPerformanceFrequency", IR_AddressOf("s_frequency"))),
        IR_Return(Some(time / ("s_frequency.QuadPart" / 1000.0)))))
      case "WIN_TIME"     => IR_Return(Some(time * 1e3))
      case "UNIX_TIME"    => IR_Return(Some(time))
      case "MPI_TIME"     => IR_Return(Some(time * 1e3))
      case "WINDOWS_RDSC" => IR_Return(Some(time / (2300000000l / 1000.0))) // FIXME: check if 2300000000l has to be processor specific
      case "RDSC"         => IR_Return(Some(time / (2300000000l / 1000.0))) // FIXME: check if 2300000000l has to be processor specific
    }
  }
}
