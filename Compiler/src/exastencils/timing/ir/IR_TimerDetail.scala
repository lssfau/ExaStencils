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
      case "Chrono"       => IR_Return(Some(IR_MemberFunctionCall(IR_FunctionCall("std::chrono::duration_cast<std::chrono::nanoseconds>", time), "count") * 1e-6))
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
