package exastencils.util

import scala.collection.mutable._

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.mpi._
import exastencils.prettyprinting._

case class TimerDetail_AssignNow(var lhs : IR_Expression) extends IR_Statement with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = TimerDetail_AssignNow\n"

  override def expand() : Output[IR_Statement] = {
    Knowledge.timer_type match {
      case "Chrono"       => IR_Assignment(lhs, new FunctionCallExpression("std::chrono::high_resolution_clock::now"))
      case "QPC"          => IR_Scope(ListBuffer[IR_Statement](
        IR_VariableDeclaration(IR_SpecialDatatype("LARGE_INTEGER"), "now"),
        FunctionCallExpression("QueryPerformanceCounter", ListBuffer(IR_AddressofExpression("now"))),
        IR_Assignment(lhs, MemberAccess(IR_VariableAccess("now"), "QuadPart"))))
      case "WIN_TIME"     => IR_Assignment(lhs, CastExpression(IR_DoubleDatatype, FunctionCallExpression("clock", ListBuffer())) / "CLOCKS_PER_SEC")
      case "UNIX_TIME"    => IR_Scope(ListBuffer[IR_Statement](
        IR_VariableDeclaration(IR_SpecialDatatype("timeval"), "timePoint"),
        FunctionCallExpression("gettimeofday", ListBuffer(IR_AddressofExpression("timePoint"), "NULL")),
        IR_Assignment(lhs,
          CastExpression(IR_DoubleDatatype, MemberAccess(IR_VariableAccess("timePoint"), "tv_sec") * 1e3
            + CastExpression(IR_DoubleDatatype, MemberAccess(IR_VariableAccess("timePoint"), "tv_usec") * 1e-3)))))
      case "MPI_TIME"     => IR_Assignment(lhs, FunctionCallExpression("MPI_Wtime", ListBuffer()))
      case "WINDOWS_RDSC" => IR_Assignment(lhs, FunctionCallExpression("__rdtsc", ListBuffer()))
      case "RDSC"         => IR_Assignment(lhs, FunctionCallExpression("__rdtsc", ListBuffer()))
    }
  }
}

case class TimerDetail_Zero() extends IR_Expression {
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

case class TimerDetail_ReturnConvertToMS(var time : IR_Expression) extends IR_Statement with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = TimerDetail_ReturnConvertToMS\n"

  override def expand() : Output[IR_Statement] = {
    Knowledge.timer_type match {
      case "Chrono"       => IR_Return(Some(new MemberFunctionCallExpression(new FunctionCallExpression("std::chrono::duration_cast<std::chrono::nanoseconds>", time), "count") * 1e-6))
      case "QPC"          => IR_Scope(ListBuffer[IR_Statement](
        IR_VariableDeclaration(IR_SpecialDatatype("static LARGE_INTEGER"), "s_frequency"),
        IR_VariableDeclaration(IR_SpecialDatatype("static BOOL"), "s_use_qpc", Some(FunctionCallExpression("QueryPerformanceFrequency", ListBuffer(IR_AddressofExpression("s_frequency"))))),
        IR_Return(Some(time / ("s_frequency.QuadPart" / 1000.0)))))
      case "WIN_TIME"     => IR_Return(Some(time * 1e3))
      case "UNIX_TIME"    => IR_Return(Some(time))
      case "MPI_TIME"     => IR_Return(Some(time * 1e3))
      case "WINDOWS_RDSC" => IR_Return(Some(time / (2300000000l / 1000.0))) // FIXME: check if 2300000000l has to be processor specific
      case "RDSC"         => IR_Return(Some(time / (2300000000l / 1000.0))) // FIXME: check if 2300000000l has to be processor specific
    }
  }
}

case class TimerFunctions() extends IR_FunctionCollection("Util/TimerFunctions",
  ListBuffer("fstream"),
  ListBuffer("Globals/Globals.h", "Util/Stopwatch.h")) {

  functions += TimerFct_StartTimer()
  functions += TimerFct_StopTimer()
  functions += TimerFct_GetTotalTime()
  functions += TimerFct_GetMeanTime()
  functions += TimerFct_GetLastTime()
  functions += TimerFct_PrintAllTimers()
  functions += TimerFct_PrintAllTimersToFile()

  if (Knowledge.experimental_timerEnableCallStacks) {
    internalDependencies += s"Util/CallEntity.h"
    internalDependencies += s"Util/CallTracker.h"
  }
}

abstract class AbstractTimerFunction extends IR_AbstractFunction

object AbstractTimerFunction {
  def accessMember(member : String) = {
    MemberAccess(IR_VariableAccess("stopWatch", Some(IR_SpecialDatatype("StopWatch&"))), member)
  }
}

case class TimerFct_StartTimer() extends AbstractTimerFunction with IR_Expandable {

  import AbstractTimerFunction._

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = TimerFct_StartTimer\n"
  override def prettyprint_decl : String = prettyprint
  override def name = "startTimer"

  override def expand() : Output[IR_Function] = {
    var statements = ListBuffer[IR_Statement](
      IR_IfCondition(IR_EqEqExpression(0, accessMember("numEntries")), ListBuffer[IR_Statement](
        TimerDetail_AssignNow(accessMember("timerStarted")),
        IR_Assignment(accessMember("lastTimeMeasured"), TimerDetail_Zero()))),
      if (Knowledge.experimental_timerEnableCallStacks) "CallTracker::StartTimer(&stopWatch)" else "",
      IR_PreIncrementExpression(accessMember("numEntries")))

    IR_Function(IR_UnitDatatype, name, ListBuffer(IR_FunctionArgument("stopWatch", IR_SpecialDatatype("StopWatch&"))), statements, true, false)
  }
}

case class TimerFct_StopTimer() extends AbstractTimerFunction with IR_Expandable {

  import AbstractTimerFunction._

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = TimerFct_StopTimer\n"
  override def prettyprint_decl : String = prettyprint
  override def name = "stopTimer"

  override def expand() : Output[IR_Function] = {
    var statements = ListBuffer[IR_Statement](
      IR_PreDecrementExpression(accessMember("numEntries")),
      IR_IfCondition(IR_EqEqExpression(0, accessMember("numEntries")), ListBuffer[IR_Statement](
        TimerDetail_AssignNow(accessMember("timerEnded")),
        IR_Assignment(accessMember("lastTimeMeasured"),
          if ("Chrono" == Knowledge.timer_type)
            FunctionCallExpression("std::chrono::duration_cast<std::chrono::nanoseconds>", ListBuffer(accessMember("timerEnded") - accessMember("timerStarted")))
          else
            accessMember("timerEnded") - accessMember("timerStarted")),
        IR_Assignment(accessMember("totalTimeMeasured"), accessMember("lastTimeMeasured"), "+="),
        if (Knowledge.experimental_timerEnableCallStacks) "CallTracker::StopTimer(&stopWatch)" else "",
        IR_PreIncrementExpression(accessMember("numMeasurements")))))

    IR_Function(IR_UnitDatatype, name, ListBuffer(IR_FunctionArgument("stopWatch", IR_SpecialDatatype("StopWatch&"))), statements, true, false)
  }
}

case class TimerFct_GetTotalTime /* in milliseconds */ () extends AbstractTimerFunction with IR_Expandable {

  import AbstractTimerFunction._

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = TimerFct_GetTotalTime\n"
  override def prettyprint_decl : String = prettyprint
  override def name = "getTotalTime"

  override def expand() : Output[IR_Function] = {
    var statements = ListBuffer[IR_Statement](
      TimerDetail_ReturnConvertToMS(accessMember("totalTimeMeasured")))

    IR_Function(IR_DoubleDatatype, name, ListBuffer(IR_FunctionArgument("stopWatch", IR_SpecialDatatype("StopWatch&"))), statements, true, false)
  }
}

case class TimerFct_GetMeanTime /* in milliseconds */ () extends AbstractTimerFunction with IR_Expandable {

  import AbstractTimerFunction._

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = TimerFct_GetMeanTime\n"
  override def prettyprint_decl : String = prettyprint
  override def name = "getMeanTime"

  override def expand() : Output[IR_Function] = {
    var statements = ListBuffer[IR_Statement](
      IR_Return(Some(new TernaryConditionExpression(
        IR_GreaterExpression(accessMember("numMeasurements"), 0),
        FunctionCallExpression("getTotalTime", ListBuffer("stopWatch")) / accessMember("numMeasurements"),
        0.0))))

    IR_Function(IR_DoubleDatatype, name, ListBuffer(IR_FunctionArgument("stopWatch", IR_SpecialDatatype("StopWatch&"))), statements, true, false)
  }
}

case class TimerFct_GetLastTime /* in milliseconds */ () extends AbstractTimerFunction with IR_Expandable {

  import AbstractTimerFunction._

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = TimerFct_GetLastTime\n"
  override def prettyprint_decl : String = prettyprint
  override def name = "getLastTime"

  override def expand() : Output[IR_Function] = {
    var statements = ListBuffer[IR_Statement](
      TimerDetail_ReturnConvertToMS(accessMember("lastTimeMeasured")))

    IR_Function(IR_DoubleDatatype, name, ListBuffer(IR_FunctionArgument("stopWatch", IR_SpecialDatatype("StopWatch&"))), statements, true, false)
  }
}

case class TimerFct_PrintAllTimers() extends AbstractTimerFunction with IR_Expandable {

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = TimerFct_PrintAllTimers\n"
  override def prettyprint_decl : String = prettyprint
  override def name = "printAllTimers"

  def genPrintTimerCode(timer : iv.Timer) : IR_Statement = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    val timeToPrint = "getTotalTime"
    statements += IR_VariableDeclaration(IR_DoubleDatatype, "timerValue", Some(FunctionCallExpression(timeToPrint, ListBuffer(timer.resolveName))))

    if (Knowledge.mpi_enabled) {
      statements += new MPI_Allreduce("&timerValue", IR_DoubleDatatype, 1, "+")
      statements += IR_Assignment("timerValue", "mpiSize", "/=")
    }

    statements += PrintStatement(ListBuffer("\"Mean mean total time for Timer " + timer.name.prettyprint() + ":\"", "timerValue"))

    IR_Scope(statements)
  }

  override def expand() : Output[IR_Function] = {
    CollectTimers.applyStandalone(StateManager.root)
    val timers = CollectTimers.timers

    var statements : ListBuffer[IR_Statement] =
      if (timers.isEmpty)
        ListBuffer()
      else
        timers.toList.sortBy(_._1).map(t => genPrintTimerCode(t._2)).to[ListBuffer]

    IR_Function(IR_UnitDatatype, name, ListBuffer(), statements, true, false)
  }
}

case class TimerFct_PrintAllTimersToFile() extends AbstractTimerFunction with IR_Expandable {

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = TimerFct_PrintAllTimersToFile\n"
  override def prettyprint_decl : String = prettyprint
  override def name = "printAllTimersToFile"

  def genDataCollect(timers : HashMap[String, iv.Timer]) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    var it = 0
    for (timer <- timers.toList.sortBy(_._1)) {
      statements += IR_Assignment(IR_ArrayAccess("timesToPrint", it), FunctionCallExpression("getTotalTime", ListBuffer(timer._2.resolveName)))
      it += 1
      statements += IR_Assignment(IR_ArrayAccess("timesToPrint", it), FunctionCallExpression("getMeanTime", ListBuffer(timer._2.resolveName)))
      it += 1
    }

    statements
  }

  def genPrint(timers : HashMap[String, iv.Timer]) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    val stride : IR_Expression = if (Knowledge.mpi_enabled && Knowledge.l3tmp_printTimersToFileForEachRank) "mpiIt" else 0

    var it = 0
    val sep = "\"" + Settings.csvSeparatorEscaped() + "\""
    for (timer <- timers.toList.sortBy(_._1)) {
      statements += PrintExpression(IR_VariableAccess("outFile"), ListBuffer[IR_Expression](
        IR_StringConstant(timer._2.name.prettyprint()), sep,
        IR_ArrayAccess("timesToPrint", (stride * (2 * timers.size)) + it), sep,
        IR_ArrayAccess("timesToPrint", (stride * (2 * timers.size)) + it + 1), IR_StringConstant("\\n")))

      it += 2
    }

    // wrap in loop over each rank if required
    if (Knowledge.mpi_enabled && Knowledge.l3tmp_printTimersToFileForEachRank) {
      statements = ListBuffer[IR_Statement](
        IR_ForLoop(
          IR_VariableDeclaration(IR_IntegerDatatype, stride.prettyprint, 0),
          IR_LowerExpression(stride, Knowledge.mpi_numThreads),
          IR_PreIncrementExpression(stride),
          statements))
    }

    statements.prepend(MemberFunctionCallExpression(IR_VariableAccess("outFile"), "open", ListBuffer(("\"" + Knowledge.l3tmp_timerOuputFile + "\""))))
    statements.prepend(IR_VariableDeclaration(IR_SpecialDatatype("std::ofstream"), "outFile"))
    statements.append(MemberFunctionCallExpression(IR_VariableAccess("outFile"), "close", ListBuffer()))

    statements
  }

  override def expand() : Output[IR_Function] = {
    CollectTimers.applyStandalone(StateManager.root)
    val timers = CollectTimers.timers

    var statements : ListBuffer[IR_Statement] = ListBuffer()

    if (!timers.isEmpty) {
      if (Knowledge.l3tmp_printTimersToFileForEachRank) {
        statements += IR_IfCondition(MPI_IsRootProc(),
          ListBuffer[IR_Statement](
            IR_VariableDeclaration(IR_ArrayDatatype(IR_DoubleDatatype, Knowledge.mpi_numThreads * 2 * timers.size), "timesToPrint"))
            ++ genDataCollect(timers)
            ++ ListBuffer[IR_Statement](new MPI_Gather("timesToPrint", IR_DoubleDatatype, 2 * timers.size))
            ++ genPrint(timers),
          ListBuffer[IR_Statement](IR_VariableDeclaration(IR_ArrayDatatype(IR_DoubleDatatype, 2 * timers.size), "timesToPrint"))
            ++ genDataCollect(timers)
            ++ ListBuffer[IR_Statement](MPI_Gather("timesToPrint", "timesToPrint", IR_DoubleDatatype, 2 * timers.size)))
      } else {
        statements += IR_VariableDeclaration(IR_ArrayDatatype(IR_DoubleDatatype, 2 * timers.size), "timesToPrint")
        statements ++= genDataCollect(timers)
        statements += new MPI_Reduce(0, "timesToPrint", IR_DoubleDatatype, 2 * timers.size, "+")
        def timerId = IR_VariableAccess("timerId", Some(IR_IntegerDatatype))
        statements += IR_ForLoop(IR_VariableDeclaration(timerId, 0), IR_LowerExpression(timerId, 2 * timers.size), IR_PreIncrementExpression(timerId),
          IR_Assignment(IR_ArrayAccess("timesToPrint", timerId), Knowledge.mpi_numThreads, "/="))
        statements += IR_IfCondition(MPI_IsRootProc(), genPrint(timers))
      }
    }

    IR_Function(IR_UnitDatatype, name, ListBuffer(), statements, true, false)
  }
}
