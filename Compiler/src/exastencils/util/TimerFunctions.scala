package exastencils.util

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.mpi._
import exastencils.prettyprinting._

case class TimerDetail_AssignNow(var lhs : Expression) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = TimerDetail_AssignNow\n"

  def expand() : Output[Statement] = {
    Knowledge.timer_type match {
      case "Chrono" => AssignmentStatement(lhs, new FunctionCallExpression("std::chrono::high_resolution_clock::now"))
      case "QPC" => Scope(ListBuffer[Statement](
        VariableDeclarationStatement(SpecialDatatype("LARGE_INTEGER"), "now"),
        FunctionCallExpression("QueryPerformanceCounter", ListBuffer("&" ~ "now")),
        AssignmentStatement(lhs, MemberAccess(VariableAccess("now"), VariableAccess("QuadPart")))))
      case "WIN_TIME" => AssignmentStatement(lhs, CastExpression(RealDatatype, FunctionCallExpression("clock", ListBuffer())) / "CLOCKS_PER_SEC")
      case "UNIX_TIME" => Scope(ListBuffer[Statement](
        VariableDeclarationStatement(SpecialDatatype("timeval"), "timePoint"),
        FunctionCallExpression("gettimeofday", ListBuffer("&" ~ "timePoint", "NULL")),
        AssignmentStatement(lhs,
          CastExpression(RealDatatype, MemberAccess(VariableAccess("timePoint"), VariableAccess("tv_sec")) * 1e3
            + CastExpression(RealDatatype, MemberAccess(VariableAccess("timePoint"), VariableAccess("tv_usec")) * 1e-3)))))
      case "MPI_TIME"     => AssignmentStatement(lhs, FunctionCallExpression("MPI_Wtime", ListBuffer()))
      case "WINDOWS_RDSC" => AssignmentStatement(lhs, FunctionCallExpression("__rdtsc", ListBuffer()))
      case "RDSC"         => AssignmentStatement(lhs, FunctionCallExpression("__rdtsc", ListBuffer()))
    }
  }
}

case class TimerDetail_Zero() extends Expression {
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

case class TimerDetail_ReturnConvertToMS(var time : Expression) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = TimerDetail_ReturnConvertToMS\n"

  def expand() : Output[Statement] = {
    Knowledge.timer_type match {
      case "Chrono" => ReturnStatement(Some(new MemberFunctionCallExpression(new FunctionCallExpression("std::chrono::duration_cast<std::chrono::nanoseconds>", time), "count") * 1e-6))
      case "QPC" => Scope(ListBuffer[Statement](
        VariableDeclarationStatement(SpecialDatatype("static LARGE_INTEGER"), "s_frequency"),
        VariableDeclarationStatement(SpecialDatatype("static BOOL"), "s_use_qpc", Some(FunctionCallExpression("QueryPerformanceFrequency", ListBuffer("&" ~ "s_frequency")))),
        ReturnStatement(Some(time / ("s_frequency.QuadPart" / 1000.0)))))
      case "WIN_TIME"     => ReturnStatement(Some(time * 1e3))
      case "UNIX_TIME"    => ReturnStatement(Some(time))
      case "MPI_TIME"     => ReturnStatement(Some(time * 1e3))
      case "WINDOWS_RDSC" => ReturnStatement(Some(time / (2300000000l / 1000.0))) // FIXME: check if 2300000000l has to be processor specific
      case "RDSC"         => ReturnStatement(Some(time / (2300000000l / 1000.0))) // FIXME: check if 2300000000l has to be processor specific
    }
  }
}

case class TimerFunctions() extends FunctionCollection("Util/TimerFunctions",
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

abstract class AbstractTimerFunction extends AbstractFunctionStatement

object AbstractTimerFunction {
  def accessMember(member : String) = {
    MemberAccess(VariableAccess("stopWatch", Some(SpecialDatatype("StopWatch&"))), VariableAccess(member) // TODO: dt req?
    )
  }
}

case class TimerFct_StartTimer() extends AbstractTimerFunction with Expandable {
  import AbstractTimerFunction._

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = TimerFct_StartTimer\n"
  override def prettyprint_decl : String = prettyprint

  def expand() : Output[FunctionStatement] = {
    var statements = ListBuffer[Statement](
      new ConditionStatement(EqEqExpression(0, accessMember("numEntries")), ListBuffer[Statement](
        TimerDetail_AssignNow(accessMember("timerStarted")),
        AssignmentStatement(accessMember("lastTimeMeasured"), TimerDetail_Zero()))),
      if (Knowledge.experimental_timerEnableCallStacks) "CallTracker::StartTimer(&stopWatch)" else "",
      PreIncrementExpression(accessMember("numEntries")))

    FunctionStatement(UnitDatatype, s"startTimer", ListBuffer(VariableAccess("stopWatch", Some(SpecialDatatype("StopWatch&")))), statements, true, false)
  }
}

case class TimerFct_StopTimer() extends AbstractTimerFunction with Expandable {
  import AbstractTimerFunction._

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = TimerFct_StopTimer\n"
  override def prettyprint_decl : String = prettyprint

  def expand() : Output[FunctionStatement] = {
    var statements = ListBuffer[Statement](
      PreDecrementExpression(accessMember("numEntries")),
      new ConditionStatement(EqEqExpression(0, accessMember("numEntries")), ListBuffer[Statement](
        TimerDetail_AssignNow(accessMember("timerEnded")),
        AssignmentStatement(accessMember("lastTimeMeasured"),
          if ("Chrono" == Knowledge.timer_type)
            FunctionCallExpression("std::chrono::duration_cast<std::chrono::nanoseconds>", ListBuffer(accessMember("timerEnded") - accessMember("timerStarted")))
          else
            accessMember("timerEnded") - accessMember("timerStarted")),
        AssignmentStatement(accessMember("totalTimeMeasured"), accessMember("lastTimeMeasured"), "+="),
        if (Knowledge.experimental_timerEnableCallStacks) "CallTracker::StopTimer(&stopWatch)" else "",
        PreIncrementExpression(accessMember("numMeasurements")))))

    FunctionStatement(UnitDatatype, s"stopTimer", ListBuffer(VariableAccess("stopWatch", Some(SpecialDatatype("StopWatch&")))), statements, true, false)
  }
}

case class TimerFct_GetTotalTime /* in milliseconds */ () extends AbstractTimerFunction with Expandable {
  import AbstractTimerFunction._

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = TimerFct_GetTotalTime\n"
  override def prettyprint_decl : String = prettyprint

  def expand() : Output[FunctionStatement] = {
    var statements = ListBuffer[Statement](
      TimerDetail_ReturnConvertToMS(accessMember("totalTimeMeasured")))

    FunctionStatement(RealDatatype, s"getTotalTime", ListBuffer(VariableAccess("stopWatch", Some(SpecialDatatype("StopWatch&")))), statements, true, false)
  }
}

case class TimerFct_GetMeanTime /* in milliseconds */ () extends AbstractTimerFunction with Expandable {
  import AbstractTimerFunction._

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = TimerFct_GetMeanTime\n"
  override def prettyprint_decl : String = prettyprint

  def expand() : Output[FunctionStatement] = {
    var statements = ListBuffer[Statement](
      ReturnStatement(Some(new TernaryConditionExpression(
        GreaterExpression(accessMember("numMeasurements"), 0),
        FunctionCallExpression("getTotalTime", ListBuffer("stopWatch")) / accessMember("numMeasurements"),
        0.0))))

    FunctionStatement(RealDatatype, s"getMeanTime", ListBuffer(VariableAccess("stopWatch", Some(SpecialDatatype("StopWatch&")))), statements, true, false)
  }
}

case class TimerFct_GetLastTime /* in milliseconds */ () extends AbstractTimerFunction with Expandable {
  import AbstractTimerFunction._

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = TimerFct_GetLastTime\n"
  override def prettyprint_decl : String = prettyprint

  def expand() : Output[FunctionStatement] = {
    var statements = ListBuffer[Statement](
      TimerDetail_ReturnConvertToMS(accessMember("lastTimeMeasured")))

    FunctionStatement(RealDatatype, s"getLastTime", ListBuffer(VariableAccess("stopWatch", Some(SpecialDatatype("StopWatch&")))), statements, true, false)
  }
}

case class TimerFct_PrintAllTimers() extends AbstractTimerFunction with Expandable {
  import AbstractTimerFunction._

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = TimerFct_PrintAllTimers\n"
  override def prettyprint_decl : String = prettyprint

  def genPrintTimerCode(timer : iv.Timer) : Statement = {
    var statements : ListBuffer[Statement] = ListBuffer()

    val timeToPrint = "getTotalTime"
    statements += VariableDeclarationStatement(RealDatatype, "timerValue", Some(FunctionCallExpression(timeToPrint, ListBuffer(timer.resolveName))))

    if (Knowledge.mpi_enabled) {
      statements += new MPI_Allreduce("&timerValue", RealDatatype, 1, "+")
      statements += AssignmentStatement("timerValue", "mpiSize", "/=")
    }

    statements += PrintStatement(ListBuffer("\"Mean mean total time for Timer " ~ timer.name ~ ":\"", "timerValue"))

    Scope(statements)
  }

  def expand() : Output[FunctionStatement] = {
    CollectTimers.applyStandalone(StateManager.root)
    val timers = CollectTimers.timers

    var statements : ListBuffer[Statement] =
      if (timers.isEmpty)
        ListBuffer()
      else
        timers.toList.sortBy(_._1).map(t => genPrintTimerCode(t._2)).to[ListBuffer]

    FunctionStatement(UnitDatatype, s"printAllTimers", ListBuffer(), statements, true, false)
  }
}

case class TimerFct_PrintAllTimersToFile() extends AbstractTimerFunction with Expandable {
  import AbstractTimerFunction._

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = TimerFct_PrintAllTimersToFile\n"
  override def prettyprint_decl : String = prettyprint

  def genDataCollect(timers : HashMap[String, iv.Timer]) : ListBuffer[Statement] = {
    var statements : ListBuffer[Statement] = ListBuffer()

    var it = 0
    for (timer <- timers.toList.sortBy(_._1)) {
      statements += AssignmentStatement(ArrayAccess("timesToPrint", it), FunctionCallExpression("getTotalTime", ListBuffer(timer._2.resolveName)))
      it += 1
      statements += AssignmentStatement(ArrayAccess("timesToPrint", it), FunctionCallExpression("getMeanTime", ListBuffer(timer._2.resolveName)))
      it += 1
    }

    statements
  }

  def genPrint(timers : HashMap[String, iv.Timer]) : ListBuffer[Statement] = {
    var statements : ListBuffer[Statement] = ListBuffer()

    val stride : Expression = if (Knowledge.mpi_enabled) "mpiIt" else 0

    var it = 0
    var toPrint : ListBuffer[Expression] = ListBuffer()
    val sep = "\";\""
    for (timer <- timers.toList.sortBy(_._1)) {
      toPrint ++= ListBuffer[Expression]("\"" ~ timer._2.name ~ "\"", sep,
        ArrayAccess("timesToPrint", (stride * (2 * timers.size)) + it), sep,
        ArrayAccess("timesToPrint", (stride * (2 * timers.size)) + it + 1), sep)
      it += 2
    }
    // toPrint.dropRight(1) // remove last seperator
    statements += PrintStatement(toPrint, "outFile")

    if (Knowledge.mpi_enabled) {
      statements = ListBuffer[Statement](
        ForLoopStatement(
          VariableDeclarationStatement(IntegerDatatype, stride.prettyprint, Some(0)),
          LowerExpression(stride, Knowledge.mpi_numThreads),
          PreIncrementExpression(stride),
          statements))
    }

    statements.prepend(MemberFunctionCallExpression(VariableAccess("outFile"), "open", ListBuffer(("\"" + Knowledge.l3tmp_timerOuputFile + "\""))))
    statements.prepend(VariableDeclarationStatement(SpecialDatatype("std::ofstream"), "outFile"))
    statements.append(MemberFunctionCallExpression(VariableAccess("outFile"), "close", ListBuffer()))

    statements
  }

  def expand() : Output[FunctionStatement] = {
    CollectTimers.applyStandalone(StateManager.root)
    val timers = CollectTimers.timers

    var statements : ListBuffer[Statement] = ListBuffer()

    if (!timers.isEmpty)
      statements += ConditionStatement(MPI_IsRootProc(),
        ListBuffer[Statement](
          VariableDeclarationStatement(ArrayDatatype(RealDatatype, Knowledge.mpi_numThreads * 2 * timers.size), "timesToPrint"))
          ++ genDataCollect(timers)
          ++ ListBuffer[Statement](new MPI_Gather("timesToPrint", RealDatatype, 2 * timers.size))
          ++ genPrint(timers),
        ListBuffer[Statement](VariableDeclarationStatement(ArrayDatatype(RealDatatype, 2 * timers.size), "timesToPrint"))
          ++ genDataCollect(timers)
          ++ ListBuffer[Statement](MPI_Gather("timesToPrint", "timesToPrint", RealDatatype, 2 * timers.size)))

    FunctionStatement(UnitDatatype, s"printAllTimersToFile", ListBuffer(), statements, true, false)
  }
}
