package exastencils.timing.ir

import exastencils.config._
import exastencils.datastructures._
import exastencils.deprecated.ir._
import exastencils.prettyprinting._

/// IR_Stopwatch

case class IR_Stopwatch() extends Node with FilePrettyPrintable {
  override def printToFile() : Unit = {
    if (Knowledge.experimental_timerEnableCallStacks) {
      IR_CallEntity().printToFile()
      IR_CallTracker().printToFile()
    }

    val writerHeader = PrettyprintingManager.getPrinter(s"Util/Stopwatch.h")
    writerHeader.addExternalDependency("string")

    Knowledge.timer_type match {
      case "Chrono"       => writerHeader.addExternalDependency("chrono")
      case "QPC"          => writerHeader.addExternalDependency("windows.h")
      case "WIN_TIME"     =>
        writerHeader.addExternalDependency("time.h"); writerHeader.addExternalDependency("sys/types.h")
      case "UNIX_TIME"    =>
        writerHeader.addExternalDependency("sys/time.h"); writerHeader.addExternalDependency("sys/types.h")
      case "MPI_TIME"     => writerHeader.addExternalDependency("mpi.h")
      case "WINDOWS_RDSC" =>
      case "RDSC"         => writerHeader.addExternalDependency("stdint.h")
    }

    var timerDurationType = ""
    var timerTimepointType = ""

    Knowledge.timer_type match {
      case "Chrono"       =>
        timerDurationType = "std::chrono::nanoseconds"; timerTimepointType = "std::chrono::high_resolution_clock::time_point"
      case "QPC"          =>
        timerDurationType = "long long"; timerTimepointType = "long long"
      case "WIN_TIME"     =>
        timerDurationType = "double"; timerTimepointType = "double"
      case "UNIX_TIME"    =>
        timerDurationType = "double"; timerTimepointType = "double"
      case "MPI_TIME"     =>
        timerDurationType = "double"; timerTimepointType = "double"
      case "WINDOWS_RDSC" =>
        timerDurationType = "long long"; timerTimepointType = "long long"
      case "RDSC"         => timerDurationType = "long long"; timerTimepointType = "long long"
    }

    writerHeader <<< s"class StopWatch {"
    writerHeader <<< s"public:"
    writerHeader <<< s"\tStopWatch()"
    writerHeader << s"\t\t: numEntries(0), numMeasurements(0), lastTimeMeasured(${ IR_ZeroTimerValue().prettyprint() }), totalTimeMeasured(${ IR_ZeroTimerValue().prettyprint() })\n"
    writerHeader <<< s"\t{}\n"

    writerHeader <<< s"\tstd::string timerName;\n"

    writerHeader <<< s"\tint numEntries;"
    writerHeader <<< s"\tint numMeasurements;"
    writerHeader <<< s"\t$timerTimepointType timerStarted;"
    writerHeader <<< s"\t$timerTimepointType timerEnded;"
    writerHeader <<< s"\t$timerDurationType lastTimeMeasured;"
    writerHeader <<< s"\t$timerDurationType totalTimeMeasured;"
    writerHeader <<< s"};\n"
  }
}
