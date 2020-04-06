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

import exastencils.base.ir.IR_Node
import exastencils.config._
import exastencils.prettyprinting._

/// IR_Stopwatch

object IR_Stopwatch {
  def defHeader = "Util/Stopwatch.h"
}

case class IR_Stopwatch() extends IR_Node with FilePrettyPrintable {
  override def printToFile() : Unit = {
    val writerHeader = PrettyprintingManager.getPrinter(IR_Stopwatch.defHeader)
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
    writerHeader << s"\t\t: numEntries(0), numMeasurements(0), lastTimeMeasured(${ IR_ZeroTimerValue().prettyprint() }), totalTimeMeasured(${ IR_ZeroTimerValue().prettyprint() }), totalTimeAveraged(0.0)\n"
    writerHeader <<< s"\t{}\n"

    writerHeader <<< s"\tstd::string timerName;\n"

    writerHeader <<< s"\tint numEntries;"
    writerHeader <<< s"\tint numMeasurements;"
    writerHeader <<< s"\t$timerTimepointType timerStarted;"
    writerHeader <<< s"\t$timerTimepointType timerEnded;"
    writerHeader <<< s"\t$timerDurationType lastTimeMeasured;"
    writerHeader <<< s"\t$timerDurationType totalTimeMeasured;"
    writerHeader <<< s"\tdouble totalTimeAveraged;"
    writerHeader <<< s"};\n"
  }
}
