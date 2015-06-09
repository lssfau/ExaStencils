package exastencils.prettyprinting

import exastencils.core._
import exastencils.knowledge._

object JobScriptGenerator {
  def write : Unit = {
    Knowledge.targetCompiler match {
      case "IBMBG" | "IBMXL" => {
        val numOMP = Knowledge.omp_numThreads
        val numMPI = Knowledge.mpi_numThreads
        val numThreadsPerNode = 64
        val numMPIRanksPerNode = numThreadsPerNode / numOMP
        val numNodes = (numOMP * numMPI) / 64

        val printer = PrettyprintingManager.getPrinter("runJuQueen")
        printer <<< s"#@ shell = /bin/bash"
        printer <<< s"#@ job_name = GENERATED_$numNodes"
        printer <<< "#@ error = $(job_name).$(jobid).out"
        printer <<< "#@ output = $(job_name).$(jobid).out"
        printer <<< s"#@ environment = COPY_ALL"
        printer <<< s"#@ notification = always"
        Settings.user.toLowerCase() match {
          case "sebastian" | "kuckuk" | "sebastiankuckuk" => printer <<< s"#@ notify_user = sebastian.kuckuk@fau.de"
          case "christian" | "schmitt" | "christianschmitt" => printer <<< s"#@ notify_user = christian.schmitt@cs.fau.de"
          case "stefan" | "kronawitter" | "stefankronawitter" => printer <<< s"#@ notify_user = kronast@fim.uni-passau.de"
          case "alex" | "alexander" | "grebhahn" | "alexandergrebhahn" => printer <<< s"#@ notify_user = grebhahn@fim.uni-passau.de"
          case "hannah" | "rittich" | "hannahrittich" => printer <<< s"#@ notify_user = rittich@math.uni-wuppertal.de"
          case _ => // no user -> no notifications
        }
        printer <<< s"#@ job_type = bluegene"
        printer <<< s"#@ bg_size = $numNodes"
        printer <<< s"#@ bg_connectivity = TORUS"
        printer <<< s"#@ wall_clock_limit = 00:30:00"
        printer <<< s"#@ queue"
        printer <<< ""

        // TODO: tune the next 4 parameters
        val srcFolder = "$HOME/Exa" + (if ("" != Settings.configName) "/Generated_" + Settings.configName else "")
        val srcBinary = Settings.binary
        val destFolder = "$WORK/ExaTemp"
        val destBinary = Settings.binary + (if ("" != Settings.configName) "_" + Settings.configName else "")
        printer <<< s"mkdir $destFolder # make sure temp folder exists"
        printer <<< s"cp $srcFolder/$srcBinary $destFolder/$destBinary # copy binary to temp folder"
        printer <<< s"cd $destFolder # switch to temp folder"
        printer <<< ""

        printer <<< s"export OMP_NUM_THREADS=$numOMP"
        printer <<< s"time runjob --ranks-per-node $numMPIRanksPerNode --np $numMPI --exp-env OMP_NUM_THREADS : ./$destBinary"

        printer.finish
      }
      case _ =>
    }
  }
}