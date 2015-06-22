package exastencils.prettyprinting

import exastencils.core._
import exastencils.knowledge._

object JobScriptGenerator {
  def write : Unit = {
    Knowledge.targetCompiler match {
      case "IBMBG" | "IBMXL" => {
        val numOMP = Knowledge.omp_numThreads
        val numMPI = Knowledge.mpi_numThreads
        val numThreadsPerNode = Knowledge.hw_numThreadsPerNode
        val numMPIRanksPerNode = numThreadsPerNode / numOMP
        val numNodes = (numOMP * numMPI) / numThreadsPerNode

        val printer = PrettyprintingManager.getPrinter("runJuQueen")
        printer <<< s"#@ shell = /bin/bash"
        printer <<< s"#@ job_name = GENERATED_$numNodes"
        printer <<< "#@ error = $(job_name).$(jobid).out"
        printer <<< "#@ output = $(job_name).$(jobid).out"
        printer <<< s"#@ environment = COPY_ALL"
        var notify_user : String = null
        Settings.user.toLowerCase() match {
          case "sebastian" | "kuckuk" | "sebastiankuckuk" => notify_user = "sebastian.kuckuk@fau.de"
          case "christian" | "schmitt" | "christianschmitt" => notify_user = "christian.schmitt@cs.fau.de"
          case "stefan" | "kronawitter" | "stefankronawitter" => notify_user = "kronast@fim.uni-passau.de"
          case "alex" | "alexander" | "grebhahn" | "alexandergrebhahn" => notify_user = "grebhahn@fim.uni-passau.de"
          case "hannah" | "rittich" | "hannahrittich" => notify_user = "rittich@math.uni-wuppertal.de"
          case _ => // no user -> no notifications
        }
        if (notify_user != null) {
          printer <<< s"#@ notification = always"
          printer <<< s"#@ notify_user = $notify_user"
        } else
          printer <<< s"#@ notification = never"
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
        printer <<< s"mkdir -p $destFolder # make sure temp folder exists"
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

  def write(numMPI : Int, numOMP : Int, ranksPerNode : Int, sourcePath : Array[String], number : Int, suffix : String) : Unit = {
    Knowledge.targetCompiler match {
      case "IBMBG" | "IBMXL" => {
        val numThreadsPerNode = ranksPerNode
        val numMPIRanksPerNode = numThreadsPerNode / numOMP
        val numNodes = (numOMP * numMPI) / ranksPerNode
        println("OMP: " + numOMP + " MPI: " + numMPI + "  NumNodes: " + numNodes)
        val printer = PrettyprintingManager.getPrinter("runJuQueen_" + suffix + "_" + numMPI + "_" + numOMP + "_" + ranksPerNode + "_" + number)
        printer <<< s"#@ shell = /bin/bash"
        printer <<< s"#@ job_name = GENERATED_" + numMPI + "_" + numOMP + "_" + number
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
        if (numNodes > 64) {
          var hours : Int = ((sourcePath.size * 3) / 60).toInt
          var minutes : Int = ((sourcePath.size * 3) % 60).toInt
          if (minutes < 10) {
            printer <<< s"#@ wall_clock_limit = 0" + hours + ":0" + minutes + ":00"
          } else {
            printer <<< s"#@ wall_clock_limit = 0" + hours + ":" + minutes + ":00"
          }
        } else {
          printer <<< s"#@ wall_clock_limit = 00:30:00"
        }
        printer <<< s"#@ queue"
        printer <<< ""

        for (a <- 0 to sourcePath.size - 1) {
          // list of source and destination locations for the different jobs
          // TODO: tune the next 4 parameters

          if (sourcePath(a) != null) {
            val srcFolder = "$HOME/Exa"
            val srcBinary = sourcePath(a)
            val destFolder = "$WORK/ExaTemp"
            val destBinary = sourcePath(a)
            //          printer <<< s"mkdir -p $destFolder # make sure temp folder exists"
            //          printer <<< s"cp $srcFolder/$srcBinary $destFolder/$destBinary # copy binary to temp folder"
            //          printer <<< s"cd $destFolder # switch to temp folder"
            //          printer <<< ""

            printer <<< s"export OMP_NUM_THREADS=$numOMP"
            printer <<< s"time runjob --ranks-per-node $numMPIRanksPerNode --np $numMPI --exp-env OMP_NUM_THREADS : /homea/her18/her183/Exa/$destBinary" + suffix + "/exastencils.exe"
            printer <<< ""
          }
        }

        printer.finish
      }
      case _ =>
    }
  }
}