package exastencils.prettyprinting

import exastencils.config._
import exastencils.logger.Logger

object JobScriptGenerator {
  def resolveUserMail() : String = {
    Settings.user.toLowerCase() match {
      case "sebastian" | "kuckuk" | "sebastiankuckuk"              => "sebastian.kuckuk@fau.de"
      case "christian" | "schmitt" | "christianschmitt"            => "christian.schmitt@cs.fau.de"
      case "stefan" | "kronawitter" | "stefankronawitter"          => "kronast@fim.uni-passau.de"
      case "alex" | "alexander" | "grebhahn" | "alexandergrebhahn" => "grebhahn@fim.uni-passau.de"
      case "hannah" | "rittich" | "hannahrittich"                  => "rittich@math.uni-wuppertal.de"
      case _                                                       => "" // no user -> no notifications
    }
  }

  def write() : Unit = {
    Platform.targetName.toLowerCase() match {
      case "piz_daint" | "pizdaint" =>
        val filename = "run"
        val debug = false

        Logger.dbg(s"Generating job script for PizDaint with filename $filename")
        val printer = PrettyprintingManager.getPrinter(filename)

        def numMPI = Knowledge.mpi_numThreads
        def numOMP = Knowledge.omp_numThreads
        def tasksPerNode = Knowledge.mpi_numThreads / Platform.hw_numNodes

        printer <<< s"#!/bin/bash -l"

        printer <<< s"#SBATCH --job-name=${ Settings.configName }"

        printer <<< s"#SBATCH --mail-type=ALL"
        printer <<< s"#SBATCH --mail-user=${ resolveUserMail() }"

        printer <<< s"#SBATCH --nodes=${ Platform.hw_numNodes }"
        printer <<< s"#SBATCH --ntasks-per-core=2" // activate hyperthreading by default
        printer <<< s"#SBATCH --ntasks-per-node=$tasksPerNode"
        printer <<< s"#SBATCH --cpus-per-task=${ Platform.hw_cpu_numHWThreads / tasksPerNode }"

        printer <<< s"#SBATCH --constraint=gpu"

        printer <<< s"#SBATCH --time=00:10:00"

        printer <<< s"#SBATCH --switches=1"

        printer <<< ""

        printer <<< s"module load daint-gpu"
        printer <<< s"module load craype-accel-nvidia60"

        printer <<< ""

        // if (numMPI > 1) // for some reason this flag is also required if only one mpi rank is used
        printer <<< s"export MPICH_RDMA_ENABLED_CUDA=1  # allow GPU-GPU data transfer"

        // if (tasksPerNode > 1) // for some reason this flag is also required if only one thread per node is used
        printer <<< s"export CRAY_CUDA_MPS=1            # allow GPU sharing"

        if (Knowledge.omp_enabled) {
          printer <<< s"export OMP_NUM_THREADS=$numOMP          # set number of OMP threads"
          printer <<< s"export OMP_WAIT_POLICY=PASSIVE"
        }
        if (debug)
          printer <<< s"export CRAY_OMP_CHECK_AFFINITY=TRUE"

        printer <<< ""

        printer <<< s"cd ${ Settings.executionPath }"

        printer <<< ""

        // large job adaptation according to https://user.cscs.ch/getting_started/running_jobs/#large-jobs
        printer <<< "export PMI_MMAP_SYNC_WAIT_TIME=300"

        printer << s"srun "
        if (debug) printer << "-v "
        printer << s"--wait 180 --bcast=/tmp/${ Settings.configName } "
        printer << s"-n $numMPI --ntasks-per-node=$tasksPerNode "
        if ("Condition" == Knowledge.cuda_preferredExecution && Platform.omp_pinning.nonEmpty)
          printer << s"--cpu_bind=${ Platform.omp_pinning } "
        printer <<< s"./${ Settings.binary }"

        printer.finish()

      case "tsubame" | "tsubame3" =>
        val filename = "run"

        Logger.dbg(s"Generating job script for Tsubame with filename $filename")
        val printer = PrettyprintingManager.getPrinter(filename)

        def numMPI = Knowledge.mpi_numThreads
        def numOMP = Knowledge.omp_numThreads
        def tasksPerNode = Knowledge.mpi_numThreads / Platform.hw_numNodes

        printer <<< s"#!/bin/sh"

        // use current work directory to ensure correct location of output files
        printer <<< s"#$$-cwd"
        printer <<< s"#$$ -l f_node=${ Platform.hw_numNodes }"
        printer <<< s"#$$ -l h_rt=0:02:30"
        printer <<< s"#$$ -N generated_${ Settings.configName }"
        val userMail = resolveUserMail()
        if (userMail.nonEmpty) {
          printer <<< s"#$$ -m abe"
          printer <<< s"#$$ -M $userMail"
        }

        printer <<< ""

        if (Knowledge.cuda_enabled) {
          printer <<< s"## cuda path propagation"
          printer <<< s"#$$ -v LD_LIBRARY_PATH=/apps/t3/sles12sp2/cuda/8.0/lib64"
          printer <<< ""
        }

        // load modules
        printer <<< s". /etc/profile.d/modules.sh"

        // on this machine, mpi depends on cuda
        if (Knowledge.cuda_enabled || Knowledge.mpi_enabled)
          printer <<< s"module load cuda"

        if ("ICC" == Platform.targetCompiler)
          printer <<< s"module load intel"

        Platform.mpi_variant.toLowerCase() match {
          case "openmpi"                          => printer <<< s"module load openmpi"
          case "intelmpi" | "intel-mpi" | "intel" => printer <<< s"module load intel-mpi"
          case other                              => Logger.error(s"Unsupported mpi variant $other")
        }

        printer <<< ""
        printer <<< s"cd ${ Settings.executionPath }"

        printer <<< ""
        printer <<< s"make -j 56"

        if (Knowledge.omp_enabled)
          printer <<< s"export OMP_NUM_THREADS=$numOMP"

        if (!Knowledge.mpi_enabled)
          printer <<< s"./exastencils"
        else
          Platform.mpi_variant.toLowerCase() match {
            case "openmpi"                          => printer <<< s"mpirun -n $numMPI -npernode $tasksPerNode ./exastencils"
            case "intelmpi" | "intel-mpi" | "intel" => printer <<< s"mpiexec.hydra -n $numMPI -ppn $tasksPerNode ./exastencils"
            case other                              => Logger.error(s"Unsupported mpi variant $other")
          }

        printer <<< ""
        printer <<< s"echo done"

        printer <<< ""

        printer.finish()

      case _ => write_deprecated()
    }
  }

  def write_deprecated() : Unit = {
    Platform.targetCompiler match {
      case "IBMBG" | "IBMXL" =>
        val numOMP = Knowledge.omp_numThreads
        val numMPI = Knowledge.mpi_numThreads
        val numThreadsPerNode = Platform.hw_numThreadsPerNode
        val numMPIRanksPerNode = numThreadsPerNode / numOMP
        val numNodes = (numOMP * numMPI) / numThreadsPerNode

        val printer = PrettyprintingManager.getPrinter("runJuQueen")
        printer <<< s"#@ shell = /bin/bash"
        //printer <<< s"#@ job_name = GENERATED_$numNodes"
        printer <<< s"#@ job_name = ${ Settings.configName }"
        printer <<< "#@ error = $(job_name).$(jobid).out"
        printer <<< "#@ output = $(job_name).$(jobid).out"
        printer <<< s"#@ environment = COPY_ALL"
        val notify_user : String = resolveUserMail()
        if (notify_user.nonEmpty) {
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
        val srcFolder = "$HOME/Exa" + (if ("" != Settings.configName) "/" + Settings.configName else "")
        val srcBinary = Settings.binary
        val destFolder = "$WORK/ExaTemp"
        val destBinary = Settings.binary + (if ("" != Settings.configName) "_" + Settings.configName else "")
        printer <<< s"mkdir -p $destFolder # make sure temp folder exists"
        printer <<< s"cp $srcFolder/$srcBinary $destFolder/$destBinary # copy binary to temp folder"
        printer <<< s"cd $destFolder # switch to temp folder"
        printer <<< ""

        printer <<< s"export OMP_NUM_THREADS=$numOMP"
        printer <<< s"time runjob --ranks-per-node $numMPIRanksPerNode --np $numMPI --exp-env OMP_NUM_THREADS : ./$destBinary"

        printer.finish()

      case "ICC" | "ICPC" => // emmy - FIXME: switch according to more appropriate metrics
        val numOMP = Knowledge.omp_numThreads
        val numMPI = Knowledge.mpi_numThreads
        val numThreadsPerNode = Platform.hw_numThreadsPerNode
        val numMPIRanksPerNode = numThreadsPerNode / numOMP
        val numNodes = 2 // FIXME: (numOMP * numMPI) / numThreadsPerNode

        val printer = PrettyprintingManager.getPrinter("runTest")
        printer <<< s"#!/bin/bash -l"

        printer <<< s"#PBS -N ${ Settings.configName }"
        printer <<< s"#PBS -l nodes=$numNodes:ppn=40"
        printer <<< s"#PBS -l walltime=01:00:00"
        printer <<< s"#PBS -q route"
        var notify_user : String = null
        Settings.user.toLowerCase() match {
          case "sebastian" | "kuckuk" | "sebastiankuckuk"              => notify_user = "sebastian.kuckuk@fau.de"
          case "christian" | "schmitt" | "christianschmitt"            => notify_user = "christian.schmitt@cs.fau.de"
          case "stefan" | "kronawitter" | "stefankronawitter"          => notify_user = "kronast@fim.uni-passau.de"
          case "alex" | "alexander" | "grebhahn" | "alexandergrebhahn" => notify_user = "grebhahn@fim.uni-passau.de"
          case "hannah" | "rittich" | "hannahrittich"                  => notify_user = "rittich@math.uni-wuppertal.de"
          case _                                                       => // no user -> no notifications
        }
        if (notify_user != null)
          printer <<< s"#PBS -M $notify_user -m abe"

        printer <<< ""

        printer <<< s"#. /etc/profile.d/modules.sh"
        printer <<< s"module load intel64/15.0up05"
        printer <<< s"module load likwid"

        printer <<< ""

        val srcFolder = "$HOME/generated" + (if ("" != Settings.configName) "/" + Settings.configName else "")
        val srcBinary = Settings.binary
        printer <<< s"cd $srcFolder # switch to temp folder"

        printer <<< ""

        // FIXME: adapt for mpi/omp
        printer <<< s"time make -j && time likwid-pin -S -c S0:0-19 ./$srcBinary"

        printer.finish()

      case _ =>
    }
  }

  def write(numMPI : Int, numOMP : Int, ranksPerNode : Int, sourcePath : Array[String], number : Int, suffix : String) : Unit = {
    Platform.targetCompiler match {
      case "IBMBG" | "IBMXL" =>
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
          case "sebastian" | "kuckuk" | "sebastiankuckuk"              => printer <<< s"#@ notify_user = sebastian.kuckuk@fau.de"
          case "christian" | "schmitt" | "christianschmitt"            => printer <<< s"#@ notify_user = christian.schmitt@cs.fau.de"
          case "stefan" | "kronawitter" | "stefankronawitter"          => printer <<< s"#@ notify_user = kronast@fim.uni-passau.de"
          case "alex" | "alexander" | "grebhahn" | "alexandergrebhahn" => printer <<< s"#@ notify_user = grebhahn@fim.uni-passau.de"
          case "hannah" | "rittich" | "hannahrittich"                  => printer <<< s"#@ notify_user = rittich@math.uni-wuppertal.de"
          case _                                                       => // no user -> no notifications
        }
        printer <<< s"#@ job_type = bluegene"
        printer <<< s"#@ bg_size = $numNodes"
        printer <<< s"#@ bg_connectivity = TORUS"
        if (numNodes > 64) {
          var hours : Int = (sourcePath.length * 3) / 60
          var minutes : Int = (sourcePath.length * 3) % 60
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

        for (a <- sourcePath.indices) {
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

        printer.finish()

      case _ =>
    }
  }
}
