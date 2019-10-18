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
        printer <<< s"#SBATCH --ntasks-per-core=2" // activate hyper-threading by default
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

        // some optimization flags
        printer <<< s"export MPICH_G2G_PIPELINE=256     # adapt maximum number of concurrent in-flight messages"
        printer <<< ""

        if (Knowledge.omp_enabled) {
          printer <<< s"export OMP_NUM_THREADS=$numOMP          # set number of OMP threads"
          printer <<< s"export OMP_WAIT_POLICY=PASSIVE"
        }
        if (debug)
          printer <<< s"export CRAY_OMP_CHECK_AFFINITY=TRUE"

        printer <<< ""

        printer <<< s"cd ${ Settings.executionPath }"

        printer <<< ""

        // rank reordering for multiple mpi ranks; don't apply when condition is enabled since it usually uses the mpi rank to branch
        if (numMPI > 1 && Knowledge.cuda_preferredExecution.toLowerCase != "condition") {
          val grid = Knowledge.domain_rect_numBlocksAsVec.take(Knowledge.dimensionality)
          val cores = Array.fill(Knowledge.dimensionality)(1)
          var numCores = tasksPerNode

          def multCoresWith(factor : Int) = {
            val sizes = (0 until Knowledge.dimensionality).map(d => Knowledge.domain_fragmentLengthAsVec(d) * Knowledge.domain_rect_numFragsPerBlockAsVec(d) * cores(d))
            val smallestIndex = sizes.zipWithIndex.minBy(_._1)._2
            cores(smallestIndex) *= factor
            numCores /= factor
          }

          while (numCores > 1 && 0 == numCores % 2)
            multCoresWith(2)

          if (numCores > 1) {
            Logger.warn(s"Number of blocks per node not power of two; adding remainder of $numCores to a suitable dimension")
            multCoresWith(numCores)
          }

          printer <<< s"# set up rank ordering on regular grid"
          // reverse arrays to match dimension ordering of grid_order
          printer <<< s"grid_order -R -H -g ${ grid.reverse.mkString("x") } -c ${ cores.reverse.mkString("x") } > MPICH_RANK_ORDER"
          printer <<< s"export MPICH_RANK_REORDER_METHOD=3"
        }

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

      case "juwels" =>
        val filename = "run"
        val debug = false

        Logger.dbg(s"Generating job script for JUWELS with filename $filename")
        val printer = PrettyprintingManager.getPrinter(filename)

        def numMPI = Knowledge.mpi_numThreads
        def numOMP = Knowledge.omp_numThreads
        def tasksPerNode = Knowledge.mpi_numThreads / Platform.hw_numNodes

        printer <<< s"#!/bin/bash -l"
        printer <<< s"#SBATCH --account=her16"

        printer <<< s"#SBATCH --job-name=${ Settings.configName }"

        printer <<< s"#SBATCH --mail-type=ALL"
        printer <<< s"#SBATCH --mail-user=${ resolveUserMail() }"

        printer <<< s"#SBATCH --nodes=${ Platform.hw_numNodes }"
        printer <<< s"#SBATCH --ntasks-per-core=2" // activate hyperthreading by default
        printer <<< s"#SBATCH --ntasks-per-node=$tasksPerNode"
        printer <<< s"#SBATCH --cpus-per-task=${ Platform.hw_cpu_numHWThreads / tasksPerNode }"

        printer <<< s"#SBATCH --time=00:10:00"

        printer <<< s"#SBATCH --partition=batch"

        printer <<< ""

        printer <<< s"module load Intel"
        printer <<< s"module load ParaStationMPI"

        printer <<< ""

        if (Knowledge.omp_enabled)
          printer <<< s"export OMP_NUM_THREADS=$numOMP          # set number of OMP threads"

        printer <<< ""

        printer <<< s"cd ${ Settings.executionPath }"

        printer <<< ""

        printer << s"srun "
        if (debug) printer << "-v "
        printer << s"-n $numMPI --ntasks-per-node=$tasksPerNode "
        if (Platform.omp_pinning.nonEmpty)
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

      case other => Logger.warn(s"Unknown target machine $other, no job script is generated")
    }
  }
}
