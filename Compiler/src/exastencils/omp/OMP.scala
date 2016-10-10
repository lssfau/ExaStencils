package exastencils.omp

import scala.collection.mutable.ListBuffer

import exastencils.omp.ir.OMP_Clause
import exastencils.parallelization.ir.IR_ParallelizationInfo

@deprecated("to be integrated with loop annotations/ loop member holding optimization and parallelization information", "15.09.2016")
trait OMP_PotentiallyParallel {
  var parallelization : IR_ParallelizationInfo
  var additionalOMPClauses = new ListBuffer[OMP_Clause]
  var collapse = 1
}

