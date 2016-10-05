package exastencils.omp

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.omp.ir.OMP_Clause

@deprecated("to be integrated with loop annotations/ loop member holding optimization and parallelization information", "15.09.2016")
trait OMP_PotentiallyParallel {
  var reduction : Option[IR_Reduction];
  var additionalOMPClauses = new ListBuffer[OMP_Clause];
  var collapse = 1
}

