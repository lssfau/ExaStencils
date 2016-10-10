package exastencils.omp

import exastencils.base.ir._
import exastencils.config._
import exastencils.cuda._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.omp.ir._
import exastencils.optimization.OptimizationHint

object AddOMPPragmas extends DefaultStrategy("Adding OMP pragmas") {
  this += new Transformation("Adding OMP parallel for pragmas", {
    case target : IR_ForLoop with OMP_PotentiallyParallel if !target.hasAnnotation(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION) =>
      if (target.parallelization.reduction.isDefined)
        if (!(Platform.omp_version < 3.1 && ("min" == target.parallelization.reduction.get.op || "max" == target.parallelization.reduction.get.op)))
          target.additionalOMPClauses += OMP_Reduction(target.parallelization.reduction.get)

      target match {
        case l : OptimizationHint =>
          if (l.privateVars.nonEmpty)
            target.additionalOMPClauses += OMP_Private(l.privateVars.clone())
        case _                    =>
      }
      OMP_ParallelFor(IR_ForLoop(target.begin, target.end, target.inc, target.body, target.parallelization),
        target.additionalOMPClauses, target.collapse)
  })
}
