package exastencils.omp

import exastencils.base.ir._
import exastencils.cuda._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.knowledge._
import exastencils.omp.ir._
import exastencils.optimization.OptimizationHint

object AddOMPPragmas extends DefaultStrategy("Adding OMP pragmas") {
  this += new Transformation("Adding OMP parallel for pragmas", {
    case target : IR_ForLoop with OMP_PotentiallyParallel if !target.hasAnnotation(CudaStrategiesUtils
      .CUDA_LOOP_ANNOTATION) =>
      if (target.reduction.isDefined)
        if (!(Platform.omp_version < 3.1 && ("min" == target.reduction.get.op || "max" == target.reduction.get.op)))
          target.additionalOMPClauses += OMP_Reduction(target.reduction.get)
      target match {
        case l : OptimizationHint =>
          if (l.privateVars.nonEmpty)
            target.additionalOMPClauses += OMP_Private(l.privateVars.clone())
        case _                    =>
      }
      OMP_ParallelFor(IR_ForLoop(target.begin, target.end, target.inc, target.body, target.reduction),
        target.additionalOMPClauses, target.collapse)
  })
}
