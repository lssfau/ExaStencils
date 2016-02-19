package exastencils.omp

import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.optimization.OptimizationHint

object AddOMPPragmas extends DefaultStrategy("Adding OMP pragmas") {
  override def apply(applyAtNode : Option[Node]) = {
    this.transaction()

    if (Knowledge.omp_requiresCriticalSections) {
      this.execute(new Transformation("Adding OMP critical pragmas", {
        case target : OMP_PotentiallyCritical => target match {
          case target : Scope     => new OMP_Critical(target)
          case target : Statement => new OMP_Critical(target)
        }
      }, false))
    }

    this.execute(new Transformation("Adding OMP parallel for pragmas", {
      case target : ForLoopStatement with OMP_PotentiallyParallel =>
        if (target.reduction.isDefined)
          if (!(Knowledge.omp_version < 3.1 && ("min" == target.reduction.get.op || "max" == target.reduction.get.op)))
            target.additionalOMPClauses += new OMP_Reduction(target.reduction.get)
        target match {
          case l : OptimizationHint => target.additionalOMPClauses += new OMP_Private(l.privateVars.clone())
          case _                    =>
        }
        new OMP_ParallelFor(new ForLoopStatement(target.begin, target.end, target.inc, target.body, target.reduction),
          target.additionalOMPClauses, target.collapse)
    }))

    this.commit()
  }
}
