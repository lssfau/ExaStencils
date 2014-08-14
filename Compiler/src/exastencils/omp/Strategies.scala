package exastencils.omp

import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._

object AddOMPPragmas extends DefaultStrategy("Adding OMP pragmas") {
  if (Knowledge.omp_requiresCriticalSections)
    this += new Transformation("Adding OMP critical pragmas", {
      case target : OMP_PotentiallyCritical => target match {
        case target : Scope     => new OMP_Critical(target)
        case target : Statement => new OMP_Critical(target)
      }
    }, false)

  this += new Transformation("Adding OMP parallel for pragmas", {
    case target : ForLoopStatement with OMP_PotentiallyParallel =>
      new OMP_ParallelFor(new ForLoopStatement(target.begin, target.end, target.inc, target.body, target.reduction),
        (if (target.reduction.isDefined) target.reduction.get.getOMPClause else NullExpression), target.collapse)
  })
}