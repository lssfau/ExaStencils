package exastencils.omp

import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.knowledge._

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
            target.addOMPStatements += target.reduction.get.getOMPClause
        new OMP_ParallelFor(new ForLoopStatement(target.begin, target.end, target.inc, target.body, target.reduction),
          target.addOMPStatements, target.collapse)
    }))

    this.commit()
  }
}
