package exastencils.primitives

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.core.collectors._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.omp._
import exastencils.knowledge._

case class HandleBoundaries(var field : Field, var neighbors : ListBuffer[(NeighborInfo, IndexRange)]) extends Statement with Expandable {
  def cpp : String = { return "NOT VALID ; CLASS = HandleBoundaries\n"; }

  override def expand(collector : StackCollector) : Statement = {
    // TODO: match boundary conditions
    if (field.bcDir0) {
      return new LoopOverFragments(
        neighbors.map(neigh =>
          new ConditionStatement(new getNeighInfo_IsInvalid(neigh._1),
            new LoopOverDimensions(neigh._2,
              new AssignmentStatement(
                new FieldAccess(field, "slot", DefaultLoopMultiIndex()),
                0.0)) with OMP_PotentiallyParallel) : Statement)) with OMP_PotentiallyParallel;
    } else {
      return new NullStatement;
    }
  }
}
