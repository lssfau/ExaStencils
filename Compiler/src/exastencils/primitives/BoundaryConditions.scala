package exastencils.primitives

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.core.collectors._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.omp._
import exastencils.polyhedron._

case class HandleBoundaries(var field : Field, var neighbors : ListBuffer[(NeighborInfo, IndexRange)]) extends Statement with Expandable {
  def cpp : String = { return "NOT VALID ; CLASS = HandleBoundaries\n" }

  override def expand : Statement = {
    if (field.dirichletBC.isDefined) {
      new LoopOverFragments(field.domain,
        neighbors.map(neigh =>
          new ConditionStatement(new getNeighInfo_IsInvalid(neigh._1, field.domain),
            new LoopOverDimensions(neigh._2,
              ListBuffer[Statement](
                new InitGeomCoords(field), // FIXME: only add if really required
                new AssignmentStatement(
                  new DirectFieldAccess("curFragment.", field, "slot", DefaultLoopMultiIndex()),
                  field.dirichletBC.get))) with OMP_PotentiallyParallel with PolyhedronAccessable) : Statement)) with OMP_PotentiallyParallel
    } else {
      new NullStatement
    }
  }
}
