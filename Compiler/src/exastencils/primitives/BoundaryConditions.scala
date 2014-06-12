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
      var statements : ListBuffer[Statement] = ListBuffer()

      statements += new InitGeomCoords(field) // FIXME: only add if really required
      for (vecDim <- 0 until field.vectorSize) { // FIXME: this works for now, but users may want to specify bc's per vector element
        var index = DefaultLoopMultiIndex()
        index(Knowledge.dimensionality) = vecDim
        statements += new AssignmentStatement(
          new DirectFieldAccess(FieldSelection("curFragment.", field, "slot", vecDim), index),
          field.dirichletBC.get)
      }

      new LoopOverFragments(field.domain.index,
        neighbors.map(neigh =>
          new ConditionStatement(new getNeighInfo_IsInvalid(neigh._1, field.domain.index),
            new LoopOverDimensions(Knowledge.dimensionality, neigh._2, statements) with OMP_PotentiallyParallel with PolyhedronAccessable) : Statement)) with OMP_PotentiallyParallel
    } else {
      new NullStatement
    }
  }
}
