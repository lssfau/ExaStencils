package exastencils.multiGrid

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.omp._
import exastencils.polyhedron._

case class HandleBoundaries(var field : FieldSelection, var neighbors : ListBuffer[(NeighborInfo, IndexRange)]) extends Statement with Expandable {
  def cpp : String = { return "NOT VALID ; CLASS = HandleBoundaries\n" }

  def setupDirichlet : ListBuffer[Statement] = {
    var statements : ListBuffer[Statement] = ListBuffer()

    statements += new InitGeomCoords(field.field, true) // FIXME: only add if really required
    for (vecDim <- 0 until field.field.vectorSize) { // FIXME: this works for now, but users may want to specify bc's per vector element
      var index = LoopOverDimensions.defIt
      index(Knowledge.dimensionality) = vecDim
      var fieldSel = new FieldSelection(field.field, field.slot, vecDim, field.fragIdx)
      statements += new AssignmentStatement(new DirectFieldAccess(fieldSel, index), Duplicate(field.field.dirichletBC.get))
    }

    statements
  }

  override def expand : Output[Statement] = {
    if (field.field.dirichletBC.isDefined) {
      new LoopOverFragments(field.domainIndex,
        neighbors.map(neigh =>
          new ConditionStatement(UnaryExpression(UnaryOperators.Not, iv.NeighborIsValid(field.domainIndex, neigh._1.index)),
            new LoopOverDimensions(Knowledge.dimensionality, neigh._2, setupDirichlet) with OMP_PotentiallyParallel with PolyhedronAccessable) : Statement)) with OMP_PotentiallyParallel
    } else {
      new NullStatement
    }
  }
}
