package exastencils.multiGrid

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.omp._
import exastencils.polyhedron._
import exastencils.prettyprinting._

case class InitFieldsWithZero() extends AbstractFunctionStatement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = InitFieldsWithZero\n"
  override def prettyprint_decl : String = prettyprint

  def expand() : Output[FunctionStatement] = {
    val fields = FieldCollection.fields
    var statements : ListBuffer[Statement] = new ListBuffer

    for (field <- fields) {
      val loopOverDims = new LoopOverDimensions(Knowledge.dimensionality + 1, new IndexRange(
        new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => field.fieldLayout(i).idxGhostLeftBegin)),
        new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => field.fieldLayout(i).idxGhostRightEnd))),
        (0 until field.numSlots).to[ListBuffer].map(slot =>
          new AssignmentStatement(
            new DirectFieldAccess(FieldSelection(field, field.level, slot), LoopOverDimensions.defIt),
            0.0) : Statement)) with OMP_PotentiallyParallel with PolyhedronAccessable
      loopOverDims.optLevel = 1
      statements += new LoopOverFragments(
        new ConditionStatement(iv.IsValidForSubdomain(field.domain.index), loopOverDims)) with OMP_PotentiallyParallel
    }

    new FunctionStatement(new UnitDatatype, s"initFieldsWithZero", ListBuffer[VariableAccess](), statements)
  }
}

case class MultiGridFunctions() extends FunctionCollection("MultiGrid/MultiGrid",
  ListBuffer("cmath"),
  ListBuffer("Globals/Globals.h", "Util/Vector.h", "Util/Stopwatch.h", "CommFunctions/CommFunctions.h", "Domains/DomainGenerated.h")) {

  if (Knowledge.useMPI)
    externalDependencies += "mpi.h"
  if (Knowledge.opt_vectorize)
    externalDependencies += "immintrin.h"
  if (Knowledge.opt_vectorize || Knowledge.poly_optLevel_fine > 0)
    externalDependencies += "algorithm"
  if (Knowledge.data_initAllFieldsWithZero)
    functions += new InitFieldsWithZero
}
