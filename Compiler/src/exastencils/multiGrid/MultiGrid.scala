package exastencils.multiGrid

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.omp._
import exastencils.polyhedron._

case class InitFieldsWithZero() extends AbstractFunctionStatement with Expandable {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = InitFieldsWithZero\n"
  override def cpp_decl : String = cpp

  def expand() : Output[FunctionStatement] = {
    val fields = FieldCollection.fields
    var statements : ListBuffer[Statement] = new ListBuffer

    for (field <- fields) {
      statements += new LoopOverFragments(
        new ConditionStatement(iv.IsValidForSubdomain(field.domain.index),
          new LoopOverDimensions(Knowledge.dimensionality + 1, new IndexRange(
            new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => field.layout(i).idxGhostLeftBegin)),
            new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => field.layout(i).idxGhostRightEnd))),
            (0 until field.numSlots).to[ListBuffer].map(slot =>
              new AssignmentStatement(
                new DirectFieldAccess(FieldSelection(field, field.level, slot, -1), LoopOverDimensions.defIt),
                0.0) : Statement)) with OMP_PotentiallyParallel with PolyhedronAccessable)) with OMP_PotentiallyParallel
    }

    new FunctionStatement(new UnitDatatype, s"initFieldsWithZero", ListBuffer[VariableAccess](), statements)
  }
}

case class MultiGridFunctions() extends FunctionCollection("MultiGrid/MultiGrid",
  (if (Knowledge.useMPI) ListBuffer("mpi.h") else ListBuffer())
    ++ (if (Knowledge.opt_vectorize) ListBuffer("immintrin.h") else ListBuffer())
    ++ (if (Knowledge.poly_usePolyOpt) ListBuffer("algorithm") else ListBuffer()),
  ListBuffer("Globals/Globals.h", "Util/Vector.h", "Util/Stopwatch.h", "CommFunctions/CommFunctions.h", "Domains/DomainGenerated.h")) {

  if (Knowledge.data_initAllFieldsWithZero)
    functions += new InitFieldsWithZero
}
