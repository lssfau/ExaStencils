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
    val fields = FieldCollection.getSortedFields
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

    new FunctionStatement(UnitDatatype, s"initFieldsWithZero", ListBuffer[VariableAccess](), statements)
  }
}

case class MultiGridFunctions() extends FunctionCollection("MultiGrid/MultiGrid",
  ListBuffer("cmath", "algorithm"), // provide math functions like sin, etc. as well as commonly used functions like min/max by default
  ListBuffer("Globals/Globals.h", "Util/Vector.h", "Util/Matrix.h", "Util/TimerFunctions.h", "CommFunctions/CommFunctions.h", "Domains/DomainGenerated.h")) {

  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"
  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"
  if (Knowledge.opt_vectorize) {
    val header = Knowledge.simd_header
    if (header != null) externalDependencies += header
  }
  if (Knowledge.data_initAllFieldsWithZero)
    functions += new InitFieldsWithZero()
  if (Knowledge.opt_vectorize && Knowledge.simd_instructionSet == "NEON")
    functions += new NEONDivision()
}

case class NEONDivision() extends AbstractFunctionStatement(true) {
  override def prettyprint(out : PpStream) : Unit = {
    out << """static inline float32x4_t vdivq_f32(const float32x4_t &a, const float32x4_t &b) {
  // get an initial estimate of 1/b.
  float32x4_t reciprocal = vrecpeq_f32(b);

  // use a couple Newton-Raphson steps to refine the estimate.  Depending on your
  // application's accuracy requirements, you may be able to get away with only
  // one refinement (instead of the two used here).  Be sure to test!
  reciprocal = vmulq_f32(vrecpsq_f32(b, reciprocal), reciprocal);
  reciprocal = vmulq_f32(vrecpsq_f32(b, reciprocal), reciprocal);

  // and finally, compute a/b = a*(1/b)
  return vmulq_f32(a,reciprocal);
}"""
  }
  override def prettyprint_decl : String = "NOT VALID ; no prototype for vdivq_f32\n"
}
