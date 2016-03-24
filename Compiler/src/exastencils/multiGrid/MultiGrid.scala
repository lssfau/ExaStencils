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
  override def name = "initFieldsWithZero"

  override def expand : Output[FunctionStatement] = {
    val fields = FieldCollection.getSortedFields
    var statements : ListBuffer[Statement] = new ListBuffer

    for (field <- fields) {
      val numDims = field.fieldLayout.numDimsData
      var index = LoopOverDimensions.defIt(numDims)

      val loopOverDims = new LoopOverDimensions(numDims, new IndexRange(
        new MultiIndex((0 until numDims).toArray.map(dim => field.fieldLayout.idxById("GLB", dim))),
        new MultiIndex((0 until numDims).toArray.map(dim => field.fieldLayout.idxById("GRE", dim)))),
        (0 until field.numSlots).to[ListBuffer].map(slot =>
          new AssignmentStatement(
            new DirectFieldAccess(FieldSelection(field, field.level, slot), index),
            0.0) : Statement)) with OMP_PotentiallyParallel with PolyhedronAccessible
      loopOverDims.optLevel = 1

      val wrapped = new LoopOverFragments(
        new ConditionStatement(iv.IsValidForSubdomain(field.domain.index), loopOverDims)) with OMP_PotentiallyParallel

      if ("MSVC" == Platform.targetCompiler /*&& Platform.targetCompilerVersion <= 11*/ ) // fix for https://support.microsoft.com/en-us/kb/315481
        statements += new Scope(wrapped)
      else
        statements += wrapped
    }

    new FunctionStatement(UnitDatatype, name, ListBuffer[VariableAccess](), statements)
  }
}

case class MultiGridFunctions() extends FunctionCollection("MultiGrid/MultiGrid",
  ListBuffer("cmath", "algorithm"), // provide math functions like sin, etc. as well as commonly used functions like min/max by default
  ListBuffer("Globals/Globals.h", "Util/Vector.h", "Util/Matrix.h", "Util/TimerFunctions.h", "CommFunctions/CommFunctions.h", "Domains/DomainGenerated.h")) {

  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"
  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"
  if (Knowledge.experimental_cuda_enabled) {
    externalDependencies += "cuda.h"
    externalDependencies += "cuda_runtime.h"

    internalDependencies += "KernelFunctions/KernelFunctions.h"
  }
  if (Knowledge.opt_vectorize) {
    val header = Platform.simd_header
    if (header != null) externalDependencies += header
  }
  if (Knowledge.data_initAllFieldsWithZero)
    functions += new InitFieldsWithZero()
  if (Knowledge.opt_vectorize && Platform.simd_instructionSet == "NEON")
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
  override def name = "vdivq_f32"
}
