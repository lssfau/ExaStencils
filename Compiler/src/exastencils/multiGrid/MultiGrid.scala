package exastencils.multiGrid

import java.io.PrintWriter
import java.io.File
import scala.collection.mutable.ListBuffer
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.multiGrid._
import exastencils.prettyprinting._
import exastencils.omp._
import exastencils.polyhedron._

case class InitFieldsWithZero() extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = InitFieldsWithZero\n"
  override def cpp_decl : String = cpp

  def expand() : FunctionStatement = {
    val fields = FieldCollection.fields
    var statements : ListBuffer[Statement] = new ListBuffer

    for (field <- fields) {
      statements += new LoopOverFragments(field.domain.index,
        new LoopOverDimensions(Knowledge.dimensionality + 1, new IndexRange(
          new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => field.layout(i).idxGhostLeftBegin)),
          new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => field.layout(i).idxGhostRightEnd))),
          (0 until field.numSlots).to[ListBuffer].map(slot =>
            new AssignmentStatement(
              new DirectFieldAccess(FieldSelection(field, slot, -1), LoopOverDimensions.defIt),
              0.0) : Statement)) with OMP_PotentiallyParallel with PolyhedronAccessable) with OMP_PotentiallyParallel
    }

    new FunctionStatement(new UnitDatatype, s"initFieldsWithZero", ListBuffer[VariableAccess](), statements)
  }
}

case class MultiGridFunctions() extends FunctionCollection("MultiGrid/MultiGrid",
  ListBuffer("#include \"Globals/Globals.h\"") ++
  (if (Knowledge.useMPI)
    ListBuffer("#pragma warning(disable : 4800)", "#include <mpi.h>")
  else
    ListBuffer())
    ++
    (if (Knowledge.opt_vectorize)
      ListBuffer("#include <immintrin.h>")
    else
      ListBuffer())
    ++
    (if (Knowledge.poly_usePolyOpt)
      ListBuffer("#include <algorithm>")
    else
      ListBuffer())
    ++
    ListBuffer(
      "#include \"Util/Log.h\"",
      "#include \"Util/Vector.h\"",
      "#include \"Util/Stopwatch.h\"",
      "#include \"Primitives/CommunicationFunctions.h\"",
      "#include \"Domains/DomainGenerated.h\"")) {

  if (Knowledge.data_initAllFieldsWithZero)
    functions += new InitFieldsWithZero
}
