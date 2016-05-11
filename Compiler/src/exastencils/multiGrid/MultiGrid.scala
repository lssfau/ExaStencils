package exastencils.multiGrid

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import exastencils.core.StateManager
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.omp.OMP_PotentiallyParallel
import exastencils.performance._
import exastencils.polyhedron.PolyhedronAccessible
import exastencils.prettyprinting.PpStream

case class InitFieldsWithZero() extends AbstractFunctionStatement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = InitFieldsWithZero\n"
  override def prettyprint_decl() : String = prettyprint
  override def name = "initFieldsWithZero"

  override def expand() : Output[FunctionStatement] = {
    val fields = FieldCollection.getSortedFields
    var statements : ListBuffer[Statement] = new ListBuffer

    for (field <- fields) {
      val numDims = field.fieldLayout.numDimsData
      val index = LoopOverDimensions.defIt(numDims)

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
    if (header != null)
      externalDependencies += header
    if (Platform.simd_instructionSet == "NEON")
      functions += NEONDivision
    val mathLibHeader = Platform.simd_mathLibHeader
    if (mathLibHeader != null)
      externalDependencies ++= mathLibHeader
  }
  if (Knowledge.data_initAllFieldsWithZero)
    functions += new InitFieldsWithZero()
}

object SIMD_MathFunctions {

  private val numberArgs = Map("exp" -> 1, "exp2" -> 1, "exp10" -> 1, "log" -> 1, "log10" -> 1, "ldexp" -> 2, "pow" -> 2, "sqrt" -> 1,
    "sin" -> 1, "cos" -> 1, "tan" -> 1, "asin" -> 1, "acos" -> 1, "atan" -> 1, "sinh" -> 1, "cosh" -> 1, "tanh" -> 1, "atan2" -> 2)

  private val functionNameMapping = new HashMap[String, String]()
  private lazy val multigridCollection = StateManager.findFirst[MultiGridFunctions].get // there must be a MultiGridFunctions object

  def isAllowed(func : String) : Boolean = {
    return numberArgs.contains(func)
  }

  def addUsage(func : String) : String = {
    val nrArgs = numberArgs(func) // expected fail if !isAllowed(func)
    return functionNameMapping.getOrElseUpdate(func, {
      val funcStmt : AbstractFunctionStatement = new SIMD_MathFunc(func, nrArgs)
      multigridCollection.functions += funcStmt
      funcStmt.name
    })
  }
}
