package exastencils.multiGrid

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.datastructures.Transformation._
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir._
import exastencils.knowledge._
import exastencils.omp.OMP_PotentiallyParallel
import exastencils.performance._
import exastencils.polyhedron.PolyhedronAccessible
import exastencils.prettyprinting.PpStream

case class InitFieldsWithZero() extends IR_AbstractFunction with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
  override def prettyprint_decl() : String = prettyprint
  override def name = "initFieldsWithZero"

  override def expand() : Output[IR_Function] = {
    val fields = IR_FieldCollection.sortedObjects
    var statements : ListBuffer[IR_Statement] = new ListBuffer

    for (field <- fields) {
      val numDims = field.fieldLayout.numDimsData
      val index = IR_LoopOverDimensions.defIt(numDims)

      val loopOverDims = new IR_LoopOverDimensions(numDims, new IndexRange(
        IR_ExpressionIndex((0 until numDims).toArray.map(dim => field.fieldLayout.idxById("GLB", dim))),
        IR_ExpressionIndex((0 until numDims).toArray.map(dim => field.fieldLayout.idxById("GRE", dim)))),
        (0 until field.numSlots).to[ListBuffer].map(slot =>
          IR_Assignment(
            IR_DirectFieldAccess(IR_FieldSelection(field, field.level, slot), index),
            0.0) : IR_Statement)) with OMP_PotentiallyParallel with PolyhedronAccessible
      loopOverDims.optLevel = 1

      val wrapped = new IR_LoopOverFragments(
        ListBuffer[IR_Statement](IR_IfCondition(IR_IV_IsValidForDomain(field.domain.index), loopOverDims))) with OMP_PotentiallyParallel

      if ("MSVC" == Platform.targetCompiler /*&& Platform.targetCompilerVersion <= 11*/ ) // fix for https://support.microsoft.com/en-us/kb/315481
        statements += IR_Scope(wrapped)
      else
        statements += wrapped
    }

    new IR_Function(IR_UnitDatatype, name, ListBuffer[IR_FunctionArgument](), statements)
  }
}

case class MultiGridFunctions() extends IR_FunctionCollection("MultiGrid/MultiGrid",
  ListBuffer("cmath", "algorithm"), // provide math functions like sin, etc. as well as commonly used functions like min/max by default
  ListBuffer("Globals/Globals.h", "Util/Vector.h", "Util/Matrix.h", "Util/TimerFunctions.h", "CommFunctions/CommFunctions.h", "Domains/DomainGenerated.h")) {

  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"
  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"
  if (Knowledge.cuda_enabled) {
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
