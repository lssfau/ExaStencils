package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ExpressionStatement
import exastencils.base.ir.IR_ForLoop
import exastencils.base.ir.IR_MemberFunctionCallArrow
import exastencils.base.ir.IR_Neq
import exastencils.base.ir.IR_PreIncrement
import exastencils.base.ir.IR_ScopedStatement
import exastencils.base.ir.IR_SpecialDatatype
import exastencils.base.ir.IR_SpecialExpandable
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.datastructures.Transformation.Output
import exastencils.field.ir.IR_FieldAccess
import exastencils.parallelization.ir.IR_HasParallelizationInfo
import exastencils.parallelization.ir.IR_ParallelizationInfo
import exastencils.util.ir.IR_CollectFieldAccesses
import exastencils.waLBerla.ir.IR_WaLBerlaSweep.getBlocks

///

// iterates through process-local blocks
case class IR_WaLBerlaLoopOverBlocks(
    var body : ListBuffer[IR_Statement],
    var parallelization : IR_ParallelizationInfo = IR_ParallelizationInfo()) extends IR_ScopedStatement with IR_SpecialExpandable with IR_HasParallelizationInfo {

  def expandSpecial() : Output[IR_ForLoop] = {
    // TODO: separate omp and potentiallyParallel
    parallelization.potentiallyParallel = Knowledge.omp_enabled && Knowledge.omp_parallelizeLoopOverFragments && parallelization.potentiallyParallel

    def defIt = IR_VariableAccess("block", IR_SpecialDatatype("auto"))

    // collect fields accessed in loop
    val fieldAccesses = ListBuffer[IR_FieldAccess]()
    IR_CollectFieldAccesses.applyStandalone(body)
    fieldAccesses ++= Duplicate(IR_CollectFieldAccesses.fieldAccesses).filter(IR_WaLBerlaFieldCollection.contains).groupBy(_.name).map(_._2.head)

    new IR_ForLoop(
      IR_VariableDeclaration(defIt, IR_MemberFunctionCallArrow(getBlocks, "begin", defIt.datatype)),
      IR_Neq(defIt, IR_MemberFunctionCallArrow(getBlocks, "end", defIt.datatype)),
      IR_ExpressionStatement(IR_PreIncrement(defIt)),
      IR_WaLBerlaUtil.getFields(fieldAccesses) ++ body,
      parallelization)
  }
}

object IR_WaLBerlaResolveLoopOverBlocks extends DefaultStrategy("Resolve waLBerla LoopOverBlocks") {
  this += Transformation("Resolve", {
    case loop : IR_WaLBerlaLoopOverBlocks => loop.expandSpecial()
  })
}
