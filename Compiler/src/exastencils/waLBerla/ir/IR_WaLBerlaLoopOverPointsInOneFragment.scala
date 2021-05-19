package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_ScopedStatement
import exastencils.base.ir.IR_SpecialExpandable
import exastencils.base.ir.IR_Statement
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverPointsInOneFragment
import exastencils.baseExt.ir.IR_RegionSpecification
import exastencils.communication.ir.IR_Communicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir.StatementList
import exastencils.parallelization.ir.IR_HasParallelizationInfo
import exastencils.parallelization.ir.IR_ParallelizationInfo

case class IR_WaLBerlaLoopOverPointsInOneFragment(
    var domain : Int,
    var wbField : IR_WaLBerlaField,
    var region : Option[IR_RegionSpecification],
    var startOffset : IR_ExpressionIndex,
    var endOffset : IR_ExpressionIndex,
    var increment : IR_ExpressionIndex,
    var body : ListBuffer[IR_Statement],
    var preComms : ListBuffer[IR_Communicate] = ListBuffer(),
    var postComms : ListBuffer[IR_Communicate] = ListBuffer(),
    var parallelization : IR_ParallelizationInfo = IR_ParallelizationInfo(),
    var condition : Option[IR_Expression] = None) extends IR_ScopedStatement with IR_SpecialExpandable with IR_HasParallelizationInfo {

  def numDims = wbField.numDimsGrid

  def expandSpecial : Output[StatementList] = {
    IR_LoopOverPointsInOneFragment(domain, wbField.field, region, startOffset, endOffset, increment, body, preComms, postComms, parallelization, condition).expandSpecial
  }
}


/// IR_WaLBerlaResolveLoopOverPointsInOneFragment

object IR_WaLBerlaResolveLoopOverPointsInOneFragment extends DefaultStrategy("Resolve LoopOverPointsInOneFragment nodes") {

  // TODO approach might not be robust enough
  var oldloopOverDimensions : ListBuffer[IR_LoopOverDimensions] = ListBuffer()
  var newloopOverDimensions : ListBuffer[IR_LoopOverDimensions] = ListBuffer()

  this += new Transformation("Collect potential older LoopOverDims", {
    case loop : IR_LoopOverDimensions =>
      oldloopOverDimensions += loop
      loop
  })

  this += new Transformation("Resolve", {
    case loop : IR_WaLBerlaLoopOverPointsInOneFragment => loop.expandSpecial
  })

  this += new Transformation("Collect produced LoopOverDims", {
    case loop : IR_LoopOverDimensions if !oldloopOverDimensions.contains(loop) =>
      newloopOverDimensions += loop
      loop
  })

  this += new Transformation("Replace produced LoopOverDims", {
    case loop @ IR_LoopOverDimensions(dims, indices, body, stepSize, parInfo, cond, genOMPThreadLoop) if newloopOverDimensions.contains(loop) =>
      IR_WaLBerlaLoopOverDimensions(dims, indices, body, stepSize, parInfo, cond, genOMPThreadLoop)
  })
}