package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.communication.ir.IR_Communicate
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.field.ir.IR_Field
import exastencils.logger.Logger
import exastencils.parallelization.ir._
import exastencils.util.ir.IR_StackCollector

/// IR_RegionSpecification

case class IR_RegionSpecification(var region : String, var dir : IR_ConstIndex, var onlyOnBoundary : Boolean) extends IR_Node

/// IR_LoopOverPoints

object IR_LoopOverPoints {
  def apply(field : IR_Field, body : ListBuffer[IR_Statement]) =
    new IR_LoopOverPoints(field, None, IR_ExpressionIndex(Array.fill(field.fieldLayout.numDimsGrid)(0)),
      IR_ExpressionIndex(Array.fill(field.fieldLayout.numDimsGrid)(0)),
      IR_ExpressionIndex(Array.fill(field.fieldLayout.numDimsGrid)(1)),
      body)

  def apply(field : IR_Field, body : IR_Statement*) : IR_LoopOverPoints = apply(field, body.to[ListBuffer])
}

case class IR_LoopOverPoints(
    var field : IR_Field,
    var region : Option[IR_RegionSpecification],
    var startOffset : IR_ExpressionIndex,
    var endOffset : IR_ExpressionIndex,
    var increment : IR_ExpressionIndex,
    var body : ListBuffer[IR_Statement],
    var preComms : ListBuffer[IR_Communicate] = ListBuffer(),
    var postComms : ListBuffer[IR_Communicate] = ListBuffer(),
    var parallelization : IR_ParallelizationInfo = IR_ParallelizationInfo(),
    var condition : Option[IR_Expression] = None) extends IR_Statement with IR_SpecialExpandable with IR_HasParallelizationInfo {

  def expandSpecial(collector : IR_StackCollector) : Output[StatementList] = {
    val insideFragLoop = collector.stack.exists(_.isInstanceOf[IR_LoopOverFragments])
    val innerLoop =
      if (Knowledge.experimental_splitLoopsForAsyncComm)
        IR_LoopOverPointsInOneFragment(field.domain.index, field, region, startOffset, endOffset, increment, body, preComms, postComms, Duplicate(parallelization), condition)
      else
        IR_LoopOverPointsInOneFragment(field.domain.index, field, region, startOffset, endOffset, increment, body, ListBuffer(), ListBuffer(), Duplicate(parallelization), condition)

    var stmts = ListBuffer[IR_Statement]()
    stmts += innerLoop

    if (!insideFragLoop)
      stmts = ListBuffer(IR_LoopOverFragments(stmts, Duplicate(parallelization)))

    if (Knowledge.experimental_splitLoopsForAsyncComm)
      stmts
    else {
      if (preComms.nonEmpty) Logger.warn("Found precomm")
      preComms ++ stmts ++ postComms
    }
  }
}

/// IR_ResolveLoopOverPoints

object IR_ResolveLoopOverPoints extends DefaultStrategy("Resolve LoopOverPoints nodes") {
  val collector = new IR_StackCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve", {
    case loop : IR_LoopOverPoints => loop.expandSpecial(collector)
  })
}
