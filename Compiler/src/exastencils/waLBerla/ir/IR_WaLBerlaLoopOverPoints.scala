package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.ir.IR_Communicate
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir.StatementList
import exastencils.logger.Logger
import exastencils.parallelization.ir.IR_HasParallelizationInfo
import exastencils.parallelization.ir.IR_ParallelizationInfo
import exastencils.util.ir.IR_StackCollector

case class IR_WaLBerlaLoopOverPoints(
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


  def expandSpecial(collector : IR_StackCollector) : Output[StatementList] = {
    val insideFragLoop = collector.stack.exists(_.isInstanceOf[IR_LoopOverFragments])
    val innerLoop =
      if (Knowledge.experimental_splitLoopsForAsyncComm)
        IR_WaLBerlaLoopOverPointsInOneFragment(wbField.domain.index, wbField, region, startOffset, endOffset, increment, body, preComms, postComms, Duplicate(parallelization), condition)
      else
        IR_WaLBerlaLoopOverPointsInOneFragment(wbField.domain.index, wbField, region, startOffset, endOffset, increment, body, ListBuffer(), ListBuffer(), Duplicate(parallelization), condition)

    if (insideFragLoop && innerLoop.parallelization.reduction.isDefined)
      innerLoop.parallelization.reduction.get.skipMpi = true

    val loopOverFrags = ListBuffer(IR_WaLBerlaLoopOverBlocks(ListBuffer(innerLoop), Duplicate(parallelization)))

    if (Knowledge.experimental_splitLoopsForAsyncComm)
      loopOverFrags
    else {
      if (preComms.nonEmpty) Logger.warn("Found precomm")
      preComms ++ loopOverFrags ++ postComms
    }
  }
}

/// IR_WaLBerlaResolveLoopOverPoints

object IR_WaLBerlaResolveLoopOverPoints extends DefaultStrategy("Resolve WB LoopOverPoints nodes") {
  val collector = new IR_StackCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve", {
    case loop : IR_WaLBerlaLoopOverPoints => loop.expandSpecial(collector)
  })
}


// old stuff
/*
// WB_ReplaceLoopOverPoints

object IR_WaLBerlaReplaceLoopOverFragments extends DefaultStrategy("Replace loops in WB kernel") {

  this += datastructures.Transformation("Replace fragment loops", {
    case IR_LoopOverFragments(bdy, parInfo)    =>
      //Logger.dbg("IR_LoopOverFragments")
      IR_WaLBerlaLoopOverFragments(bdy, parInfo).expandSpecial()
    /*
    case loop : IR_LoopOverPointsInOneFragment =>
      Logger.dbg("IR_LoopOverPointsInOneFragment")
      loop
    case loop : IR_LoopOverPoints              =>
      Logger.dbg("IR_LoopOverPoints")
      loop
    case loop : IR_LoopOverDimensions          =>
      Logger.dbg("IR_LoopOverDimensions")
      loop
    case stmt : IR_Statement                   =>
      stmt
    */
  }, applyAtNode = IR_WaLBerlaUtil.startNode)
}
*/