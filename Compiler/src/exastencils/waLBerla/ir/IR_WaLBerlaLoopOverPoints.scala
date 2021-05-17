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
import exastencils.field.ir.IR_FieldAccess
import exastencils.grid.ir.IR_VirtualFieldAccess
import exastencils.logger.Logger
import exastencils.parallelization.ir.IR_HasParallelizationInfo
import exastencils.parallelization.ir.IR_ParallelizationInfo
import exastencils.util.ir.IR_CollectFieldAccesses
import exastencils.util.ir.IR_StackCollector
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes._
import exastencils.waLBerla.ir.IR_WaLBerlaSweep._

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

    // collect fields accessed in loop
    val fieldAccesses = ListBuffer[IR_FieldAccess]()
    val vfieldAccesses = ListBuffer[IR_VirtualFieldAccess]()
    IR_CollectFieldAccesses.applyStandalone(body)
    fieldAccesses ++= Duplicate(IR_CollectFieldAccesses.fieldAccesses).groupBy(_.name).map(_._2.head)
    vfieldAccesses ++= Duplicate(IR_CollectFieldAccesses.vFieldAccesses).groupBy(_.name).map(_._2.head) // TODO handle too

    // get field data from block
    val blockDataIDs = fieldAccesses.map(f => getBlockDataID(f) : IR_Expression)
    for (fAcc <- fieldAccesses) {
      val fieldDt = WB_FieldDatatype(fAcc.field)
      body.prepend(WB_IV_FieldData(IR_WaLBerlaField(fAcc.field), fAcc.slot, fAcc.fragIdx).declare(
        Some(new IR_MemberFunctionCallArrow(iblock, s"getData< ${fieldDt.typeName} >", blockDataIDs, fieldDt))))
    }

    // iterate over blocks from block storage
    def loopOverBlocks(body : IR_Statement*) = {
      def defIt = IR_VariableAccess("block", IR_SpecialDatatype("auto"))

      new IR_ForLoop(
        IR_VariableDeclaration(defIt, IR_MemberFunctionCallArrow(blockStoragePtr, "begin", defIt.datatype)),
        IR_Neq(defIt, IR_MemberFunctionCallArrow(blockStoragePtr, "end", defIt.datatype)),
        IR_ExpressionStatement(IR_PreIncrement(defIt)),
        body.to[ListBuffer],
        parallelization)
    }

    // TODO: check if using the underlying 'real' field for the inner loop breaks anything
    val insideFragLoop = collector.stack.exists(_.isInstanceOf[IR_LoopOverFragments])
    val innerLoop =
      if (Knowledge.experimental_splitLoopsForAsyncComm)
        IR_LoopOverPointsInOneFragment(wbField.domain.index, wbField.field, region, startOffset, endOffset, increment, body, preComms, postComms, Duplicate(parallelization), condition)
      else
        IR_LoopOverPointsInOneFragment(wbField.domain.index, wbField.field, region, startOffset, endOffset, increment, body, ListBuffer(), ListBuffer(), Duplicate(parallelization), condition)

    if (insideFragLoop && innerLoop.parallelization.reduction.isDefined)
      innerLoop.parallelization.reduction.get.skipMpi = true

    if (Knowledge.experimental_splitLoopsForAsyncComm)
      ListBuffer(loopOverBlocks(innerLoop))
    else {
      if (preComms.nonEmpty) Logger.warn("Found precomm")
      preComms ++ ListBuffer(loopOverBlocks(innerLoop)) ++ postComms
    }
  }
}

/// IR_WaLBerlaResolveLoopOverPoints
object IR_WaLBerlaResolveLoopOverPoints extends DefaultStrategy("Resolve LoopOverPoints nodes") {
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