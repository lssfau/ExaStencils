package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.ir.QuadraticInterpPackingC2FHelper._
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.StatementList
import exastencils.domain.ir._
import exastencils.field.ir._
import exastencils.grid.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_GeneralSimplify

case class IR_LinearInterpPackingF2CRemote(
    var send : Boolean,
    var field : IR_Field,
    var slot : IR_Expression,
    var refinementCase : RefinementCase.Access,
    var packInfo : IR_RemotePackInfo,
    var concurrencyId : Int,
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  import IR_InterpPackingHelper._

  def numDims : Int = field.layout.numDimsData

  def defIt : IR_ExpressionIndex = IR_LoopOverDimensions.defIt(numDims)

  def commDir : Array[Int] = packInfo.neighDir

  override def expand() : Output[StatementList] = {
    if (condition.isDefined)
      Logger.error("Conditions for refined communication are not supported yet.")

    val neighbor = packInfo.neighbor
    val neighborIdx = neighbor.index
    val indices = packInfo.getPackInterval()

    var ret = ListBuffer[IR_Statement]()

    def it = IR_IV_CommBufferIterator(field, s"${ itName }_${ concurrencyId }", neighborIdx)

    def itName = if (send) "Send" else "Recv"

    def commBuffer = IR_IV_CommBuffer(field, s"${ itName }_${ concurrencyId }", indices.getTotalSize, neighborIdx)

    val tmpBufAccess = IR_TempBufferAccess(commBuffer,
      IR_ExpressionIndex(it), IR_ExpressionIndex(0) /* dummy stride */)

    var innerStmts : ListBuffer[IR_Statement] = ListBuffer()

    // init temp buf idx counter
    ret += IR_Assignment(it, 0)

    if (send) {

    } else {

    }

    // fine neighbor cells (2 in 2D, 4 in 3D) are linearly interpolated and the result is sent to the coarse neighbor
    val stride = if (send) IR_ExpressionIndex(Array.fill(Knowledge.dimensionality)(2).updated(getDimFromDir(commDir), 1)) else null

    val loop = new IR_LoopOverDimensions(numDims, indices, innerStmts, stride, condition = condition)
    loop.polyOptLevel = 1
    loop.parallelization.potentiallyParallel = true
    ret += loop

    ret
  }
}

case class IR_LinearInterpPackingF2CLocal(
    var send : Boolean,
    var field : IR_Field,
    var slot : IR_Expression,
    var refinementCase : RefinementCase.Access,
    var packInfo : IR_LocalPackInfo,
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  import IR_InterpPackingHelper._

  def numDims : Int = field.layout.numDimsData

  def commDir : Array[Int] = packInfo.neighDir

  override def expand() : OutputType = {
    if (condition.isDefined)
      Logger.error("Conditions for refined communication are not supported yet.")

    val packIntervalDest = packInfo.getPackIntervalDest()
    val packIntervalSrc = packInfo.getPackIntervalSrc()

    val neighbor = packInfo.neighbor
    val domainIdx = field.domain.index
    val neighborIdx = neighbor.index

    var innerStmts : ListBuffer[IR_Statement] = ListBuffer()

    // fine neighbor cells (2 in 2D, 4 in 3D) are linearly interpolated and the result is sent to the coarse neighbor
    val stride = if (send) IR_ExpressionIndex(Array.fill(Knowledge.dimensionality)(2).updated(getDimFromDir(commDir), 1)) else null

    val loop = new IR_LoopOverDimensions(numDims, packIntervalDest, innerStmts, stride, condition = condition)
    loop.polyOptLevel = 1
    loop.parallelization.potentiallyParallel = true
    loop
  }
}

