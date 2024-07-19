package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.StatementList
import exastencils.domain.ir._
import exastencils.fieldlike.ir._
import exastencils.logger.Logger

object LinearInterpPackingF2CHelper {

  import IR_InterpPackingBaseHelper._
  import exastencils.communication.ir.IR_InterpPackingHelper._

  private val shifts = LinearBaseShifts(remapped = true)

  def generateInterpExpr(field : IR_FieldLike, origin : IR_ExpressionIndex, slot : IR_Expression, packInfo : IR_PackInfo) : IR_Expression = {
    def level : Int = field.level

    def localization = field.localization

    def commDir : Array[Int] = packInfo.neighDir

    def invCommDir : Array[Int] = commDir.map(_ * -1)

    // get neighbors in upwind ortho dir and get additional neighbors by offsetting to inv comm dir
    val upwindOrthogonals = getCrossSumOfUpwindOrthogonals(commDir).distinct
    val invCommUpwindOrthogonals = upwindOrthogonals.map(off => dirSum(off, invCommDir))

    // access field at all neighbor locations
    val accesses = (upwindOrthogonals ++ invCommUpwindOrthogonals).distinct.map { off =>
      IR_DirectFieldLikeAccess(field, Duplicate(slot), origin + IR_ExpressionIndex(off))
    }

    val w = Knowledge.dimensionality match {
      case 2 => 0.25
      case 3 => 0.125
    }

    // add all field accesses up and multiply with precomputed weight
    w * IR_Addition(accesses : _*)

    /*
    // TODO: maybe rewrite by using BaseValues/Positions etc. in the future
    // calculate linear interpolations on orthogonal (fine) neighbor cells in upwind dir
    val linearInterpResult : Array[(IR_Expression, IR_Expression)] = getCrossSumOfUpwindOrthogonals(commDir).distinct.map(offset => {
      val basePositionsOrtho = getBasePositions(level, localization, invCommDir, origin + IR_ExpressionIndex(offset), shifts)
      val baseValuesOrtho = getBaseValues(field, slot, invCommDir, origin + IR_ExpressionIndex(offset), shifts)
      val pos = 0.5 * getCellWidth(level, getDimFromDir(invCommDir), origin + IR_ExpressionIndex(offset))

      pos -> interpolate1D(pos, basePositionsOrtho, baseValuesOrtho)
    }).toArray

    // transition from 1D to 2D, and then 3D
    def interpAgain(res : Array[(IR_Expression, IR_Expression)]) : Array[(IR_Expression, IR_Expression)] = {
      res.sliding(2).collect { case Array(a, b) => (a, b) }.map {
        case ((x0, a), (x1, b)) =>
          val pos = 0.5 * (x1 - x0)
          pos -> interpolate1D(pos, LinearBasePositions(x0, x1), LinearBaseValues(a, b))
      }.toArray
    }

    val bilinearInterpResult = interpAgain(linearInterpResult)
    Knowledge.dimensionality match {
      case 2 =>
        bilinearInterpResult.head._2
      case 3 =>
        interpAgain(bilinearInterpResult).head._2
    }
    */
  }
}

case class IR_LinearInterpPackingF2CRemote(
    var send : Boolean,
    var field : IR_FieldLike,
    var slot : IR_Expression,
    var refinementCase : RefinementCase.Access,
    var packInfo : IR_RemotePackInfo,
    var concurrencyId : Int,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  import IR_InterpPackingHelper._
  import LinearInterpPackingF2CHelper._

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

    def commBuffer = IR_IV_CommBuffer(field, send, indices.getTotalSize, neighborIdx, concurrencyId, indexOfRefinedNeighbor)

    def tmpBufAccess(offset : IR_Expression) = IR_TempBufferAccess(commBuffer,
      IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims), indices.begin, _ - _) + IR_ExpressionIndex(offset),
      IR_ExpressionIndex(indices.end, indices.begin, _ - _))

    var innerStmts : ListBuffer[IR_Statement] = ListBuffer()

    if (send) {
      innerStmts += IR_Assignment(tmpBufAccess(0), generateInterpExpr(field, defIt, slot, packInfo))
    } else {
      // interp/extrap values from coarse neighbor are written in order
      // of the upwind orthogonal dirs (in regards to comm dir) and their cross sums

      // read from buffer into field in order of cross sum of orthogonal upwind dirs
      for ((offset, i) <- getCrossSumOfUpwindOrthogonals(commDir).distinct.zipWithIndex) {
        innerStmts += IR_Assignment(
          IR_DirectFieldLikeAccess(field, Duplicate(slot), defIt + IR_ExpressionIndex(offset)), tmpBufAccess(i))
      }
    }

    // fine neighbor cells (2 in 2D, 4 in 3D) are linearly interpolated and the result is sent to the coarse neighbor
    val stride = IR_ExpressionIndex(Array.fill(Knowledge.dimensionality)(2).updated(getDimFromDir(commDir), 1))

    val loop = new IR_LoopOverDimensions(numDims, indices, innerStmts, stride, condition = condition)
    loop.polyOptLevel = 1
    loop.parallelization.potentiallyParallel = true
    loop.parallelization.noVect = !send // different indexing of field iterator and tmp buffer for recv
    ret += loop

    ret
  }
}

case class IR_LinearInterpPackingF2CLocal(
    var send : Boolean,
    var field : IR_FieldLike,
    var slot : IR_Expression,
    var refinementCase : RefinementCase.Access,
    var packInfo : IR_LocalPackInfo,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  import IR_InterpPackingHelper._
  import LinearInterpPackingF2CHelper._

  def numDims : Int = field.layout.numDimsData

  def commDir : Array[Int] = packInfo.neighDir

  override def expand() : OutputType = {
    if (condition.isDefined)
      Logger.error("Conditions for refined communication are not supported yet.")

    // TODO: pull scheme for local comm, only push implemented
    if (!send)
      Logger.warn("Pull comm scheme is not yet implemented for linear F2C interp.")

    val packIntervalDest = packInfo.getPackIntervalDest()
    val packIntervalSrc = packInfo.getPackIntervalSrc()

    val neighbor = packInfo.neighbor
    val domainIdx = field.domain.index
    val neighborIdx = neighbor.index

    // index mapping between the (local) fine/coarse iteration space
    val originDest = IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims).indices.zipWithIndex.map { case (idx, i) =>
      if (getDimFromDir(neighbor.dir) != i)
        packIntervalDest.begin(i) + (Knowledge.refinement_maxFineNeighborsPerDim * (idx - packIntervalSrc.begin(i)))
      else
        packIntervalDest.begin(i) + idx - packIntervalSrc.begin(i)
    })

    var innerStmts : ListBuffer[IR_Statement] = ListBuffer()

    // push result to destination
    innerStmts += IR_Assignment(
      IR_DirectFieldLikeAccess(field, Duplicate(slot),
        IR_IV_NeighborFragmentIdx(domainIdx, neighborIdx, indexOfRefinedNeighbor),
        IR_LoopOverDimensions.defIt(numDims)),
      generateInterpExpr(field, originDest, slot, packInfo))

    val loop = new IR_LoopOverDimensions(numDims, packIntervalSrc, innerStmts, condition = condition)
    loop.polyOptLevel = 1
    loop.parallelization.potentiallyParallel = true
    loop
  }
}

