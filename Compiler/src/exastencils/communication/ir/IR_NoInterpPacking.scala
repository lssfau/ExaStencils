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
import exastencils.field.ir._

case class IR_NoInterpPackingRemote(
    var send : Boolean,
    var field : IR_Field,
    var slot : IR_Expression,
    var refinementCase : RefinementCase.Access,
    var packInfo : IR_RemotePackInfo,
    var concurrencyId : Int,
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  def numDims = field.layout.numDimsData

  override def expand() : Output[StatementList] = {
    val neighbor = packInfo.neighbor
    val neighborIdx = neighbor.index
    val indices = packInfo.getPackInterval()

    var ret = ListBuffer[IR_Statement]()

    def itName = if (send) "Send" else "Recv"

    def commBuffer = IR_IV_CommBuffer(field, s"${ itName }_${ concurrencyId }", indices.getTotalSize, neighborIdx)

    val fieldAccess = IR_DirectFieldAccess(field, Duplicate(slot), IR_LoopOverDimensions.defIt(numDims))

    if (condition.isDefined && Knowledge.comm_compactPackingForConditions) {
      // compact packing with condition

      def it = IR_IV_CommBufferIterator(field, s"${ itName }_${ concurrencyId }", neighborIdx)

      val tmpBufAccess = IR_TempBufferAccess(commBuffer,
        IR_ExpressionIndex(it), IR_ExpressionIndex(0) /* dummy stride */)

      val assign = if (send) IR_Assignment(tmpBufAccess, fieldAccess) else IR_Assignment(fieldAccess, tmpBufAccess)

      ret += IR_Assignment(it, 0)
      ret += IR_LoopOverDimensions(numDims, indices, IR_IfCondition(
        condition.get, ListBuffer[IR_Statement](
          assign,
          IR_Assignment(it, 1, "+="))))
    } else {
      // in case of condition: non-compact packing

      val tmpBufAccess = IR_TempBufferAccess(commBuffer,
        IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims), indices.begin, _ - _),
        IR_ExpressionIndex(indices.end, indices.begin, _ - _))

      val assign = if (send) IR_Assignment(tmpBufAccess, fieldAccess) else IR_Assignment(fieldAccess, tmpBufAccess)

      if (!send && Knowledge.comm_enableCommTransformations) {
        // special case: recv with comm trafos
        val trafoId = IR_IV_CommTrafoId(field.domain.index, neighborIdx)

        def loop(trafo : IR_CommTransformation) = {
          val ret = new IR_LoopOverDimensions(numDims, indices, ListBuffer[IR_Statement](IR_Assignment(trafo.applyRemoteTrafo(fieldAccess, indices, neighbor), trafo.applyBufferTrafo(tmpBufAccess))))
          ret.polyOptLevel = 1
          ret.parallelization.potentiallyParallel = true
          ret
        }

        ret += IR_Switch(trafoId, IR_CommTransformationCollection.trafos.zipWithIndex.map {
          case (trafo, i) => IR_Case(i, ListBuffer[IR_Statement](loop(trafo)))
        })
      } else {
        // regular case: (un-)packing for send/recv

        val loop = new IR_LoopOverDimensions(numDims, indices, ListBuffer(assign), condition = condition)
        loop.polyOptLevel = 1
        loop.parallelization.potentiallyParallel = true
        ret += loop
      }
    }

    ret
  }
}

case class IR_NoInterpPackingLocal(
    var send : Boolean,
    var field : IR_Field,
    var slot : IR_Expression,
    var refinementCase : RefinementCase.Access,
    var packInfo : IR_LocalPackInfo,
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  def numDims = field.layout.numDimsData

  override def expand() : OutputType = {
    val packIntervalDest = packInfo.getPackIntervalDest()
    val packIntervalSrc = packInfo.getPackIntervalSrc()

    val neighbor = packInfo.neighbor
    val domainIdx = field.domain.index
    val neighborIdx = neighbor.index

    // only inner statement for assignment is different for send/recv
    var innerStmt : IR_Statement = if (send)
      IR_Assignment(
        IR_DirectFieldAccess(field, Duplicate(slot), IR_IV_NeighborFragmentIdx(domainIdx, neighborIdx), IR_ExpressionIndex(
          IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims), packIntervalSrc.begin, _ + _), packIntervalDest.begin, _ - _)),
        IR_DirectFieldAccess(field, Duplicate(slot), IR_LoopOverDimensions.defIt(numDims)))
    else
      IR_Assignment(
        IR_DirectFieldAccess(field, Duplicate(slot), IR_LoopOverDimensions.defIt(numDims)),
        IR_DirectFieldAccess(field, Duplicate(slot), IR_IV_NeighborFragmentIdx(domainIdx, neighborIdx),
          IR_ExpressionIndex(IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims), packIntervalSrc.begin, _ + _), packIntervalDest.begin, _ - _)))

    if (condition.isDefined)
      innerStmt = IR_IfCondition(condition.get, innerStmt)

    val loop = new IR_LoopOverDimensions(numDims, packIntervalDest, ListBuffer[IR_Statement](innerStmt))
    loop.polyOptLevel = 1
    loop.parallelization.potentiallyParallel = true

    loop
  }
}
