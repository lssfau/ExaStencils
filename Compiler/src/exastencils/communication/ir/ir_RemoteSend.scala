package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication._
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir.{ iv, _ }
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.domain.ir._
import exastencils.field.ir.IR_DirectFieldAccess
import exastencils.parallelization.api.mpi._
import exastencils.parallelization.ir.IR_PotentiallyCritical
import exastencils.polyhedron.PolyhedronAccessible
import exastencils.prettyprinting.PpStream

case class IR_RemoteSend(
    var field : IR_FieldSelection,
    var neighbor : NeighborInfo,
    var src : IR_Expression,
    var numDataPoints : IR_Expression,
    var datatype : IR_Datatype,
    var concurrencyId : Int) extends IR_Statement with IR_Expandable {

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand() : Output[StatementList] = {
    ListBuffer[IR_Statement](
      IR_PotentiallyCritical(MPI_Send(src, numDataPoints, datatype, IR_IV_NeighborRemoteRank(field.domainIndex, neighbor.index),
        MPI_GeneratedTag(iv.CommId(), IR_IV_NeighborFragmentIdx(field.domainIndex, neighbor.index), neighbor.index, concurrencyId),
        iv.MpiRequest(field.field, s"Send_${ concurrencyId }", neighbor.index))),
      IR_Assignment(iv.RemoteReqOutstanding(field.field, s"Send_${ concurrencyId }", neighbor.index), true))
  }
}

/// IR_CopyToSendBuffer

case class IR_CopyToSendBuffer(
    var field : IR_FieldSelection,
    var neighbor : NeighborInfo,
    var indices : IR_ExpressionIndexRange,
    var concurrencyId : Int,
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def numDims = field.field.fieldLayout.numDimsData

  override def expand() : Output[StatementList] = {
    var ret = ListBuffer[IR_Statement]()

    if (condition.isDefined) {
      // switch to iterator based copy operation if condition is defined -> number of elements and index mapping is unknown
      def it = iv.TmpBufferIterator(field.field, s"Send_${ concurrencyId }", neighbor.index)

      val tmpBufAccess = IR_TempBufferAccess(iv.TmpBuffer(field.field, s"Send_${ concurrencyId }", indices.getTotalSize, neighbor.index),
        IR_ExpressionIndex(it), IR_ExpressionIndex(0) /* dummy stride */)
      val fieldAccess = IR_DirectFieldAccess(IR_FieldSelection(field.field, field.level, field.slot), IR_LoopOverDimensions.defIt(numDims))

      ret += IR_Assignment(it, 0)
      ret += IR_LoopOverDimensions(numDims, indices, IR_IfCondition(
        condition.get, ListBuffer[IR_Statement](
          IR_Assignment(tmpBufAccess, fieldAccess),
          IR_Assignment(it, 1, "+="))))
    } else {
      val tmpBufAccess = IR_TempBufferAccess(iv.TmpBuffer(field.field, s"Send_${ concurrencyId }", indices.getTotalSize, neighbor.index),
        IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims), indices.begin, _ - _),
        IR_ExpressionIndex(indices.end, indices.begin, _ - _))
      val fieldAccess = IR_DirectFieldAccess(IR_FieldSelection(field.field, field.level, field.slot), IR_LoopOverDimensions.defIt(numDims))

      val loop = new IR_LoopOverDimensions(numDims, indices, ListBuffer[IR_Statement](IR_Assignment(tmpBufAccess, fieldAccess))) with PolyhedronAccessible
      loop.parallelization.potentiallyParallel = true
      ret += loop
    }

    ret
  }
}
