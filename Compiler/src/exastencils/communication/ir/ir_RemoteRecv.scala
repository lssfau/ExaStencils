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
import exastencils.knowledge._
import exastencils.mpi.ir._
import exastencils.omp.OMP_PotentiallyParallel
import exastencils.omp.ir.OMP_PotentiallyCritical
import exastencils.polyhedron.PolyhedronAccessible
import exastencils.prettyprinting.PpStream

/// IR_RemoteRecv

case class IR_RemoteRecv(
    var field : IR_FieldSelection,
    var neighbor : NeighborInfo,
    var dest : IR_Expression,
    var numDataPoints : IR_Expression,
    var datatype : IR_Datatype,
    var concurrencyId : Int) extends IR_Statement with IR_Expandable {

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand() : Output[StatementList] = {
    ListBuffer[IR_Statement](
      OMP_PotentiallyCritical(MPI_Receive(dest, numDataPoints, datatype, IR_IV_NeighborRemoteRank(field.domainIndex, neighbor.index),
        MPI_GeneratedTag(IR_IV_NeighborFragmentIdx(field.domainIndex, neighbor.index), iv.CommId(),
          Fragment.getOpposingNeigh(neighbor.index).index, concurrencyId),
        iv.MpiRequest(field.field, s"Recv_${ concurrencyId }", neighbor.index))),
      IR_Assignment(iv.RemoteReqOutstanding(field.field, s"Recv_${ concurrencyId }", neighbor.index), true))
  }
}

/// IR_CopyFromRecvBuffer

case class IR_CopyFromRecvBuffer(
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
      def it = iv.TmpBufferIterator(field.field, s"Recv_${ concurrencyId }", neighbor.index)

      val tmpBufAccess = IR_TempBufferAccess(iv.TmpBuffer(field.field, s"Recv_${ concurrencyId }", indices.getTotalSize, neighbor.index),
        IR_ExpressionIndex(it), IR_ExpressionIndex(0) /* dummy stride */)
      val fieldAccess = IR_DirectFieldAccess(IR_FieldSelection(field.field, field.level, field.slot), IR_LoopOverDimensions.defIt(numDims))

      ret += IR_Assignment(it, 0)
      ret += IR_LoopOverDimensions(numDims, indices, IR_IfCondition(
        condition.get, ListBuffer[IR_Statement](
          IR_Assignment(fieldAccess, tmpBufAccess),
          IR_Assignment(it, 1, "+="))))
    } else {
      val tmpBufAccess = IR_TempBufferAccess(iv.TmpBuffer(field.field, s"Recv_${ concurrencyId }", indices.getTotalSize, neighbor.index),
        IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims), indices.begin, _ - _),
        IR_ExpressionIndex(indices.end, indices.begin, _ - _))
      val fieldAccess = IR_DirectFieldAccess(IR_FieldSelection(field.field, field.level, field.slot), IR_LoopOverDimensions.defIt(numDims))

      ret += new IR_LoopOverDimensions(numDims, indices, ListBuffer[IR_Statement](IR_Assignment(fieldAccess, tmpBufAccess))) with OMP_PotentiallyParallel with PolyhedronAccessible
    }

    ret
  }
}
