package exastencils.primitives

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.core.collectors._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.globals._
import exastencils.knowledge._
import exastencils.mpi._
import exastencils.omp._
import exastencils.polyhedron._
import exastencils.strategies.SimplifyStrategy
import exastencils.util._

case class CommunicateStatement(var field : FieldSelection) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = CommunicateStatement\n"

  def expand : Statement = {
    new FunctionCallExpression("exch" ~ field.codeName, field.slot)
  }
}

case class LocalSend(var field : FieldSelection, var neighbors : ListBuffer[(NeighborInfo, IndexRange, IndexRange)]) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = LocalSend\n"

  def expand : LoopOverFragments = {
    new LoopOverFragments(field.domainIndex,
      neighbors.map(neigh =>
        (new ConditionStatement(iv.NeighborIsValid(field.domainIndex, neigh._1.index) AndAnd UnaryExpression(UnaryOperators.Not, iv.NeighborIsRemote(field.domainIndex, neigh._1.index)),
          ListBuffer[Statement](
            new LoopOverDimensions(Knowledge.dimensionality + 1,
              neigh._2,
              new AssignmentStatement(
                new DirectFieldAccess(FieldSelection(field.field, field.slot, -1, iv.NeighborFragLocalId(field.domainIndex, neigh._1.index)), new MultiIndex(
                  new MultiIndex(DefaultLoopMultiIndex(), neigh._3.begin, _ + _), neigh._2.begin, _ - _)),
                new DirectFieldAccess(FieldSelection(field.field, field.slot, -1), DefaultLoopMultiIndex()))) with OMP_PotentiallyParallel with PolyhedronAccessable))) : Statement)) with OMP_PotentiallyParallel
  }
}

case class CopyToSendBuffer(var field : FieldSelection, var neighbor : NeighborInfo, var indices : IndexRange) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = CopyToSendBuffer\n"

  def expand : Statement = {
    val tmpBufAccess = new ArrayAccess(iv.TmpBuffer(field.field, "Send", indices.getSizeHigher, neighbor.index),
      Mapping.resolveMultiIdx(new MultiIndex(DefaultLoopMultiIndex(), indices.begin, _ - _), indices))
    val fieldAccess = new DirectFieldAccess(FieldSelection(field.field, field.slot, -1), DefaultLoopMultiIndex())

    new LoopOverDimensions(Knowledge.dimensionality + 1, indices, new AssignmentStatement(tmpBufAccess, fieldAccess)) with OMP_PotentiallyParallel with PolyhedronAccessable
  }
}

case class CopyFromRecvBuffer(var field : FieldSelection, var neighbor : NeighborInfo, var indices : IndexRange) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = CopyFromRecvBuffer\n"

  def expand : Statement = {
    val tmpBufAccess = new ArrayAccess(iv.TmpBuffer(field.field, "Recv", indices.getSizeHigher, neighbor.index),
      Mapping.resolveMultiIdx(new MultiIndex(DefaultLoopMultiIndex(), indices.begin, _ - _), indices))
    val fieldAccess = new DirectFieldAccess(FieldSelection(field.field, field.slot, -1), DefaultLoopMultiIndex())

    new LoopOverDimensions(Knowledge.dimensionality + 1, indices, new AssignmentStatement(fieldAccess, tmpBufAccess)) with OMP_PotentiallyParallel with PolyhedronAccessable
  }
}

case class RemoteSend(var field : FieldSelection, var neighbor : NeighborInfo, var src : Expression, var numDataPoints : Expression, var datatype : Datatype) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = RemoteSend\n"

  def expand : Statement = {
    StatementBlock(ListBuffer[Statement](
      new MPI_Send(src, numDataPoints, datatype, iv.NeighborRemoteRank(field.domainIndex, neighbor.index),
        s"((unsigned int)" ~ iv.CommId() ~ " << 16) + ((unsigned int)(" ~ iv.NeighborFragLocalId(field.domainIndex, neighbor.index) ~ ") & 0x0000ffff)",
        iv.MpiRequest(field.field, "Send", neighbor.index)) with OMP_PotentiallyCritical,
      AssignmentStatement(iv.ReqOutstanding(field.field, "Send", neighbor.index), true)))
  }
}

case class RemoteRecv(var field : FieldSelection, var neighbor : NeighborInfo, var dest : Expression, var numDataPoints : Expression, var datatype : Datatype) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = RemoteRecv\n"

  def expand : Statement = {
    StatementBlock(ListBuffer[Statement](
      new MPI_Receive(dest, numDataPoints, datatype, iv.NeighborRemoteRank(field.domainIndex, neighbor.index),
        "((unsigned int)(" ~ iv.NeighborFragLocalId(field.domainIndex, neighbor.index) ~ ") << 16) + ((unsigned int)" ~ iv.CommId() ~ " & 0x0000ffff)",
        iv.MpiRequest(field.field, "Recv", neighbor.index)) with OMP_PotentiallyCritical,
      AssignmentStatement(iv.ReqOutstanding(field.field, "Recv", neighbor.index), true)))
  }
}

case class WaitForTransfer(var field : FieldSelection, var neighbor : NeighborInfo, var direction : String) extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = WaitForTransfer\n"

  def expand : Statement = {
    new ConditionStatement(
      iv.ReqOutstanding(field.field, direction, neighbor.index),
      ListBuffer[Statement](
        new WaitForMPIReq(iv.MpiRequest(field.field, direction, neighbor.index)) with OMP_PotentiallyCritical,
        AssignmentStatement(iv.ReqOutstanding(field.field, direction, neighbor.index), false)))
  }
}

abstract class RemoteTransfers extends Statement with Expandable {
  var field : FieldSelection
  var neighbors : ListBuffer[(NeighborInfo, IndexRange)]

  def genCopy(neighbor : NeighborInfo, indices : IndexRange, addCondition : Boolean) : Statement
  def genTransfer(neighbor : NeighborInfo, indices : IndexRange, addCondition : Boolean) : Statement

  def wrapCond(neighbor : NeighborInfo, body : ListBuffer[Statement]) : Statement = {
    new ConditionStatement(iv.NeighborIsValid(field.domainIndex, neighbor.index) AndAnd iv.NeighborIsRemote(field.domainIndex, neighbor.index),
      body)
  }
}

case class RemoteSends(var field : FieldSelection, var neighbors : ListBuffer[(NeighborInfo, IndexRange)], var start : Boolean, var end : Boolean) extends RemoteTransfers {
  override def cpp : String = "NOT VALID ; CLASS = RemoteSends\n"

  def genCopy(neighbor : NeighborInfo, indices : IndexRange, addCondition : Boolean) : Statement = {
    if (!MPI_DataType.shouldBeUsed(indices) && SimplifyExpression.evalIntegral(indices.getSizeHigher) > 1) {
      var body = CopyToSendBuffer(field, neighbor, indices)
      if (addCondition) wrapCond(neighbor, ListBuffer[Statement](body)) else body
    } else {
      new NullStatement
    }
  }

  def genTransfer(neighbor : NeighborInfo, indices : IndexRange, addCondition : Boolean) : Statement = {
    var body = {
      if (1 == SimplifyExpression.evalIntegral(indices.getSizeHigher)) {
        RemoteSend(field, neighbor, s"&" ~ new DirectFieldAccess(field, indices.begin), 1, new RealDatatype)
      } else if (MPI_DataType.shouldBeUsed(indices)) {
        RemoteSend(field, neighbor, s"&" ~ new DirectFieldAccess(field, indices.begin), 1, MPI_DataType(field, indices))
      } else {
        var cnt = DimArrayHigher().map(i => (indices.end(i) - indices.begin(i)).asInstanceOf[Expression]).reduceLeft(_ * _)
        SimplifyStrategy.doUntilDoneStandalone(cnt)
        RemoteSend(field, neighbor, iv.TmpBuffer(field.field, "Send", cnt, neighbor.index), cnt, new RealDatatype)
      }
    }
    if (addCondition) wrapCond(neighbor, ListBuffer[Statement](body)) else body
  }

  def genWait(neighbor : NeighborInfo) : Statement = {
    new WaitForTransfer(field, neighbor, "Send")
  }

  def expand : StatementBlock = {
    // TODO: think about employing neighbor loops
    //      var minIdx = neighbors.reduce((neigh, res) => if (neigh.index < res.index) neigh else res).index
    //      var maxIdx = neighbors.reduce((neigh, res) => if (neigh.index > res.index) neigh else res).index
    //        new ForLoopStatement(s"int i = $minIdx", s"i <= $maxIdx", "++i", ...)
    if (Knowledge.comm_useFragmentLoopsForEachOp)
      StatementBlock(ListBuffer[Statement](
        if (start) new LoopOverFragments(field.domainIndex, neighbors.map(neigh => genCopy(neigh._1, neigh._2, true))) with OMP_PotentiallyParallel else new NullStatement,
        if (start) new LoopOverFragments(field.domainIndex, neighbors.map(neigh => genTransfer(neigh._1, neigh._2, true))) with OMP_PotentiallyParallel else new NullStatement,
        if (end) new LoopOverFragments(field.domainIndex, neighbors.map(neigh => genWait(neigh._1))) else new NullStatement))
    else
      StatementBlock(ListBuffer[Statement](
        new LoopOverFragments(field.domainIndex, neighbors.map(neigh =>
          wrapCond(neigh._1, ListBuffer(
            if (start) genCopy(neigh._1, neigh._2, false) else NullStatement(),
            if (start) genTransfer(neigh._1, neigh._2, false) else NullStatement(),
            if (end) genWait(neigh._1) else NullStatement())))) with OMP_PotentiallyParallel))
  }
}

case class RemoteRecvs(var field : FieldSelection, var neighbors : ListBuffer[(NeighborInfo, IndexRange)], var start : Boolean, var end : Boolean) extends RemoteTransfers {
  override def cpp : String = "NOT VALID ; CLASS = RemoteRecvs\n"

  def genCopy(neighbor : NeighborInfo, indices : IndexRange, addCondition : Boolean) : Statement = {
    if (!MPI_DataType.shouldBeUsed(indices) && SimplifyExpression.evalIntegral(indices.getSizeHigher) > 1) {
      var body = CopyFromRecvBuffer(field, neighbor, indices)
      if (addCondition) wrapCond(neighbor, ListBuffer[Statement](body)) else body
    } else {
      new NullStatement
    }
  }

  def genTransfer(neighbor : NeighborInfo, indices : IndexRange, addCondition : Boolean) : Statement = {
    var body = {
      if (1 == SimplifyExpression.evalIntegral(indices.getSizeHigher)) {
        RemoteRecv(field, neighbor, s"&" ~ new DirectFieldAccess(field, indices.begin), 1, new RealDatatype)
      } else if (MPI_DataType.shouldBeUsed(indices)) {
        RemoteRecv(field, neighbor, s"&" ~ new DirectFieldAccess(field, indices.begin), 1, MPI_DataType(field, indices))
      } else {
        var cnt = DimArrayHigher().map(i => (indices.end(i) - indices.begin(i)).asInstanceOf[Expression]).reduceLeft(_ * _)
        SimplifyStrategy.doUntilDoneStandalone(cnt)
        RemoteRecv(field, neighbor, iv.TmpBuffer(field.field, "Recv", cnt, neighbor.index), cnt, new RealDatatype)
      }
    }
    if (addCondition) wrapCond(neighbor, ListBuffer[Statement](body)) else body
  }

  def genWait(neighbor : NeighborInfo) : Statement = {
    new WaitForTransfer(field, neighbor, "Recv")
  }

  def expand : StatementBlock = {
    // TODO: think about employing neighbor loops
    //      var minIdx = neighbors.reduce((neigh, res) => if (neigh.index < res.index) neigh else res).index
    //      var maxIdx = neighbors.reduce((neigh, res) => if (neigh.index > res.index) neigh else res).index
    //        new ForLoopStatement(s"int i = $minIdx", s"i <= $maxIdx", "++i", ...)
    if (Knowledge.comm_useFragmentLoopsForEachOp)
      StatementBlock(ListBuffer[Statement](
        if (start) new LoopOverFragments(field.domainIndex, neighbors.map(neigh => genTransfer(neigh._1, neigh._2, true))) with OMP_PotentiallyParallel else new NullStatement,
        if (end) new LoopOverFragments(field.domainIndex, neighbors.map(neigh => genWait(neigh._1))) else new NullStatement,
        if (end) new LoopOverFragments(field.domainIndex, neighbors.map(neigh => genCopy(neigh._1, neigh._2, true))) with OMP_PotentiallyParallel else new NullStatement))
    else
      StatementBlock(ListBuffer[Statement](
        new LoopOverFragments(field.domainIndex, neighbors.map(neigh =>
          wrapCond(neigh._1, ListBuffer(
            if (start) genTransfer(neigh._1, neigh._2, false) else NullStatement(),
            if (end) genWait(neigh._1) else NullStatement(),
            if (end) genCopy(neigh._1, neigh._2, false) else NullStatement())))) with OMP_PotentiallyParallel))
  }
}

case class WaitForMPIReq(var request : Expression) extends Statement {
  override def cpp : String = {
    s"waitForMPIReq(&${request.cpp});"
  }
}