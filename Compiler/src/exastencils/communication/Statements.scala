package exastencils.communication

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir.StatementList
import exastencils.knowledge._
import exastencils.mpi._
import exastencils.omp._
import exastencils.polyhedron._
import exastencils.prettyprinting._
import exastencils.strategies._
import exastencils.util._

case class CommunicateTarget(var target : String, var begin : Option[MultiIndex], var end : Option[MultiIndex]) extends Expression {
  if (begin.isDefined && !end.isDefined) end = {
    var newEnd = Duplicate(begin.get)
    for (dim <- 0 until Knowledge.dimensionality)
      newEnd(dim) += 1
    Some(newEnd)
  }

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CommunicateTarget\n"
}

case class CommunicateStatement(var field : FieldSelection, var op : String, var targets : ListBuffer[CommunicateTarget]) extends Statement {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CommunicateStatement\n"
}

case class ApplyBCsStatement(var field : FieldSelection) extends Statement {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = ApplyBCsStatement\n"
}

/// local communication operations

abstract class LocalTransfers extends Statement with Expandable {
  def field : FieldSelection
  def neighbors : ListBuffer[(NeighborInfo, IndexRange, IndexRange)]
  def insideFragLoop : Boolean

  def wrapFragLoop(toWrap : Statement, parallel : Boolean) : Statement = {
    if (insideFragLoop)
      toWrap
    else if (parallel)
      new LoopOverFragments(toWrap) with OMP_PotentiallyParallel
    else
      new LoopOverFragments(toWrap)
  }

  def setLocalCommReady() : ListBuffer[Statement] = {
    neighbors.map(neighbor =>
      new ConditionStatement(iv.NeighborIsValid(field.domainIndex, neighbor._1.index)
        AndAnd NegationExpression(iv.NeighborIsRemote(field.domainIndex, neighbor._1.index)),
        AssignmentStatement(iv.LocalCommReady(field.field, neighbor._1.index), BooleanConstant(true))))
  }

  def waitForLocalComm() : ListBuffer[Statement] = {
    neighbors.map(neighbor =>
      new ConditionStatement(iv.NeighborIsValid(field.domainIndex, neighbor._1.index)
        AndAnd NegationExpression(iv.NeighborIsRemote(field.domainIndex, neighbor._1.index)),
        ListBuffer[Statement](
          new FunctionCallExpression("waitForFlag", AddressofExpression(iv.LocalCommDone(field.field, Fragment.getOpposingNeigh(neighbor._1).index))))))
  }
}

case class LocalSends(var field : FieldSelection, var neighbors : ListBuffer[(NeighborInfo, IndexRange, IndexRange)], var start : Boolean, var end : Boolean,
    var insideFragLoop : Boolean) extends LocalTransfers {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LocalSends\n"

  def expand : Output[StatementList] = {
    var output = ListBuffer[Statement]()

    if (!Knowledge.domain_canHaveLocalNeighs || !Knowledge.comm_pushLocalData)
      return output // nothing to do

    if (start) {
      // set LocalCommReady to allow neighbors to write to this fragment
      output ++= setLocalCommReady

      // distribute this fragment's data
      output += wrapFragLoop(
        new ConditionStatement(iv.IsValidForSubdomain(field.domainIndex),
          neighbors.map(neigh => LocalSend(field, neigh._1, neigh._2, neigh._3, insideFragLoop) : Statement)),
        true)
    }
    if (end) {
      // wait until all neighbors signal that they are finished pushing their data
      output ++= waitForLocalComm
    }
    output
  }
}

case class LocalRecvs(var field : FieldSelection, var neighbors : ListBuffer[(NeighborInfo, IndexRange, IndexRange)], var start : Boolean, var end : Boolean,
    var insideFragLoop : Boolean) extends LocalTransfers {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LocalRecvs\n"

  def expand : Output[StatementList] = {
    var output = ListBuffer[Statement]()

    if (!Knowledge.domain_canHaveLocalNeighs || Knowledge.comm_pushLocalData)
      return output // nothing to do

    if (start) {
      // set LocalCommReady to allow neighbors to read from this fragment
      output ++= setLocalCommReady

      // pull data for this fragment
      output += wrapFragLoop(
        new ConditionStatement(iv.IsValidForSubdomain(field.domainIndex),
          neighbors.map(neigh => LocalRecv(field, neigh._1, neigh._2, neigh._3, insideFragLoop) : Statement)),
        true)
    }
    if (end) {
      // wait until all neighbors signal that they are finished reading data
      output ++= waitForLocalComm
    }
    output
  }
}

case class LocalSend(var field : FieldSelection, var neighbor : NeighborInfo, var dest : IndexRange, var src : IndexRange,
    var insideFragLoop : Boolean) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LocalSend\n"

  def expand : Output[Statement] = {
    new ConditionStatement(iv.NeighborIsValid(field.domainIndex, neighbor.index) AndAnd NegationExpression(iv.NeighborIsRemote(field.domainIndex, neighbor.index)),
      ListBuffer[Statement](
        // wait until the fragment to be written to is ready for communication
        new FunctionCallExpression("waitForFlag", AddressofExpression(iv.LocalCommReady(field.field, Fragment.getOpposingNeigh(neighbor.index).index, iv.NeighborFragLocalId(field.domainIndex, neighbor.index)))),
        new LoopOverDimensions(Knowledge.dimensionality + 1,
          dest,
          new AssignmentStatement(
            new DirectFieldAccess(FieldSelection(field.field, field.level, field.slot, None, iv.NeighborFragLocalId(field.domainIndex, neighbor.index)), new MultiIndex(
              new MultiIndex(LoopOverDimensions.defIt, src.begin, _ + _), dest.begin, _ - _)),
            new DirectFieldAccess(FieldSelection(field.field, field.level, field.slot), LoopOverDimensions.defIt))) with OMP_PotentiallyParallel with PolyhedronAccessable,
        // signal other threads that the data reading step is completed
        AssignmentStatement(iv.LocalCommDone(field.field, neighbor.index), BooleanConstant(true))))
  }
}

case class LocalRecv(var field : FieldSelection, var neighbor : NeighborInfo, var dest : IndexRange, var src : IndexRange,
    var insideFragLoop : Boolean) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LocalRecv\n"

  def expand : Output[Statement] = {
    new ConditionStatement(iv.NeighborIsValid(field.domainIndex, neighbor.index) AndAnd NegationExpression(iv.NeighborIsRemote(field.domainIndex, neighbor.index)),
      ListBuffer[Statement](
        // wait until the fragment to be read from is ready for communication
        new FunctionCallExpression("waitForFlag", AddressofExpression(iv.LocalCommReady(field.field, Fragment.getOpposingNeigh(neighbor.index).index, iv.NeighborFragLocalId(field.domainIndex, neighbor.index)))),
        new LoopOverDimensions(Knowledge.dimensionality + 1,
          dest,
          AssignmentStatement(
            DirectFieldAccess(FieldSelection(field.field, field.level, field.slot), LoopOverDimensions.defIt),
            DirectFieldAccess(FieldSelection(field.field, field.level, field.slot, None, iv.NeighborFragLocalId(field.domainIndex, neighbor.index)),
              new MultiIndex(new MultiIndex(LoopOverDimensions.defIt, src.begin, _ + _), dest.begin, _ - _)))) with OMP_PotentiallyParallel with PolyhedronAccessable,
        // signal other threads that the data reading step is completed
        AssignmentStatement(iv.LocalCommDone(field.field, neighbor.index), BooleanConstant(true))))
  }
}

/// remote communication operations

abstract class RemoteTransfers extends Statement with Expandable {
  def field : FieldSelection
  def neighbors : ListBuffer[(NeighborInfo, IndexRange)]

  def insideFragLoop : Boolean

  def genCopy(neighbor : NeighborInfo, indices : IndexRange, addCondition : Boolean) : Statement
  def genTransfer(neighbor : NeighborInfo, indices : IndexRange, addCondition : Boolean) : Statement

  def wrapCond(neighbor : NeighborInfo, body : ListBuffer[Statement]) : Statement = {
    new ConditionStatement(iv.NeighborIsValid(field.domainIndex, neighbor.index) AndAnd iv.NeighborIsRemote(field.domainIndex, neighbor.index),
      body)
  }

  def wrapFragLoop(toWrap : Statement, parallel : Boolean) : Statement = {
    if (insideFragLoop)
      toWrap
    else if (parallel)
      new LoopOverFragments(toWrap) with OMP_PotentiallyParallel
    else
      new LoopOverFragments(toWrap)
  }
}

case class RemoteSends(var field : FieldSelection, var neighbors : ListBuffer[(NeighborInfo, IndexRange)], var start : Boolean, var end : Boolean,
    var concurrencyId : Int, var insideFragLoop : Boolean) extends RemoteTransfers {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = RemoteSends\n"

  def genCopy(neighbor : NeighborInfo, indices : IndexRange, addCondition : Boolean) : Statement = {
    if (Knowledge.experimental_useLevelIndepFcts || (!MPI_DataType.shouldBeUsed(indices) && SimplifyExpression.evalIntegral(indices.getSizeHigher) > 1)) {
      var body = CopyToSendBuffer(field, neighbor, indices, concurrencyId)
      if (addCondition) wrapCond(neighbor, ListBuffer[Statement](body)) else body
    } else {
      NullStatement
    }
  }

  def genTransfer(neighbor : NeighborInfo, indices : IndexRange, addCondition : Boolean) : Statement = {
    var body = {
      if (!Knowledge.experimental_useLevelIndepFcts && 1 == SimplifyExpression.evalIntegral(indices.getSizeHigher)) {
        RemoteSend(field, neighbor, s"&" ~ new DirectFieldAccess(field, indices.begin), 1, RealDatatype, concurrencyId)
      } else if (MPI_DataType.shouldBeUsed(indices)) {
        RemoteSend(field, neighbor, s"&" ~ new DirectFieldAccess(field, indices.begin), 1, MPI_DataType(field, indices), concurrencyId)
      } else {
        var cnt = DimArrayHigher().map(i => (indices.end(i) - indices.begin(i)).asInstanceOf[Expression]).reduceLeft(_ * _)
        SimplifyStrategy.doUntilDoneStandalone(cnt)
        RemoteSend(field, neighbor, iv.TmpBuffer(field.field, s"Send_${concurrencyId}", cnt, neighbor.index), cnt, RealDatatype, concurrencyId)
      }
    }
    if (addCondition) wrapCond(neighbor, ListBuffer[Statement](body)) else body
  }

  def genWait(neighbor : NeighborInfo) : Statement = {
    new WaitForTransfer(field, neighbor, s"Send_${concurrencyId}")
  }

  def expand : Output[StatementList] = {
    if (!Knowledge.domain_canHaveRemoteNeighs)
      return ListBuffer[Statement]() // nothing to do

    // TODO: think about employing neighbor loops
    //      var minIdx = neighbors.reduce((neigh, res) => if (neigh.index < res.index) neigh else res).index
    //      var maxIdx = neighbors.reduce((neigh, res) => if (neigh.index > res.index) neigh else res).index
    //        new ForLoopStatement(s"int i = $minIdx", s"i <= $maxIdx", "++i", ...)
    if (Knowledge.comm_useFragmentLoopsForEachOp)
      ListBuffer[Statement](
        if (start) wrapFragLoop(
          new ConditionStatement(iv.IsValidForSubdomain(field.domainIndex),
            neighbors.map(neigh => genCopy(neigh._1, neigh._2, true))), true)
        else NullStatement,
        if (start) wrapFragLoop(
          new ConditionStatement(iv.IsValidForSubdomain(field.domainIndex),
            neighbors.map(neigh => genTransfer(neigh._1, neigh._2, true))), true)
        else NullStatement,
        if (end) wrapFragLoop(
          new ConditionStatement(iv.IsValidForSubdomain(field.domainIndex),
            neighbors.map(neigh => genWait(neigh._1))), true)
        else NullStatement)
    else
      ListBuffer(wrapFragLoop(
        new ConditionStatement(iv.IsValidForSubdomain(field.domainIndex), neighbors.map(neigh =>
          wrapCond(neigh._1, ListBuffer(
            if (start) genCopy(neigh._1, neigh._2, false) else NullStatement,
            if (start) genTransfer(neigh._1, neigh._2, false) else NullStatement,
            if (end) genWait(neigh._1) else NullStatement)))), true))
  }
}

case class RemoteRecvs(var field : FieldSelection, var neighbors : ListBuffer[(NeighborInfo, IndexRange)], var start : Boolean, var end : Boolean,
    var concurrencyId : Int, var insideFragLoop : Boolean) extends RemoteTransfers {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = RemoteRecvs\n"

  def genCopy(neighbor : NeighborInfo, indices : IndexRange, addCondition : Boolean) : Statement = {
    if (Knowledge.experimental_useLevelIndepFcts || (!MPI_DataType.shouldBeUsed(indices) && SimplifyExpression.evalIntegral(indices.getSizeHigher) > 1)) {
      var body = CopyFromRecvBuffer(field, neighbor, indices, concurrencyId)
      if (addCondition) wrapCond(neighbor, ListBuffer[Statement](body)) else body
    } else {
      NullStatement
    }
  }

  def genTransfer(neighbor : NeighborInfo, indices : IndexRange, addCondition : Boolean) : Statement = {
    var body = {
      if (!Knowledge.experimental_useLevelIndepFcts && 1 == SimplifyExpression.evalIntegral(indices.getSizeHigher)) {
        RemoteRecv(field, neighbor, s"&" ~ new DirectFieldAccess(field, indices.begin), 1, RealDatatype, concurrencyId)
      } else if (MPI_DataType.shouldBeUsed(indices)) {
        RemoteRecv(field, neighbor, s"&" ~ new DirectFieldAccess(field, indices.begin), 1, MPI_DataType(field, indices), concurrencyId)
      } else {
        var cnt = DimArrayHigher().map(i => (indices.end(i) - indices.begin(i)).asInstanceOf[Expression]).reduceLeft(_ * _)
        SimplifyStrategy.doUntilDoneStandalone(cnt)
        RemoteRecv(field, neighbor, iv.TmpBuffer(field.field, s"Recv_${concurrencyId}", cnt, neighbor.index), cnt, RealDatatype, concurrencyId)
      }
    }
    if (addCondition) wrapCond(neighbor, ListBuffer[Statement](body)) else body
  }

  def genWait(neighbor : NeighborInfo) : Statement = {
    new WaitForTransfer(field, neighbor, s"Recv_${concurrencyId}")
  }

  def expand : Output[StatementList] = {
    if (!Knowledge.domain_canHaveRemoteNeighs)
      return ListBuffer[Statement]() // nothing to do

    // TODO: think about employing neighbor loops
    //      var minIdx = neighbors.reduce((neigh, res) => if (neigh.index < res.index) neigh else res).index
    //      var maxIdx = neighbors.reduce((neigh, res) => if (neigh.index > res.index) neigh else res).index
    //        new ForLoopStatement(s"int i = $minIdx", s"i <= $maxIdx", "++i", ...)
    if (Knowledge.comm_useFragmentLoopsForEachOp)
      ListBuffer[Statement](
        if (start) wrapFragLoop(
          new ConditionStatement(iv.IsValidForSubdomain(field.domainIndex),
            neighbors.map(neigh => genTransfer(neigh._1, neigh._2, true))), true)
        else NullStatement,
        if (end) wrapFragLoop(
          new ConditionStatement(iv.IsValidForSubdomain(field.domainIndex),
            neighbors.map(neigh => genWait(neigh._1))), true) // TODO: omp parallel or too much overhead? remove inner critical?
        else NullStatement,
        if (end) wrapFragLoop(
          new ConditionStatement(iv.IsValidForSubdomain(field.domainIndex),
            neighbors.map(neigh => genCopy(neigh._1, neigh._2, true))), true)
        else NullStatement)
    else
      ListBuffer(wrapFragLoop(
        new ConditionStatement(iv.IsValidForSubdomain(field.domainIndex), neighbors.map(neigh =>
          wrapCond(neigh._1, ListBuffer(
            if (start) genTransfer(neigh._1, neigh._2, false) else NullStatement,
            if (end) genWait(neigh._1) else NullStatement,
            if (end) genCopy(neigh._1, neigh._2, false) else NullStatement)))), true))
  }
}

case class CopyToSendBuffer(var field : FieldSelection, var neighbor : NeighborInfo, var indices : IndexRange, var concurrencyId : Int) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CopyToSendBuffer\n"

  def expand : Output[Statement] = {
    val tmpBufAccess = new ArrayAccess(iv.TmpBuffer(field.field, s"Send_${concurrencyId}", indices.getSizeHigher, neighbor.index),
      Mapping.resolveMultiIdx(new MultiIndex(LoopOverDimensions.defIt, indices.begin, _ - _), indices),
      false && Knowledge.data_alignTmpBufferPointers /* change here if aligned vector operations are possible for tmp buffers */ )
    val fieldAccess = new DirectFieldAccess(FieldSelection(field.field, field.level, field.slot), LoopOverDimensions.defIt)

    new LoopOverDimensions(Knowledge.dimensionality + 1, indices, new AssignmentStatement(tmpBufAccess, fieldAccess)) with OMP_PotentiallyParallel with PolyhedronAccessable
  }
}

case class CopyFromRecvBuffer(var field : FieldSelection, var neighbor : NeighborInfo, var indices : IndexRange, var concurrencyId : Int) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CopyFromRecvBuffer\n"

  def expand : Output[Statement] = {
    val tmpBufAccess = new ArrayAccess(iv.TmpBuffer(field.field, s"Recv_${concurrencyId}", indices.getSizeHigher, neighbor.index),
      Mapping.resolveMultiIdx(new MultiIndex(LoopOverDimensions.defIt, indices.begin, _ - _), indices),
      false && Knowledge.data_alignTmpBufferPointers /* change here if aligned vector operations are possible for tmp buffers */ )
    val fieldAccess = new DirectFieldAccess(FieldSelection(field.field, field.level, field.slot), LoopOverDimensions.defIt)

    new LoopOverDimensions(Knowledge.dimensionality + 1, indices, new AssignmentStatement(fieldAccess, tmpBufAccess)) with OMP_PotentiallyParallel with PolyhedronAccessable
  }
}

case class RemoteSend(var field : FieldSelection, var neighbor : NeighborInfo, var src : Expression, var numDataPoints : Expression, var datatype : Datatype, var concurrencyId : Int) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = RemoteSend\n"

  def expand : Output[StatementList] = {
    ListBuffer[Statement](
      new MPI_Send(src, numDataPoints, datatype, iv.NeighborRemoteRank(field.domainIndex, neighbor.index),
        GeneratedMPITag(iv.CommId(), iv.NeighborFragLocalId(field.domainIndex, neighbor.index), concurrencyId),
        iv.MpiRequest(field.field, s"Send_${concurrencyId}", neighbor.index)) with OMP_PotentiallyCritical,
      AssignmentStatement(iv.RemoteReqOutstanding(field.field, s"Send_${concurrencyId}", neighbor.index), true))
  }
}

case class RemoteRecv(var field : FieldSelection, var neighbor : NeighborInfo, var dest : Expression, var numDataPoints : Expression, var datatype : Datatype, var concurrencyId : Int) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = RemoteRecv\n"

  def expand : Output[StatementList] = {
    ListBuffer[Statement](
      new MPI_Receive(dest, numDataPoints, datatype, iv.NeighborRemoteRank(field.domainIndex, neighbor.index),
        GeneratedMPITag(iv.NeighborFragLocalId(field.domainIndex, neighbor.index), iv.CommId(), concurrencyId),
        iv.MpiRequest(field.field, s"Recv_${concurrencyId}", neighbor.index)) with OMP_PotentiallyCritical,
      AssignmentStatement(iv.RemoteReqOutstanding(field.field, s"Recv_${concurrencyId}", neighbor.index), true))
  }
}

case class WaitForTransfer(var field : FieldSelection, var neighbor : NeighborInfo, var direction : String) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = WaitForTransfer\n"

  def expand : Output[Statement] = {
    new ConditionStatement(
      iv.RemoteReqOutstanding(field.field, direction, neighbor.index),
      ListBuffer[Statement](
        new FunctionCallExpression("waitForMPIReq", AddressofExpression(iv.MpiRequest(field.field, direction, neighbor.index))),
        AssignmentStatement(iv.RemoteReqOutstanding(field.field, direction, neighbor.index), false)))
  }
}

/// special boundary functions

case class IsOnSpecBoundary(var field : FieldSelection, var neigh : NeighborInfo) extends Expression with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = IsOnSpecBoundary\n"

  override def expand() : Output[Expression] = {
    // should work for node, cell and face discretizations

    var conditions = ListBuffer[Expression](NegationExpression(iv.NeighborIsValid(field.domainIndex, neigh.index)))
    for (dim <- 0 until Knowledge.dimensionality) {
      neigh.dir(dim) match {
        case -1 => conditions += LowerExpression(LoopOverDimensions.defIt(dim), field.fieldLayout(dim).idxDupLeftEnd - field.referenceOffset(dim))
        case 1  => conditions += GreaterEqualExpression(LoopOverDimensions.defIt(dim), field.fieldLayout(dim).idxDupRightBegin - field.referenceOffset(dim))
        case 0  => // true
      }
    }

    conditions.reduce((a, b) => AndAndExpression(a, b))
  }
}

case class IsOnBoundary(var field : FieldSelection) extends Expression with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = IsOnBoundary\n"

  override def expand() : Output[Expression] = {
    var applicableNeighbors = Fragment.neighbors
    if (Knowledge.experimental_bc_checkOnlyMainAxis)
      applicableNeighbors = applicableNeighbors.filter(n => 1 == n.dir.map(d => math.abs(d)).reduce(_ + _))

    if (Knowledge.experimental_bc_avoidOrOperations)
      NegationExpression(applicableNeighbors.map(n => NegationExpression(IsOnSpecBoundary(field, n).expand.inner) : Expression).reduce((a, b) => AndAndExpression(a, b)))
    else
      applicableNeighbors.map(n => IsOnSpecBoundary(field, n).expand.inner).reduce((a, b) => OrOrExpression(a, b))
  }
}
