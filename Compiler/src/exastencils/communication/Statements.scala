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
  if (begin.isDefined && !end.isDefined) // create end if only one 'index' is to be communicated
    end = Some(Duplicate(begin.get) + new MultiIndex(Array.fill(begin.get.length)(1)))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CommunicateTarget\n"
}

case class CommunicateStatement(var field : FieldSelection, var op : String, var targets : ListBuffer[CommunicateTarget], condition : Option[Expression]) extends Statement {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CommunicateStatement\n"
}

case class ApplyBCsStatement(var field : FieldSelection) extends Statement {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = ApplyBCsStatement\n"
}

/// local communication operations

abstract class LocalTransfers extends Statement with Expandable {
  def insideFragLoop : Boolean

  def wrapFragLoop(toWrap : Statement, parallel : Boolean) : Statement = {
    if (insideFragLoop)
      toWrap
    else if (parallel)
      new LoopOverFragments(toWrap) with OMP_PotentiallyParallel
    else
      new LoopOverFragments(toWrap)
  }

  def wrapFragLoop(toWrap : ListBuffer[Statement], parallel : Boolean) : ListBuffer[Statement] = {
    if (insideFragLoop)
      toWrap
    else if (parallel)
      ListBuffer[Statement](new LoopOverFragments(toWrap) with OMP_PotentiallyParallel)
    else
      ListBuffer[Statement](new LoopOverFragments(toWrap))
  }
}

case class StartLocalComm(var field : FieldSelection,
    var sendNeighbors : ListBuffer[(NeighborInfo, IndexRange, IndexRange)],
    var recvNeighbors : ListBuffer[(NeighborInfo, IndexRange, IndexRange)],
    var insideFragLoop : Boolean) extends LocalTransfers {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = StartLocalComm\n"

  def setLocalCommReady(neighbors : ListBuffer[(NeighborInfo, IndexRange, IndexRange)]) : ListBuffer[Statement] = {
    wrapFragLoop(
      neighbors.map(neighbor =>
        new ConditionStatement(iv.NeighborIsValid(field.domainIndex, neighbor._1.index)
          AndAnd NegationExpression(iv.NeighborIsRemote(field.domainIndex, neighbor._1.index)),
          AssignmentStatement(iv.LocalCommReady(field.field, neighbor._1.index), BooleanConstant(true)))),
      true)
  }

  override def expand : Output[StatementList] = {
    var output = ListBuffer[Statement]()

    if (!Knowledge.domain_canHaveLocalNeighs) return output // nothing to do

    // set LocalCommReady to signal neighbors readiness for communication
    if (!Knowledge.comm_pushLocalData)
      output ++= setLocalCommReady(sendNeighbors)
    else
      output ++= setLocalCommReady(recvNeighbors)

    if (Knowledge.comm_pushLocalData) {
      // distribute this fragment's data - if enabled
      output += wrapFragLoop(
        new ConditionStatement(iv.IsValidForSubdomain(field.domainIndex),
          sendNeighbors.map(neigh => LocalSend(field, neigh._1, neigh._2, neigh._3, insideFragLoop) : Statement)),
        true)
    } else {
      // pull data for this fragment - otherwise
      output += wrapFragLoop(
        new ConditionStatement(iv.IsValidForSubdomain(field.domainIndex),
          recvNeighbors.map(neigh => LocalRecv(field, neigh._1, neigh._2, neigh._3, insideFragLoop) : Statement)),
        true)
    }

    output
  }
}

case class FinishLocalComm(var field : FieldSelection,
    var sendNeighbors : ListBuffer[(NeighborInfo, IndexRange, IndexRange)],
    var recvNeighbors : ListBuffer[(NeighborInfo, IndexRange, IndexRange)],
    var insideFragLoop : Boolean) extends LocalTransfers {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = StartLocalComm\n"

  def waitForLocalComm(neighbors : ListBuffer[(NeighborInfo, IndexRange, IndexRange)]) : ListBuffer[Statement] = {
    wrapFragLoop(
      neighbors.map(neighbor =>
        new ConditionStatement(iv.NeighborIsValid(field.domainIndex, neighbor._1.index)
          AndAnd NegationExpression(iv.NeighborIsRemote(field.domainIndex, neighbor._1.index)),
          ListBuffer[Statement](
            new FunctionCallExpression("waitForFlag", AddressofExpression(iv.LocalCommDone(
              field.field,
              Fragment.getOpposingNeigh(neighbor._1).index,
              iv.NeighborFragLocalId(field.domainIndex, neighbor._1.index))))))),
      true)
  }

  override def expand : Output[StatementList] = {
    var output = ListBuffer[Statement]()

    if (!Knowledge.domain_canHaveLocalNeighs) return output // nothing to do

    // wait until all neighbors signal that they are finished
    if (!Knowledge.comm_pushLocalData)
      output ++= waitForLocalComm(sendNeighbors)
    else
      output ++= waitForLocalComm(recvNeighbors)

    output
  }
}

case class LocalSend(var field : FieldSelection, var neighbor : NeighborInfo, var dest : IndexRange, var src : IndexRange,
    var insideFragLoop : Boolean) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LocalSend\n"

  def numDims = field.field.fieldLayout.numDimsData

  override def expand : Output[Statement] = {
    new ConditionStatement(iv.NeighborIsValid(field.domainIndex, neighbor.index) AndAnd NegationExpression(iv.NeighborIsRemote(field.domainIndex, neighbor.index)),
      ListBuffer[Statement](
        // wait until the fragment to be written to is ready for communication
        new FunctionCallExpression("waitForFlag", AddressofExpression(iv.LocalCommReady(field.field, Fragment.getOpposingNeigh(neighbor.index).index, iv.NeighborFragLocalId(field.domainIndex, neighbor.index)))),
        new LoopOverDimensions(numDims,
          dest,
          new AssignmentStatement(
            new DirectFieldAccess(FieldSelection(field.field, field.level, field.slot, None, iv.NeighborFragLocalId(field.domainIndex, neighbor.index)), new MultiIndex(
              new MultiIndex(LoopOverDimensions.defIt(numDims), src.begin, _ + _), dest.begin, _ - _)),
            new DirectFieldAccess(FieldSelection(field.field, field.level, field.slot), LoopOverDimensions.defIt(numDims)))) with OMP_PotentiallyParallel with PolyhedronAccessable,
        // signal other threads that the data reading step is completed
        AssignmentStatement(iv.LocalCommDone(field.field, neighbor.index), BooleanConstant(true))))
  }
}

case class LocalRecv(var field : FieldSelection, var neighbor : NeighborInfo, var dest : IndexRange, var src : IndexRange,
    var insideFragLoop : Boolean) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LocalRecv\n"

  def numDims = field.field.fieldLayout.numDimsData

  override def expand : Output[Statement] = {
    new ConditionStatement(iv.NeighborIsValid(field.domainIndex, neighbor.index) AndAnd NegationExpression(iv.NeighborIsRemote(field.domainIndex, neighbor.index)),
      ListBuffer[Statement](
        // wait until the fragment to be read from is ready for communication
        new FunctionCallExpression("waitForFlag", AddressofExpression(iv.LocalCommReady(field.field, Fragment.getOpposingNeigh(neighbor.index).index, iv.NeighborFragLocalId(field.domainIndex, neighbor.index)))),
        new LoopOverDimensions(numDims,
          dest,
          AssignmentStatement(
            DirectFieldAccess(FieldSelection(field.field, field.level, field.slot), LoopOverDimensions.defIt(numDims)),
            DirectFieldAccess(FieldSelection(field.field, field.level, field.slot, None, iv.NeighborFragLocalId(field.domainIndex, neighbor.index)),
              new MultiIndex(new MultiIndex(LoopOverDimensions.defIt(numDims), src.begin, _ + _), dest.begin, _ - _)))) with OMP_PotentiallyParallel with PolyhedronAccessable,
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

  override def genCopy(neighbor : NeighborInfo, indices : IndexRange, addCondition : Boolean) : Statement = {
    if (Knowledge.data_genVariableFieldSizes || (!MPI_DataType.shouldBeUsed(indices) && SimplifyExpression.evalIntegral(indices.getTotalSize) > 1)) {
      var body = CopyToSendBuffer(field, neighbor, indices, concurrencyId)
      if (addCondition) wrapCond(neighbor, ListBuffer[Statement](body)) else body
    } else {
      NullStatement
    }
  }

  override def genTransfer(neighbor : NeighborInfo, indices : IndexRange, addCondition : Boolean) : Statement = {
    var body = {
      val cnt = indices.getTotalSize
      if (!Knowledge.data_genVariableFieldSizes && 1 == SimplifyExpression.evalIntegral(cnt)) {
        RemoteSend(field, neighbor, s"&" ~ new DirectFieldAccess(field, indices.begin), 1, RealDatatype, concurrencyId)
      } else if (MPI_DataType.shouldBeUsed(indices)) {
        RemoteSend(field, neighbor, s"&" ~ new DirectFieldAccess(field, indices.begin), 1, MPI_DataType(field, indices), concurrencyId)
      } else {
        RemoteSend(field, neighbor, iv.TmpBuffer(field.field, s"Send_${concurrencyId}", cnt, neighbor.index), cnt, RealDatatype, concurrencyId)
      }
    }
    if (addCondition) wrapCond(neighbor, ListBuffer[Statement](body)) else body
  }

  def genWait(neighbor : NeighborInfo) : Statement = {
    new WaitForTransfer(field, neighbor, s"Send_${concurrencyId}")
  }

  override def expand : Output[StatementList] = {
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

  override def genCopy(neighbor : NeighborInfo, indices : IndexRange, addCondition : Boolean) : Statement = {
    if (Knowledge.data_genVariableFieldSizes || (!MPI_DataType.shouldBeUsed(indices) && SimplifyExpression.evalIntegral(indices.getTotalSize) > 1)) {
      var body = CopyFromRecvBuffer(field, neighbor, indices, concurrencyId)
      if (addCondition) wrapCond(neighbor, ListBuffer[Statement](body)) else body
    } else {
      NullStatement
    }
  }

  override def genTransfer(neighbor : NeighborInfo, indices : IndexRange, addCondition : Boolean) : Statement = {
    var body = {
      val cnt = indices.getTotalSize
      if (!Knowledge.data_genVariableFieldSizes && 1 == SimplifyExpression.evalIntegral(cnt)) {
        RemoteRecv(field, neighbor, s"&" ~ new DirectFieldAccess(field, indices.begin), 1, RealDatatype, concurrencyId)
      } else if (MPI_DataType.shouldBeUsed(indices)) {
        RemoteRecv(field, neighbor, s"&" ~ new DirectFieldAccess(field, indices.begin), 1, MPI_DataType(field, indices), concurrencyId)
      } else {
        RemoteRecv(field, neighbor, iv.TmpBuffer(field.field, s"Recv_${concurrencyId}", cnt, neighbor.index), cnt, RealDatatype, concurrencyId)
      }
    }
    if (addCondition) wrapCond(neighbor, ListBuffer[Statement](body)) else body
  }

  def genWait(neighbor : NeighborInfo) : Statement = {
    new WaitForTransfer(field, neighbor, s"Recv_${concurrencyId}")
  }

  override def expand : Output[StatementList] = {
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

  def numDims = field.field.fieldLayout.numDimsData

  override def expand : Output[Statement] = {
    val tmpBufAccess = new TempBufferAccess(iv.TmpBuffer(field.field, s"Send_${concurrencyId}", indices.getTotalSize, neighbor.index),
      new MultiIndex(LoopOverDimensions.defIt(numDims), indices.begin, _ - _),
      new MultiIndex(indices.end, indices.begin, _ - _))
    val fieldAccess = new DirectFieldAccess(FieldSelection(field.field, field.level, field.slot), LoopOverDimensions.defIt(numDims))

    new LoopOverDimensions(numDims, indices, new AssignmentStatement(tmpBufAccess, fieldAccess)) with OMP_PotentiallyParallel with PolyhedronAccessable
  }
}

case class CopyFromRecvBuffer(var field : FieldSelection, var neighbor : NeighborInfo, var indices : IndexRange, var concurrencyId : Int) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CopyFromRecvBuffer\n"

  def numDims = field.field.fieldLayout.numDimsData

  override def expand : Output[Statement] = {
    val tmpBufAccess = new TempBufferAccess(iv.TmpBuffer(field.field, s"Recv_${concurrencyId}", indices.getTotalSize, neighbor.index),
      new MultiIndex(LoopOverDimensions.defIt(numDims), indices.begin, _ - _),
      new MultiIndex(indices.end, indices.begin, _ - _))
    val fieldAccess = new DirectFieldAccess(FieldSelection(field.field, field.level, field.slot), LoopOverDimensions.defIt(numDims))

    new LoopOverDimensions(numDims, indices, new AssignmentStatement(fieldAccess, tmpBufAccess)) with OMP_PotentiallyParallel with PolyhedronAccessable
  }
}

case class RemoteSend(var field : FieldSelection, var neighbor : NeighborInfo, var src : Expression, var numDataPoints : Expression, var datatype : Datatype, var concurrencyId : Int) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = RemoteSend\n"

  override def expand : Output[StatementList] = {
    ListBuffer[Statement](
      new MPI_Send(src, numDataPoints, datatype, iv.NeighborRemoteRank(field.domainIndex, neighbor.index),
        GeneratedMPITag(iv.CommId(), iv.NeighborFragLocalId(field.domainIndex, neighbor.index), concurrencyId),
        iv.MpiRequest(field.field, s"Send_${concurrencyId}", neighbor.index)) with OMP_PotentiallyCritical,
      AssignmentStatement(iv.RemoteReqOutstanding(field.field, s"Send_${concurrencyId}", neighbor.index), true))
  }
}

case class RemoteRecv(var field : FieldSelection, var neighbor : NeighborInfo, var dest : Expression, var numDataPoints : Expression, var datatype : Datatype, var concurrencyId : Int) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = RemoteRecv\n"

  override def expand : Output[StatementList] = {
    ListBuffer[Statement](
      new MPI_Receive(dest, numDataPoints, datatype, iv.NeighborRemoteRank(field.domainIndex, neighbor.index),
        GeneratedMPITag(iv.NeighborFragLocalId(field.domainIndex, neighbor.index), iv.CommId(), concurrencyId),
        iv.MpiRequest(field.field, s"Recv_${concurrencyId}", neighbor.index)) with OMP_PotentiallyCritical,
      AssignmentStatement(iv.RemoteReqOutstanding(field.field, s"Recv_${concurrencyId}", neighbor.index), true))
  }
}

case class WaitForTransfer(var field : FieldSelection, var neighbor : NeighborInfo, var direction : String) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = WaitForTransfer\n"

  override def expand : Output[Statement] = {
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
    for (dim <- 0 until field.field.fieldLayout.numDimsGrid) {
      neigh.dir(dim) match {
        case -1 => conditions += LowerExpression(LoopOverDimensions.defIt(Knowledge.dimensionality)(dim), field.fieldLayout.idxById("DLE", dim) - field.referenceOffset(dim))
        case 1  => conditions += GreaterEqualExpression(LoopOverDimensions.defIt(Knowledge.dimensionality)(dim), field.fieldLayout.idxById("DRB", dim) - field.referenceOffset(dim))
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
