package exastencils.communication

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir.{ StatementList, _ }
import exastencils.knowledge._
import exastencils.mpi._
import exastencils.omp._
import exastencils.polyhedron._
import exastencils.prettyprinting._
import exastencils.util._

case class CommunicateTarget(var target : String, var begin : Option[IR_ExpressionIndex], var end : Option[IR_ExpressionIndex]) extends IR_Expression {
  if (begin.isDefined && !end.isDefined) // create end if only one 'index' is to be communicated
    end = Some(Duplicate(begin.get) + IR_ExpressionIndex(Array.fill(begin.get.length)(1)))

  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CommunicateTarget\n"
}

case class CommunicateStatement(var field : FieldSelection, var op : String, var targets : ListBuffer[CommunicateTarget], var condition : Option[IR_Expression]) extends IR_Statement {

  object ShiftIndexAccesses extends QuietDefaultStrategy("Shifting index accesses") {
    this += new Transformation("SearchAndReplace", {
      case s : IR_StringLiteral   => {
        var ret : IR_Expression = s
        val numDims = field.field.fieldLayout.numDimsData
        for (dim <- 0 until numDims)
          if (dimToString(dim) == s.value)
            ret = IR_VariableAccess(dimToString(dim), Some(IR_IntegerDatatype)) - field.field.referenceOffset(dim)
        ret
      }
      case va : IR_VariableAccess => {
        var ret : IR_Expression = va
        val numDims = field.field.fieldLayout.numDimsData
        for (dim <- 0 until numDims)
          if (dimToString(dim) == va.name)
            ret = IR_VariableAccess(dimToString(dim), Some(IR_IntegerDatatype)) - field.field.referenceOffset(dim)
        ret
      }
    }, false)
  }

  // shift all index accesses in condition as later functions will generate direct field accesses and according loop bounds
  if (condition.isDefined) ShiftIndexAccesses.applyStandalone(IR_ExpressionStatement(condition.get))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CommunicateStatement\n"
}

case class ApplyBCsStatement(var field : FieldSelection) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = ApplyBCsStatement\n"
}

/// local communication operations

abstract class LocalTransfers extends IR_Statement with Expandable {
  def insideFragLoop : Boolean

  def wrapFragLoop(toWrap : IR_Statement, parallel : Boolean) : IR_Statement = {
    if (insideFragLoop)
      toWrap
    else if (parallel)
      new LoopOverFragments(toWrap) with OMP_PotentiallyParallel
    else
      new LoopOverFragments(toWrap)
  }

  def wrapFragLoop(toWrap : ListBuffer[IR_Statement], parallel : Boolean) : ListBuffer[IR_Statement] = {
    if (insideFragLoop)
      toWrap
    else if (parallel)
      ListBuffer[IR_Statement](new LoopOverFragments(toWrap) with OMP_PotentiallyParallel)
    else
      ListBuffer[IR_Statement](new LoopOverFragments(toWrap))
  }
}

case class StartLocalComm(var field : FieldSelection,
    var sendNeighbors : ListBuffer[(NeighborInfo, IndexRange, IndexRange)],
    var recvNeighbors : ListBuffer[(NeighborInfo, IndexRange, IndexRange)],
    var insideFragLoop : Boolean, var cond : Option[IR_Expression]) extends LocalTransfers {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = StartLocalComm\n"

  def setLocalCommReady(neighbors : ListBuffer[(NeighborInfo, IndexRange, IndexRange)]) : ListBuffer[IR_Statement] = {
    wrapFragLoop(
      neighbors.map(neighbor =>
        IR_IfCondition(iv.NeighborIsValid(field.domainIndex, neighbor._1.index)
          AndAnd IR_NegationExpression(iv.NeighborIsRemote(field.domainIndex, neighbor._1.index)),
          IR_Assignment(iv.LocalCommReady(field.field, neighbor._1.index), IR_BooleanConstant(true)))),
      true)
  }

  override def expand : Output[StatementList] = {
    var output = ListBuffer[IR_Statement]()

    if (!Knowledge.domain_canHaveLocalNeighs) return output // nothing to do

    // set LocalCommReady to signal neighbors readiness for communication
    if (!Knowledge.comm_pushLocalData)
      output ++= setLocalCommReady(sendNeighbors)
    else
      output ++= setLocalCommReady(recvNeighbors)

    if (Knowledge.comm_pushLocalData) {
      // distribute this fragment's data - if enabled
      output += wrapFragLoop(
        IR_IfCondition(iv.IsValidForSubdomain(field.domainIndex),
          sendNeighbors.map(neigh => LocalSend(field, neigh._1, neigh._2, neigh._3, insideFragLoop, cond) : IR_Statement)),
        true)
    } else {
      // pull data for this fragment - otherwise
      output += wrapFragLoop(
        IR_IfCondition(iv.IsValidForSubdomain(field.domainIndex),
          recvNeighbors.map(neigh => LocalRecv(field, neigh._1, neigh._2, neigh._3, insideFragLoop, cond) : IR_Statement)),
        true)
    }

    output
  }
}

case class FinishLocalComm(var field : FieldSelection,
    var sendNeighbors : ListBuffer[(NeighborInfo, IndexRange, IndexRange)],
    var recvNeighbors : ListBuffer[(NeighborInfo, IndexRange, IndexRange)],
    var insideFragLoop : Boolean, var cond : Option[IR_Expression]) extends LocalTransfers {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = FinishLocalComm\n"

  def waitForLocalComm(neighbors : ListBuffer[(NeighborInfo, IndexRange, IndexRange)]) : ListBuffer[IR_Statement] = {
    wrapFragLoop(
      neighbors.map(neighbor =>
        IR_IfCondition(iv.NeighborIsValid(field.domainIndex, neighbor._1.index)
          AndAnd IR_NegationExpression(iv.NeighborIsRemote(field.domainIndex, neighbor._1.index)),
          ListBuffer[IR_Statement](
            new FunctionCallExpression("waitForFlag", IR_AddressofExpression(iv.LocalCommDone(
              field.field,
              Fragment.getOpposingNeigh(neighbor._1).index,
              iv.NeighborFragLocalId(field.domainIndex, neighbor._1.index))))))),
      true)
  }

  override def expand : Output[StatementList] = {
    var output = ListBuffer[IR_Statement]()

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
    var insideFragLoop : Boolean, var condition : Option[IR_Expression]) extends IR_Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LocalSend\n"

  def numDims = field.field.fieldLayout.numDimsData

  override def expand : Output[IR_Statement] = {
    var innerStmt : IR_Statement = new IR_Assignment(
      new DirectFieldAccess(FieldSelection(field.field, field.level, field.slot, None, iv.NeighborFragLocalId(field.domainIndex, neighbor.index)), IR_ExpressionIndex(
        IR_ExpressionIndex(LoopOverDimensions.defIt(numDims), src.begin, _ + _), dest.begin, _ - _)),
      new DirectFieldAccess(FieldSelection(field.field, field.level, field.slot), LoopOverDimensions.defIt(numDims)))

    if (condition.isDefined)
      innerStmt = IR_IfCondition(condition.get, innerStmt)

    IR_IfCondition(iv.NeighborIsValid(field.domainIndex, neighbor.index) AndAnd IR_NegationExpression(iv.NeighborIsRemote(field.domainIndex, neighbor.index)),
      ListBuffer[IR_Statement](
        // wait until the fragment to be written to is ready for communication
        new FunctionCallExpression("waitForFlag", IR_AddressofExpression(iv.LocalCommReady(field.field, Fragment.getOpposingNeigh(neighbor.index).index, iv.NeighborFragLocalId(field.domainIndex, neighbor.index)))),
        new LoopOverDimensions(numDims, dest, innerStmt) with OMP_PotentiallyParallel with PolyhedronAccessible,
        // signal other threads that the data reading step is completed
        IR_Assignment(iv.LocalCommDone(field.field, neighbor.index), IR_BooleanConstant(true))))
  }
}

case class LocalRecv(var field : FieldSelection, var neighbor : NeighborInfo, var dest : IndexRange, var src : IndexRange,
    var insideFragLoop : Boolean, var condition : Option[IR_Expression]) extends IR_Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LocalRecv\n"

  def numDims = field.field.fieldLayout.numDimsData

  override def expand : Output[IR_Statement] = {
    var innerStmt : IR_Statement = IR_Assignment(
      DirectFieldAccess(FieldSelection(field.field, field.level, field.slot), LoopOverDimensions.defIt(numDims)),
      DirectFieldAccess(FieldSelection(field.field, field.level, field.slot, None, iv.NeighborFragLocalId(field.domainIndex, neighbor.index)),
        IR_ExpressionIndex(IR_ExpressionIndex(LoopOverDimensions.defIt(numDims), src.begin, _ + _), dest.begin, _ - _)))

    if (condition.isDefined)
      innerStmt = IR_IfCondition(condition.get, innerStmt)

    IR_IfCondition(iv.NeighborIsValid(field.domainIndex, neighbor.index) AndAnd IR_NegationExpression(iv.NeighborIsRemote(field.domainIndex, neighbor.index)),
      ListBuffer[IR_Statement](
        // wait until the fragment to be read from is ready for communication
        new FunctionCallExpression("waitForFlag", IR_AddressofExpression(iv.LocalCommReady(field.field, Fragment.getOpposingNeigh(neighbor.index).index, iv.NeighborFragLocalId(field.domainIndex, neighbor.index)))),
        new LoopOverDimensions(numDims, dest, innerStmt) with OMP_PotentiallyParallel with PolyhedronAccessible,
        // signal other threads that the data reading step is completed
        IR_Assignment(iv.LocalCommDone(field.field, neighbor.index), IR_BooleanConstant(true))))
  }
}

/// remote communication operations

abstract class RemoteTransfers extends IR_Statement with Expandable {
  def field : FieldSelection
  def neighbors : ListBuffer[(NeighborInfo, IndexRange)]

  def insideFragLoop : Boolean

  def genCopy(neighbor : NeighborInfo, indices : IndexRange, addCondition : Boolean) : IR_Statement
  def genTransfer(neighbor : NeighborInfo, indices : IndexRange, addCondition : Boolean) : IR_Statement

  def wrapCond(neighbor : NeighborInfo, body : ListBuffer[IR_Statement]) : IR_Statement = {
    IR_IfCondition(iv.NeighborIsValid(field.domainIndex, neighbor.index) AndAnd iv.NeighborIsRemote(field.domainIndex, neighbor.index),
      body)
  }

  def wrapFragLoop(toWrap : IR_Statement, parallel : Boolean) : IR_Statement = {
    if (insideFragLoop)
      toWrap
    else if (parallel)
      new LoopOverFragments(toWrap) with OMP_PotentiallyParallel
    else
      new LoopOverFragments(toWrap)
  }
}

case class RemoteSends(var field : FieldSelection, var neighbors : ListBuffer[(NeighborInfo, IndexRange)], var start : Boolean, var end : Boolean,
    var concurrencyId : Int, var insideFragLoop : Boolean, var condition : Option[IR_Expression]) extends RemoteTransfers {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = RemoteSends\n"

  override def genCopy(neighbor : NeighborInfo, indices : IndexRange, addCondition : Boolean) : IR_Statement = {
    if (Knowledge.data_genVariableFieldSizes || (!MPI_DataType.shouldBeUsed(indices, condition) && SimplifyExpression.evalIntegral(indices.getTotalSize) > 1)) {
      var body = CopyToSendBuffer(field, neighbor, indices, concurrencyId, condition)
      if (addCondition) wrapCond(neighbor, ListBuffer[IR_Statement](body)) else body
    } else {
      IR_NullStatement
    }
  }

  override def genTransfer(neighbor : NeighborInfo, indices : IndexRange, addCondition : Boolean) : IR_Statement = {
    var body = {
      val maxCnt = indices.getTotalSize
      val cnt = (if (condition.isDefined)
        iv.TmpBufferIterator(field.field, s"Send_${ concurrencyId }", neighbor.index)
      else
        maxCnt)
      if (!Knowledge.data_genVariableFieldSizes && (condition.isEmpty && 1 == SimplifyExpression.evalIntegral(cnt))) {
        RemoteSend(field, neighbor, IR_AddressofExpression(new DirectFieldAccess(field, indices.begin)), 1, IR_RealDatatype, concurrencyId)
      } else if (MPI_DataType.shouldBeUsed(indices, condition)) {
        RemoteSend(field, neighbor, IR_AddressofExpression(new DirectFieldAccess(field, indices.begin)), 1, MPI_DataType(field, indices, condition), concurrencyId)
      } else {
        RemoteSend(field, neighbor, iv.TmpBuffer(field.field, s"Send_${ concurrencyId }", maxCnt, neighbor.index), cnt, IR_RealDatatype, concurrencyId)
      }
    }
    if (addCondition) wrapCond(neighbor, ListBuffer[IR_Statement](body)) else body
  }

  def genWait(neighbor : NeighborInfo) : IR_Statement = {
    new WaitForTransfer(field, neighbor, s"Send_${ concurrencyId }")
  }

  override def expand : Output[StatementList] = {
    if (!Knowledge.domain_canHaveRemoteNeighs)
      return ListBuffer[IR_Statement]() // nothing to do

    // TODO: think about employing neighbor loops
    //      var minIdx = neighbors.reduce((neigh, res) => if (neigh.index < res.index) neigh else res).index
    //      var maxIdx = neighbors.reduce((neigh, res) => if (neigh.index > res.index) neigh else res).index
    //        new ForLoopStatement(s"int i = $minIdx", s"i <= $maxIdx", "++i", ...)
    if (Knowledge.comm_useFragmentLoopsForEachOp)
      ListBuffer[IR_Statement](
        if (start) wrapFragLoop(
          IR_IfCondition(iv.IsValidForSubdomain(field.domainIndex),
            neighbors.map(neigh => genCopy(neigh._1, neigh._2, true))), true)
        else IR_NullStatement,
        if (start) wrapFragLoop(
          IR_IfCondition(iv.IsValidForSubdomain(field.domainIndex),
            neighbors.map(neigh => genTransfer(neigh._1, neigh._2, true))), true)
        else IR_NullStatement,
        if (end) wrapFragLoop(
          IR_IfCondition(iv.IsValidForSubdomain(field.domainIndex),
            neighbors.map(neigh => genWait(neigh._1))), true)
        else IR_NullStatement)
    else
      ListBuffer(wrapFragLoop(
        IR_IfCondition(iv.IsValidForSubdomain(field.domainIndex), neighbors.map(neigh =>
          wrapCond(neigh._1, ListBuffer(
            if (start) genCopy(neigh._1, neigh._2, false) else IR_NullStatement,
            if (start) genTransfer(neigh._1, neigh._2, false) else IR_NullStatement,
            if (end) genWait(neigh._1) else IR_NullStatement)))), true))
  }
}

case class RemoteRecvs(var field : FieldSelection, var neighbors : ListBuffer[(NeighborInfo, IndexRange)], var start : Boolean, var end : Boolean,
    var concurrencyId : Int, var insideFragLoop : Boolean, var condition : Option[IR_Expression]) extends RemoteTransfers {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = RemoteRecvs\n"

  override def genCopy(neighbor : NeighborInfo, indices : IndexRange, addCondition : Boolean) : IR_Statement = {
    if (Knowledge.data_genVariableFieldSizes || (!MPI_DataType.shouldBeUsed(indices, condition) && SimplifyExpression.evalIntegral(indices.getTotalSize) > 1)) {
      var body = CopyFromRecvBuffer(field, neighbor, indices, concurrencyId, condition)
      if (addCondition) wrapCond(neighbor, ListBuffer[IR_Statement](body)) else body
    } else {
      IR_NullStatement
    }
  }

  override def genTransfer(neighbor : NeighborInfo, indices : IndexRange, addCondition : Boolean) : IR_Statement = {
    var body = {
      val maxCnt = indices.getTotalSize
      val cnt = maxCnt // always cnt, even when condition is defined -> max count for receive
      if (!Knowledge.data_genVariableFieldSizes && 1 == SimplifyExpression.evalIntegral(cnt)) {
        RemoteRecv(field, neighbor, IR_AddressofExpression(new DirectFieldAccess(field, indices.begin)), 1, IR_RealDatatype, concurrencyId)
      } else if (MPI_DataType.shouldBeUsed(indices, condition)) {
        RemoteRecv(field, neighbor, IR_AddressofExpression(new DirectFieldAccess(field, indices.begin)), 1, MPI_DataType(field, indices, condition), concurrencyId)
      } else {
        RemoteRecv(field, neighbor, iv.TmpBuffer(field.field, s"Recv_${ concurrencyId }", maxCnt, neighbor.index), cnt, IR_RealDatatype, concurrencyId)
      }
    }
    if (addCondition) wrapCond(neighbor, ListBuffer[IR_Statement](body)) else body
  }

  def genWait(neighbor : NeighborInfo) : IR_Statement = {
    new WaitForTransfer(field, neighbor, s"Recv_${ concurrencyId }")
  }

  override def expand : Output[StatementList] = {
    if (!Knowledge.domain_canHaveRemoteNeighs)
      return ListBuffer[IR_Statement]() // nothing to do

    // TODO: think about employing neighbor loops
    //      var minIdx = neighbors.reduce((neigh, res) => if (neigh.index < res.index) neigh else res).index
    //      var maxIdx = neighbors.reduce((neigh, res) => if (neigh.index > res.index) neigh else res).index
    //        new ForLoopStatement(s"int i = $minIdx", s"i <= $maxIdx", "++i", ...)
    if (Knowledge.comm_useFragmentLoopsForEachOp)
      ListBuffer[IR_Statement](
        if (start) wrapFragLoop(
          IR_IfCondition(iv.IsValidForSubdomain(field.domainIndex),
            neighbors.map(neigh => genTransfer(neigh._1, neigh._2, true))), true)
        else IR_NullStatement,
        if (end) wrapFragLoop(
          IR_IfCondition(iv.IsValidForSubdomain(field.domainIndex),
            neighbors.map(neigh => genWait(neigh._1))), true) // TODO: omp parallel or too much overhead? remove inner critical?
        else IR_NullStatement,
        if (end) wrapFragLoop(
          IR_IfCondition(iv.IsValidForSubdomain(field.domainIndex),
            neighbors.map(neigh => genCopy(neigh._1, neigh._2, true))), true)
        else IR_NullStatement)
    else
      ListBuffer(wrapFragLoop(
        IR_IfCondition(iv.IsValidForSubdomain(field.domainIndex), neighbors.map(neigh =>
          wrapCond(neigh._1, ListBuffer(
            if (start) genTransfer(neigh._1, neigh._2, false) else IR_NullStatement,
            if (end) genWait(neigh._1) else IR_NullStatement,
            if (end) genCopy(neigh._1, neigh._2, false) else IR_NullStatement)))), true))
  }
}

case class CopyToSendBuffer(var field : FieldSelection, var neighbor : NeighborInfo, var indices : IndexRange,
    var concurrencyId : Int, var condition : Option[IR_Expression]) extends IR_Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CopyToSendBuffer\n"

  def numDims = field.field.fieldLayout.numDimsData

  override def expand : Output[StatementList] = {
    var ret = ListBuffer[IR_Statement]()

    if (condition.isDefined) {
      // switch to iterator based copy operation if condition is defined -> number of elements and index mapping is unknown
      def it = iv.TmpBufferIterator(field.field, s"Send_${ concurrencyId }", neighbor.index)

      val tmpBufAccess = new TempBufferAccess(iv.TmpBuffer(field.field, s"Send_${ concurrencyId }", indices.getTotalSize, neighbor.index),
        IR_ExpressionIndex(it), IR_ExpressionIndex(0) /* dummy stride */)
      val fieldAccess = new DirectFieldAccess(FieldSelection(field.field, field.level, field.slot), LoopOverDimensions.defIt(numDims))

      ret += IR_Assignment(it, 0)
      ret += new LoopOverDimensions(numDims, indices, IR_IfCondition(
        condition.get, ListBuffer[IR_Statement](
          IR_Assignment(tmpBufAccess, fieldAccess),
          IR_Assignment(it, 1, "+="))))
    } else {
      val tmpBufAccess = new TempBufferAccess(iv.TmpBuffer(field.field, s"Send_${ concurrencyId }", indices.getTotalSize, neighbor.index),
        IR_ExpressionIndex(LoopOverDimensions.defIt(numDims), indices.begin, _ - _),
        IR_ExpressionIndex(indices.end, indices.begin, _ - _))
      val fieldAccess = new DirectFieldAccess(FieldSelection(field.field, field.level, field.slot), LoopOverDimensions.defIt(numDims))
      ret += new LoopOverDimensions(numDims, indices, IR_Assignment(tmpBufAccess, fieldAccess)) with OMP_PotentiallyParallel with PolyhedronAccessible
    }

    ret
  }
}

case class CopyFromRecvBuffer(var field : FieldSelection, var neighbor : NeighborInfo, var indices : IndexRange,
    var concurrencyId : Int, var condition : Option[IR_Expression]) extends IR_Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CopyFromRecvBuffer\n"

  def numDims = field.field.fieldLayout.numDimsData

  override def expand : Output[StatementList] = {
    var ret = ListBuffer[IR_Statement]()

    if (condition.isDefined) {
      // switch to iterator based copy operation if condition is defined -> number of elements and index mapping is unknown
      def it = iv.TmpBufferIterator(field.field, s"Recv_${ concurrencyId }", neighbor.index)

      val tmpBufAccess = new TempBufferAccess(iv.TmpBuffer(field.field, s"Recv_${ concurrencyId }", indices.getTotalSize, neighbor.index),
        IR_ExpressionIndex(it), IR_ExpressionIndex(0) /* dummy stride */)
      val fieldAccess = new DirectFieldAccess(FieldSelection(field.field, field.level, field.slot), LoopOverDimensions.defIt(numDims))

      ret += IR_Assignment(it, 0)
      ret += new LoopOverDimensions(numDims, indices, IR_IfCondition(
        condition.get, ListBuffer[IR_Statement](
          IR_Assignment(fieldAccess, tmpBufAccess),
          IR_Assignment(it, 1, "+="))))
    } else {
      val tmpBufAccess = new TempBufferAccess(iv.TmpBuffer(field.field, s"Recv_${ concurrencyId }", indices.getTotalSize, neighbor.index),
        IR_ExpressionIndex(LoopOverDimensions.defIt(numDims), indices.begin, _ - _),
        IR_ExpressionIndex(indices.end, indices.begin, _ - _))
      val fieldAccess = new DirectFieldAccess(FieldSelection(field.field, field.level, field.slot), LoopOverDimensions.defIt(numDims))

      ret += new LoopOverDimensions(numDims, indices, IR_Assignment(fieldAccess, tmpBufAccess)) with OMP_PotentiallyParallel with PolyhedronAccessible
    }

    ret
  }
}

case class RemoteSend(var field : FieldSelection, var neighbor : NeighborInfo, var src : IR_Expression, var numDataPoints : IR_Expression, var datatype : IR_Datatype, var concurrencyId : Int) extends IR_Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = RemoteSend\n"

  override def expand : Output[StatementList] = {
    ListBuffer[IR_Statement](
      new MPI_Send(src, numDataPoints, datatype, iv.NeighborRemoteRank(field.domainIndex, neighbor.index),
        GeneratedMPITag(iv.CommId(), iv.NeighborFragLocalId(field.domainIndex, neighbor.index), neighbor.index, concurrencyId),
        iv.MpiRequest(field.field, s"Send_${ concurrencyId }", neighbor.index)) with OMP_PotentiallyCritical,
      IR_Assignment(iv.RemoteReqOutstanding(field.field, s"Send_${ concurrencyId }", neighbor.index), true))
  }
}

case class RemoteRecv(var field : FieldSelection, var neighbor : NeighborInfo, var dest : IR_Expression, var numDataPoints : IR_Expression, var datatype : IR_Datatype, var concurrencyId : Int) extends IR_Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = RemoteRecv\n"

  override def expand : Output[StatementList] = {
    ListBuffer[IR_Statement](
      new MPI_Receive(dest, numDataPoints, datatype, iv.NeighborRemoteRank(field.domainIndex, neighbor.index),
        GeneratedMPITag(iv.NeighborFragLocalId(field.domainIndex, neighbor.index), iv.CommId(),
          Fragment.getOpposingNeigh(neighbor.index).index, concurrencyId),
        iv.MpiRequest(field.field, s"Recv_${ concurrencyId }", neighbor.index)) with OMP_PotentiallyCritical,
      IR_Assignment(iv.RemoteReqOutstanding(field.field, s"Recv_${ concurrencyId }", neighbor.index), true))
  }
}

case class WaitForTransfer(var field : FieldSelection, var neighbor : NeighborInfo, var direction : String) extends IR_Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = WaitForTransfer\n"

  override def expand : Output[IR_Statement] = {
    IR_IfCondition(
      iv.RemoteReqOutstanding(field.field, direction, neighbor.index),
      ListBuffer[IR_Statement](
        new FunctionCallExpression("waitForMPIReq", IR_AddressofExpression(iv.MpiRequest(field.field, direction, neighbor.index))),
        IR_Assignment(iv.RemoteReqOutstanding(field.field, direction, neighbor.index), false)))
  }
}

/// special boundary functions

case class IsOnSpecBoundary(var field : FieldSelection, var neigh : NeighborInfo, var index : IR_ExpressionIndex) extends IR_Expression with Expandable {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = IsOnSpecBoundary\n"

  override def expand() : Output[IR_Expression] = {
    // should work for node, cell and face discretizations

    var conditions = ListBuffer[IR_Expression](IR_NegationExpression(iv.NeighborIsValid(field.domainIndex, neigh.index)))
    for (dim <- 0 until field.field.fieldLayout.numDimsGrid) {
      neigh.dir(dim) match {
        case -1 => conditions += IR_LowerExpression(index(dim), field.fieldLayout.idxById("DLE", dim) - field.referenceOffset(dim))
        case 1  => conditions += IR_GreaterEqualExpression(index(dim), field.fieldLayout.idxById("DRB", dim) - field.referenceOffset(dim))
        case 0  => // true
      }
    }

    conditions.reduce((a, b) => IR_AndAndExpression(a, b))
  }
}

case class IsOnBoundary(var field : FieldSelection, var index : IR_ExpressionIndex) extends IR_Expression with Expandable {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = IsOnBoundary\n"

  override def expand() : Output[IR_Expression] = {
    var applicableNeighbors = Fragment.neighbors
    if (Knowledge.experimental_bc_checkOnlyMainAxis)
      applicableNeighbors = applicableNeighbors.filter(n => 1 == n.dir.map(d => math.abs(d)).reduce(_ + _))

    if (Knowledge.experimental_bc_avoidOrOperations)
      IR_NegationExpression(applicableNeighbors.map(n => IR_NegationExpression(IsOnSpecBoundary(field, n, index).expand.inner) : IR_Expression).reduce((a, b) => IR_AndAndExpression(a, b)))
    else
      applicableNeighbors.map(n => IsOnSpecBoundary(field, n, index).expand.inner).reduce((a, b) => IR_OrOrExpression(a, b))
  }
}

/// checks for IsOnBoundary as well as if outside inner/dup layers on fragment transitions
case class IsValidPoint(var field : FieldSelection, var index : IR_ExpressionIndex) extends IR_Expression with Expandable {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = IsValidPoint\n"

  override def expand() : Output[IR_Expression] = {
    var applicableNeighbors = Fragment.neighbors
    if (Knowledge.experimental_bc_checkOnlyMainAxis)
      applicableNeighbors = applicableNeighbors.filter(n => 1 == n.dir.map(d => math.abs(d)).reduce(_ + _))

    val isNotOnBoundary =
      if (Knowledge.experimental_bc_avoidOrOperations)
        applicableNeighbors.map(n => IR_NegationExpression(IsOnSpecBoundary(field, n, index).expand.inner) : IR_Expression).reduce((a, b) => IR_AndAndExpression(a, b))
      else
        IR_NegationExpression(applicableNeighbors.map(n => IsOnSpecBoundary(field, n, index).expand.inner).reduce((a, b) => IR_OrOrExpression(a, b)))

    val isInnerOrDup =
      (0 until field.field.fieldLayout.numDimsGrid).map(dim =>
        IR_AndAndExpression(
          IR_LowerExpression(index(dim), field.fieldLayout.idxById("DRE", dim) - field.referenceOffset(dim)),
          IR_GreaterEqualExpression(index(dim), field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim)))).reduce((a, b) => IR_AndAndExpression(a, b))

    IR_AndAndExpression(isNotOnBoundary, isInnerOrDup)
  }
}

