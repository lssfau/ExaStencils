package exastencils.communication

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.{ StatementList, _ }
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.domain.ir._
import exastencils.field.ir.IR_DirectFieldAccess
import exastencils.knowledge._
import exastencils.mpi._
import exastencils.omp._
import exastencils.polyhedron.PolyhedronAccessible
import exastencils.prettyprinting._
import exastencils.util._

/// local communication operations

abstract class LocalTransfers extends IR_Statement with IR_Expandable {
  def insideFragLoop : Boolean

  def wrapFragLoop(toWrap : IR_Statement, parallel : Boolean) : IR_Statement = {
    if (insideFragLoop)
      toWrap
    else if (parallel)
      new IR_LoopOverFragments(ListBuffer[IR_Statement](toWrap)) with OMP_PotentiallyParallel
    else
      IR_LoopOverFragments(toWrap)
  }

  def wrapFragLoop(toWrap : ListBuffer[IR_Statement], parallel : Boolean) : ListBuffer[IR_Statement] = {
    if (insideFragLoop)
      toWrap
    else if (parallel)
      ListBuffer[IR_Statement](new IR_LoopOverFragments(toWrap) with OMP_PotentiallyParallel)
    else
      ListBuffer[IR_Statement](new IR_LoopOverFragments(toWrap))
  }
}

case class StartLocalComm(var field : IR_FieldSelection,
    var sendNeighbors : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange, IR_ExpressionIndexRange)],
    var recvNeighbors : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange, IR_ExpressionIndexRange)],
    var insideFragLoop : Boolean, var cond : Option[IR_Expression]) extends LocalTransfers {

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def setLocalCommReady(neighbors : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange, IR_ExpressionIndexRange)]) : ListBuffer[IR_Statement] = {
    wrapFragLoop(
      neighbors.map(neighbor =>
        IR_IfCondition(IR_IV_NeighborIsValid(field.domainIndex, neighbor._1.index)
          AndAnd IR_NegationExpression(IR_IV_NeighborIsRemote(field.domainIndex, neighbor._1.index)),
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
        IR_IfCondition(IR_IV_IsValidForDomain(field.domainIndex),
          sendNeighbors.map(neigh => LocalSend(field, neigh._1, neigh._2, neigh._3, insideFragLoop, cond) : IR_Statement)),
        true)
    } else {
      // pull data for this fragment - otherwise
      output += wrapFragLoop(
        IR_IfCondition(IR_IV_IsValidForDomain(field.domainIndex),
          recvNeighbors.map(neigh => LocalRecv(field, neigh._1, neigh._2, neigh._3, insideFragLoop, cond) : IR_Statement)),
        true)
    }

    output
  }
}

case class FinishLocalComm(var field : IR_FieldSelection,
    var sendNeighbors : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange, IR_ExpressionIndexRange)],
    var recvNeighbors : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange, IR_ExpressionIndexRange)],
    var insideFragLoop : Boolean, var cond : Option[IR_Expression]) extends LocalTransfers {

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def waitForLocalComm(neighbors : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange, IR_ExpressionIndexRange)]) : ListBuffer[IR_Statement] = {
    wrapFragLoop(
      neighbors.map(neighbor =>
        IR_IfCondition(IR_IV_NeighborIsValid(field.domainIndex, neighbor._1.index)
          AndAnd IR_NegationExpression(IR_IV_NeighborIsRemote(field.domainIndex, neighbor._1.index)),
          ListBuffer[IR_Statement](
            IR_FunctionCall("waitForFlag", IR_AddressofExpression(iv.LocalCommDone(
              field.field,
              Fragment.getOpposingNeigh(neighbor._1).index,
              IR_IV_NeighborFragmentIdx(field.domainIndex, neighbor._1.index))))))),
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

case class LocalSend(var field : IR_FieldSelection, var neighbor : NeighborInfo, var dest : IR_ExpressionIndexRange, var src : IR_ExpressionIndexRange,
    var insideFragLoop : Boolean, var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def numDims = field.field.fieldLayout.numDimsData

  override def expand : Output[IR_Statement] = {
    var innerStmt : IR_Statement = new IR_Assignment(
      IR_DirectFieldAccess(IR_FieldSelection(field.field, field.level, field.slot, None, IR_IV_NeighborFragmentIdx(field.domainIndex, neighbor.index)), IR_ExpressionIndex(
        IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims), src.begin, _ + _), dest.begin, _ - _)),
      IR_DirectFieldAccess(IR_FieldSelection(field.field, field.level, field.slot), IR_LoopOverDimensions.defIt(numDims)))

    if (condition.isDefined)
      innerStmt = IR_IfCondition(condition.get, innerStmt)

    IR_IfCondition(IR_IV_NeighborIsValid(field.domainIndex, neighbor.index) AndAnd IR_NegationExpression(IR_IV_NeighborIsRemote(field.domainIndex, neighbor.index)),
      ListBuffer[IR_Statement](
        // wait until the fragment to be written to is ready for communication
        IR_FunctionCall("waitForFlag", IR_AddressofExpression(iv.LocalCommReady(field.field, Fragment.getOpposingNeigh(neighbor.index).index, IR_IV_NeighborFragmentIdx(field.domainIndex, neighbor.index)))),
        new IR_LoopOverDimensions(numDims, dest, ListBuffer[IR_Statement](innerStmt)) with OMP_PotentiallyParallel with PolyhedronAccessible,
        // signal other threads that the data reading step is completed
        IR_Assignment(iv.LocalCommDone(field.field, neighbor.index), IR_BooleanConstant(true))))
  }
}

case class LocalRecv(var field : IR_FieldSelection, var neighbor : NeighborInfo, var dest : IR_ExpressionIndexRange, var src : IR_ExpressionIndexRange,
    var insideFragLoop : Boolean, var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def numDims = field.field.fieldLayout.numDimsData

  override def expand : Output[IR_Statement] = {
    var innerStmt : IR_Statement = IR_Assignment(
      IR_DirectFieldAccess(IR_FieldSelection(field.field, field.level, field.slot), IR_LoopOverDimensions.defIt(numDims)),
      IR_DirectFieldAccess(IR_FieldSelection(field.field, field.level, field.slot, None, IR_IV_NeighborFragmentIdx(field.domainIndex, neighbor.index)),
        IR_ExpressionIndex(IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims), src.begin, _ + _), dest.begin, _ - _)))

    if (condition.isDefined)
      innerStmt = IR_IfCondition(condition.get, innerStmt)

    IR_IfCondition(IR_IV_NeighborIsValid(field.domainIndex, neighbor.index) AndAnd IR_NegationExpression(IR_IV_NeighborIsRemote(field.domainIndex, neighbor.index)),
      ListBuffer[IR_Statement](
        // wait until the fragment to be read from is ready for communication
        IR_FunctionCall("waitForFlag", IR_AddressofExpression(iv.LocalCommReady(field.field, Fragment.getOpposingNeigh(neighbor.index).index, IR_IV_NeighborFragmentIdx(field.domainIndex, neighbor.index)))),
        new IR_LoopOverDimensions(numDims, dest, ListBuffer[IR_Statement](innerStmt)) with OMP_PotentiallyParallel with PolyhedronAccessible,
        // signal other threads that the data reading step is completed
        IR_Assignment(iv.LocalCommDone(field.field, neighbor.index), IR_BooleanConstant(true))))
  }
}

/// remote communication operations

abstract class RemoteTransfers extends IR_Statement with IR_Expandable {
  def field : IR_FieldSelection
  def neighbors : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange)]

  def insideFragLoop : Boolean

  def genCopy(neighbor : NeighborInfo, indices : IR_ExpressionIndexRange, addCondition : Boolean) : IR_Statement
  def genTransfer(neighbor : NeighborInfo, indices : IR_ExpressionIndexRange, addCondition : Boolean) : IR_Statement

  def wrapCond(neighbor : NeighborInfo, body : ListBuffer[IR_Statement]) : IR_Statement = {
    IR_IfCondition(IR_IV_NeighborIsValid(field.domainIndex, neighbor.index) AndAnd IR_IV_NeighborIsRemote(field.domainIndex, neighbor.index),
      body)
  }

  def wrapFragLoop(toWrap : IR_Statement, parallel : Boolean) : IR_Statement = {
    if (insideFragLoop)
      toWrap
    else if (parallel)
      new IR_LoopOverFragments(ListBuffer[IR_Statement](toWrap)) with OMP_PotentiallyParallel
    else
      IR_LoopOverFragments(toWrap)
  }
}

case class RemoteSends(var field : IR_FieldSelection, var neighbors : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange)], var start : Boolean, var end : Boolean,
    var concurrencyId : Int, var insideFragLoop : Boolean, var condition : Option[IR_Expression]) extends RemoteTransfers {

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def genCopy(neighbor : NeighborInfo, indices : IR_ExpressionIndexRange, addCondition : Boolean) : IR_Statement = {
    if (Knowledge.data_genVariableFieldSizes || (!MPI_DataType.shouldBeUsed(indices, condition) && SimplifyExpression.evalIntegral(indices.getTotalSize) > 1)) {
      var body = CopyToSendBuffer(field, neighbor, indices, concurrencyId, condition)
      if (addCondition) wrapCond(neighbor, ListBuffer[IR_Statement](body)) else body
    } else {
      IR_NullStatement
    }
  }

  override def genTransfer(neighbor : NeighborInfo, indices : IR_ExpressionIndexRange, addCondition : Boolean) : IR_Statement = {
    var body = {
      val maxCnt = indices.getTotalSize
      val cnt = (if (condition.isDefined)
        iv.TmpBufferIterator(field.field, s"Send_${ concurrencyId }", neighbor.index)
      else
        maxCnt)
      if (!Knowledge.data_genVariableFieldSizes && (condition.isEmpty && 1 == SimplifyExpression.evalIntegral(cnt))) {
        RemoteSend(field, neighbor, IR_AddressofExpression(new IR_DirectFieldAccess(field, indices.begin)), 1, IR_RealDatatype, concurrencyId)
      } else if (MPI_DataType.shouldBeUsed(indices, condition)) {
        RemoteSend(field, neighbor, IR_AddressofExpression(new IR_DirectFieldAccess(field, indices.begin)), 1, MPI_DataType(field, indices, condition), concurrencyId)
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
          IR_IfCondition(IR_IV_IsValidForDomain(field.domainIndex),
            neighbors.map(neigh => genCopy(neigh._1, neigh._2, true))), true)
        else IR_NullStatement,
        if (start) wrapFragLoop(
          IR_IfCondition(IR_IV_IsValidForDomain(field.domainIndex),
            neighbors.map(neigh => genTransfer(neigh._1, neigh._2, true))), true)
        else IR_NullStatement,
        if (end) wrapFragLoop(
          IR_IfCondition(IR_IV_IsValidForDomain(field.domainIndex),
            neighbors.map(neigh => genWait(neigh._1))), true)
        else IR_NullStatement)
    else
      ListBuffer(wrapFragLoop(
        IR_IfCondition(IR_IV_IsValidForDomain(field.domainIndex), neighbors.map(neigh =>
          wrapCond(neigh._1, ListBuffer(
            if (start) genCopy(neigh._1, neigh._2, false) else IR_NullStatement,
            if (start) genTransfer(neigh._1, neigh._2, false) else IR_NullStatement,
            if (end) genWait(neigh._1) else IR_NullStatement)))), true))
  }
}

case class RemoteRecvs(var field : IR_FieldSelection, var neighbors : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange)], var start : Boolean, var end : Boolean,
    var concurrencyId : Int, var insideFragLoop : Boolean, var condition : Option[IR_Expression]) extends RemoteTransfers {

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def genCopy(neighbor : NeighborInfo, indices : IR_ExpressionIndexRange, addCondition : Boolean) : IR_Statement = {
    if (Knowledge.data_genVariableFieldSizes || (!MPI_DataType.shouldBeUsed(indices, condition) && SimplifyExpression.evalIntegral(indices.getTotalSize) > 1)) {
      var body = CopyFromRecvBuffer(field, neighbor, indices, concurrencyId, condition)
      if (addCondition) wrapCond(neighbor, ListBuffer[IR_Statement](body)) else body
    } else {
      IR_NullStatement
    }
  }

  override def genTransfer(neighbor : NeighborInfo, indices : IR_ExpressionIndexRange, addCondition : Boolean) : IR_Statement = {
    var body = {
      val maxCnt = indices.getTotalSize
      val cnt = maxCnt // always cnt, even when condition is defined -> max count for receive
      if (!Knowledge.data_genVariableFieldSizes && 1 == SimplifyExpression.evalIntegral(cnt)) {
        RemoteRecv(field, neighbor, IR_AddressofExpression(new IR_DirectFieldAccess(field, indices.begin)), 1, IR_RealDatatype, concurrencyId)
      } else if (MPI_DataType.shouldBeUsed(indices, condition)) {
        RemoteRecv(field, neighbor, IR_AddressofExpression(new IR_DirectFieldAccess(field, indices.begin)), 1, MPI_DataType(field, indices, condition), concurrencyId)
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
          IR_IfCondition(IR_IV_IsValidForDomain(field.domainIndex),
            neighbors.map(neigh => genTransfer(neigh._1, neigh._2, true))), true)
        else IR_NullStatement,
        if (end) wrapFragLoop(
          IR_IfCondition(IR_IV_IsValidForDomain(field.domainIndex),
            neighbors.map(neigh => genWait(neigh._1))), true) // TODO: omp parallel or too much overhead? remove inner critical?
        else IR_NullStatement,
        if (end) wrapFragLoop(
          IR_IfCondition(IR_IV_IsValidForDomain(field.domainIndex),
            neighbors.map(neigh => genCopy(neigh._1, neigh._2, true))), true)
        else IR_NullStatement)
    else
      ListBuffer(wrapFragLoop(
        IR_IfCondition(IR_IV_IsValidForDomain(field.domainIndex), neighbors.map(neigh =>
          wrapCond(neigh._1, ListBuffer(
            if (start) genTransfer(neigh._1, neigh._2, false) else IR_NullStatement,
            if (end) genWait(neigh._1) else IR_NullStatement,
            if (end) genCopy(neigh._1, neigh._2, false) else IR_NullStatement)))), true))
  }
}

case class CopyToSendBuffer(var field : IR_FieldSelection, var neighbor : NeighborInfo, var indices : IR_ExpressionIndexRange,
    var concurrencyId : Int, var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def numDims = field.field.fieldLayout.numDimsData

  override def expand : Output[StatementList] = {
    var ret = ListBuffer[IR_Statement]()

    if (condition.isDefined) {
      // switch to iterator based copy operation if condition is defined -> number of elements and index mapping is unknown
      def it = iv.TmpBufferIterator(field.field, s"Send_${ concurrencyId }", neighbor.index)

      val tmpBufAccess = new IR_TempBufferAccess(iv.TmpBuffer(field.field, s"Send_${ concurrencyId }", indices.getTotalSize, neighbor.index),
        IR_ExpressionIndex(it), IR_ExpressionIndex(0) /* dummy stride */)
      val fieldAccess = new IR_DirectFieldAccess(IR_FieldSelection(field.field, field.level, field.slot), IR_LoopOverDimensions.defIt(numDims))

      ret += IR_Assignment(it, 0)
      ret += IR_LoopOverDimensions(numDims, indices, IR_IfCondition(
        condition.get, ListBuffer[IR_Statement](
          IR_Assignment(tmpBufAccess, fieldAccess),
          IR_Assignment(it, 1, "+="))))
    } else {
      val tmpBufAccess = new IR_TempBufferAccess(iv.TmpBuffer(field.field, s"Send_${ concurrencyId }", indices.getTotalSize, neighbor.index),
        IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims), indices.begin, _ - _),
        IR_ExpressionIndex(indices.end, indices.begin, _ - _))
      val fieldAccess = new IR_DirectFieldAccess(IR_FieldSelection(field.field, field.level, field.slot), IR_LoopOverDimensions.defIt(numDims))
      ret += new IR_LoopOverDimensions(numDims, indices, ListBuffer[IR_Statement](IR_Assignment(tmpBufAccess, fieldAccess))) with OMP_PotentiallyParallel with PolyhedronAccessible
    }

    ret
  }
}

case class CopyFromRecvBuffer(var field : IR_FieldSelection, var neighbor : NeighborInfo, var indices : IR_ExpressionIndexRange,
    var concurrencyId : Int, var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def numDims = field.field.fieldLayout.numDimsData

  override def expand : Output[StatementList] = {
    var ret = ListBuffer[IR_Statement]()

    if (condition.isDefined) {
      // switch to iterator based copy operation if condition is defined -> number of elements and index mapping is unknown
      def it = iv.TmpBufferIterator(field.field, s"Recv_${ concurrencyId }", neighbor.index)

      val tmpBufAccess = new IR_TempBufferAccess(iv.TmpBuffer(field.field, s"Recv_${ concurrencyId }", indices.getTotalSize, neighbor.index),
        IR_ExpressionIndex(it), IR_ExpressionIndex(0) /* dummy stride */)
      val fieldAccess = new IR_DirectFieldAccess(IR_FieldSelection(field.field, field.level, field.slot), IR_LoopOverDimensions.defIt(numDims))

      ret += IR_Assignment(it, 0)
      ret += IR_LoopOverDimensions(numDims, indices, IR_IfCondition(
        condition.get, ListBuffer[IR_Statement](
          IR_Assignment(fieldAccess, tmpBufAccess),
          IR_Assignment(it, 1, "+="))))
    } else {
      val tmpBufAccess = new IR_TempBufferAccess(iv.TmpBuffer(field.field, s"Recv_${ concurrencyId }", indices.getTotalSize, neighbor.index),
        IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims), indices.begin, _ - _),
        IR_ExpressionIndex(indices.end, indices.begin, _ - _))
      val fieldAccess = new IR_DirectFieldAccess(IR_FieldSelection(field.field, field.level, field.slot), IR_LoopOverDimensions.defIt(numDims))

      ret += new IR_LoopOverDimensions(numDims, indices, ListBuffer[IR_Statement](IR_Assignment(fieldAccess, tmpBufAccess))) with OMP_PotentiallyParallel with PolyhedronAccessible
    }

    ret
  }
}

case class RemoteSend(var field : IR_FieldSelection, var neighbor : NeighborInfo, var src : IR_Expression, var numDataPoints : IR_Expression, var datatype : IR_Datatype, var concurrencyId : Int) extends IR_Statement with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand : Output[StatementList] = {
    ListBuffer[IR_Statement](
      new MPI_Send(src, numDataPoints, datatype, IR_IV_NeighborRemoteRank(field.domainIndex, neighbor.index),
        GeneratedMPITag(iv.CommId(), IR_IV_NeighborFragmentIdx(field.domainIndex, neighbor.index), neighbor.index, concurrencyId),
        iv.MpiRequest(field.field, s"Send_${ concurrencyId }", neighbor.index)) with OMP_PotentiallyCritical,
      IR_Assignment(iv.RemoteReqOutstanding(field.field, s"Send_${ concurrencyId }", neighbor.index), true))
  }
}

case class RemoteRecv(var field : IR_FieldSelection, var neighbor : NeighborInfo, var dest : IR_Expression, var numDataPoints : IR_Expression, var datatype : IR_Datatype, var concurrencyId : Int) extends IR_Statement with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand : Output[StatementList] = {
    ListBuffer[IR_Statement](
      new MPI_Receive(dest, numDataPoints, datatype, IR_IV_NeighborRemoteRank(field.domainIndex, neighbor.index),
        GeneratedMPITag(IR_IV_NeighborFragmentIdx(field.domainIndex, neighbor.index), iv.CommId(),
          Fragment.getOpposingNeigh(neighbor.index).index, concurrencyId),
        iv.MpiRequest(field.field, s"Recv_${ concurrencyId }", neighbor.index)) with OMP_PotentiallyCritical,
      IR_Assignment(iv.RemoteReqOutstanding(field.field, s"Recv_${ concurrencyId }", neighbor.index), true))
  }
}

case class WaitForTransfer(var field : IR_FieldSelection, var neighbor : NeighborInfo, var direction : String) extends IR_Statement with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand : Output[IR_Statement] = {
    IR_IfCondition(
      iv.RemoteReqOutstanding(field.field, direction, neighbor.index),
      ListBuffer[IR_Statement](
        IR_FunctionCall("waitForMPIReq", IR_AddressofExpression(iv.MpiRequest(field.field, direction, neighbor.index))),
        IR_Assignment(iv.RemoteReqOutstanding(field.field, direction, neighbor.index), false)))
  }
}

/// special boundary functions

case class IsOnSpecBoundary(var field : IR_FieldSelection, var neigh : NeighborInfo, var index : IR_ExpressionIndex) extends IR_Expression with IR_Expandable {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand() : Output[IR_Expression] = {
    // should work for node, cell and face discretizations

    var conditions = ListBuffer[IR_Expression](IR_NegationExpression(IR_IV_NeighborIsValid(field.domainIndex, neigh.index)))
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

case class IsOnBoundary(var field : IR_FieldSelection, var index : IR_ExpressionIndex) extends IR_Expression with IR_Expandable {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

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
case class IsValidPoint(var field : IR_FieldSelection, var index : IR_ExpressionIndex) extends IR_Expression with IR_Expandable {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

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

