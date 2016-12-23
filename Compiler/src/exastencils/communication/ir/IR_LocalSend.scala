package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.domain.ir._
import exastencils.field.ir.IR_DirectFieldAccess
import exastencils.polyhedron.PolyhedronAccessible

/// IR_LocalSend

case class IR_LocalSend(
    var field : IR_FieldSelection,
    var neighbor : NeighborInfo,
    var dest : IR_ExpressionIndexRange,
    var src : IR_ExpressionIndexRange,
    var insideFragLoop : Boolean,
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  def numDims = field.field.fieldLayout.numDimsData

  override def expand() : Output[IR_Statement] = {
    var innerStmt : IR_Statement = IR_Assignment(
      IR_DirectFieldAccess(IR_FieldSelection(field.field, field.level, Duplicate(field.slot), None, IR_IV_NeighborFragmentIdx(field.domainIndex, neighbor.index)), IR_ExpressionIndex(
        IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims), src.begin, _ + _), dest.begin, _ - _)),
      IR_DirectFieldAccess(IR_FieldSelection(field.field, field.level, Duplicate(field.slot)), IR_LoopOverDimensions.defIt(numDims)))

    if (condition.isDefined)
      innerStmt = IR_IfCondition(condition.get, innerStmt)

    val loop = new IR_LoopOverDimensions(numDims, dest, ListBuffer[IR_Statement](innerStmt)) with PolyhedronAccessible
    loop.parallelization.potentiallyParallel = true

    IR_IfCondition(IR_IV_NeighborIsValid(field.domainIndex, neighbor.index) AndAnd IR_Negation(IR_IV_NeighborIsRemote(field.domainIndex, neighbor.index)),
      ListBuffer[IR_Statement](
        // wait until the fragment to be written to is ready for communication
        IR_FunctionCall("waitForFlag", IR_AddressOf(IR_IV_LocalCommReady(field.field, DefaultNeighbors.getOpposingNeigh(neighbor.index).index, IR_IV_NeighborFragmentIdx(field.domainIndex, neighbor.index)))),
        loop,
        // signal other threads that the data reading step is completed
        IR_Assignment(IR_IV_LocalCommDone(field.field, neighbor.index), IR_BooleanConstant(true))))
  }
}
