package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication._
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir.iv
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.domain.ir._
import exastencils.field.ir.IR_DirectFieldAccess
import exastencils.polyhedron.PolyhedronAccessible
import exastencils.prettyprinting.PpStream

/// IR_LocalSend

case class IR_LocalSend(
    var field : IR_FieldSelection,
    var neighbor : NeighborInfo,
    var dest : IR_ExpressionIndexRange,
    var src : IR_ExpressionIndexRange,
    var insideFragLoop : Boolean,
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def numDims = field.field.fieldLayout.numDimsData

  override def expand() : Output[IR_Statement] = {
    var innerStmt : IR_Statement = IR_Assignment(
      IR_DirectFieldAccess(IR_FieldSelection(field.field, field.level, field.slot, None, IR_IV_NeighborFragmentIdx(field.domainIndex, neighbor.index)), IR_ExpressionIndex(
        IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims), src.begin, _ + _), dest.begin, _ - _)),
      IR_DirectFieldAccess(IR_FieldSelection(field.field, field.level, field.slot), IR_LoopOverDimensions.defIt(numDims)))

    if (condition.isDefined)
      innerStmt = IR_IfCondition(condition.get, innerStmt)

    val loop = new IR_LoopOverDimensions(numDims, dest, ListBuffer[IR_Statement](innerStmt)) with PolyhedronAccessible
    loop.parallelization.potentiallyParallel = true

    IR_IfCondition(IR_IV_NeighborIsValid(field.domainIndex, neighbor.index) AndAnd IR_NegationExpression(IR_IV_NeighborIsRemote(field.domainIndex, neighbor.index)),
      ListBuffer[IR_Statement](
        // wait until the fragment to be written to is ready for communication
        IR_FunctionCall("waitForFlag", IR_AddressofExpression(iv.LocalCommReady(field.field, DefaultNeighbors.getOpposingNeigh(neighbor.index).index, IR_IV_NeighborFragmentIdx(field.domainIndex, neighbor.index)))),
        loop,
        // signal other threads that the data reading step is completed
        IR_Assignment(iv.LocalCommDone(field.field, neighbor.index), IR_BooleanConstant(true))))
  }
}
