package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.domain.ir._
import exastencils.field.ir.IR_DirectFieldAccess
import exastencils.parallelization.api.omp.OMP_WaitForFlag

/// IR_LocalRecv

case class IR_LocalRecv(
    var field : IR_FieldSelection,
    var neighbor : NeighborInfo,
    var dest : IR_ExpressionIndexRange,
    var src : IR_ExpressionIndexRange,
    var insideFragLoop : Boolean,
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  def numDims = field.field.fieldLayout.numDimsData

  override def expand() : Output[IR_Statement] = {
    var innerStmt : IR_Statement = IR_Assignment(
      IR_DirectFieldAccess(IR_FieldSelection(field.field, field.level, Duplicate(field.slot)), IR_LoopOverDimensions.defIt(numDims)),
      IR_DirectFieldAccess(IR_FieldSelection(field.field, field.level, Duplicate(field.slot), IR_IV_NeighborFragmentIdx(field.domainIndex, neighbor.index)),
        IR_ExpressionIndex(IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims), src.begin, _ + _), dest.begin, _ - _)))

    if (condition.isDefined)
      innerStmt = IR_IfCondition(condition.get, innerStmt)

    val loop = new IR_LoopOverDimensions(numDims, dest, ListBuffer[IR_Statement](innerStmt))
    loop.polyOptLevel = 1
    loop.parallelization.potentiallyParallel = true

    def loopWithCommTrafos(trafo : IR_CommTransformation) = {
      val fieldAccess = IR_DirectFieldAccess(IR_FieldSelection(field.field, field.level, Duplicate(field.slot)), IR_LoopOverDimensions.defIt(numDims))
      val neighFieldAccess = IR_DirectFieldAccess(IR_FieldSelection(field.field, field.level, Duplicate(field.slot), IR_IV_NeighborFragmentIdx(field.domainIndex, neighbor.index)),
        IR_ExpressionIndex(IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims), src.begin, _ + _), dest.begin, _ - _))
      val ret = new IR_LoopOverDimensions(numDims, dest, ListBuffer[IR_Statement](IR_Assignment(fieldAccess, trafo.applyLocalTrafo(neighFieldAccess, dest, neighbor))))
      ret.polyOptLevel = 1
      ret.parallelization.potentiallyParallel = true
      ret
    }

    var ifCondStmts = ListBuffer[IR_Statement]()
    // wait until the fragment to be read from is ready for communication
    if (Knowledge.comm_enableCommTransformations) {
      ifCondStmts += IR_FunctionCall(OMP_WaitForFlag.generateFctAccess(), IR_AddressOf(IR_IV_LocalCommReady(
        field.field, IR_IV_CommNeighIdx(field.domainIndex, neighbor.index), IR_IV_NeighborFragmentIdx(field.domainIndex, neighbor.index)))) // TODO replace getOpposingNeigh
      val trafoId = IR_IV_CommTrafoId(field.domainIndex, neighbor.index)
      ifCondStmts += IR_Switch(trafoId, IR_CommTransformationCollection.trafos.zipWithIndex.map {
        case (trafo, i) => IR_Case(i, ListBuffer[IR_Statement](loopWithCommTrafos(trafo)))
      })
    }
    else {
      ifCondStmts += IR_FunctionCall(OMP_WaitForFlag.generateFctAccess(), IR_AddressOf(IR_IV_LocalCommReady(
        field.field, DefaultNeighbors.getOpposingNeigh(neighbor.index).index, IR_IV_NeighborFragmentIdx(field.domainIndex, neighbor.index))))
      ifCondStmts += loop
    }
    // signal other threads that the data reading step is completed
    ifCondStmts += IR_Assignment(IR_IV_LocalCommDone(field.field, neighbor.index), IR_BooleanConstant(true)) // TODO here too

    IR_IfCondition(IR_IV_NeighborIsValid(field.domainIndex, neighbor.index) AndAnd IR_Negation(IR_IV_NeighborIsRemote(field.domainIndex, neighbor.index)),
      ifCondStmts)

  }
}

