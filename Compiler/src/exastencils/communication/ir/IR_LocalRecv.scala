//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.domain.ir._
import exastencils.field.ir._
import exastencils.parallelization.api.omp.OMP_WaitForFlag

/// IR_LocalRecv

case class IR_LocalRecv(
    var field : IR_Field,
    var slot : IR_Expression,
    var neighbor : NeighborInfo,
    var dest : IR_ExpressionIndexRange,
    var src : IR_ExpressionIndexRange,
    var insideFragLoop : Boolean,
    var condition : Option[IR_Expression]) extends IR_Statement with IR_Expandable {

  def numDims = field.layout.numDimsData

  override def expand() : Output[IR_Statement] = {
    var innerStmt : IR_Statement = IR_Assignment(
      IR_DirectFieldAccess(field, Duplicate(slot), IR_LoopOverDimensions.defIt(numDims)),
      IR_DirectFieldAccess(field, Duplicate(slot), IR_IV_NeighborFragmentIdx(field.domain.index, neighbor.index),
        IR_ExpressionIndex(IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims), src.begin, _ + _), dest.begin, _ - _)))

    if (condition.isDefined)
      innerStmt = IR_IfCondition(condition.get, innerStmt)

    val loop = new IR_LoopOverDimensions(numDims, dest, ListBuffer[IR_Statement](innerStmt))
    loop.polyOptLevel = 1
    loop.parallelization.potentiallyParallel = true

    def loopWithCommTrafos(trafo : IR_CommTransformation) = {
      val fieldAccess = IR_DirectFieldAccess(field, Duplicate(slot), IR_LoopOverDimensions.defIt(numDims))
      val neighFieldAccess = IR_DirectFieldAccess(field, Duplicate(slot), IR_IV_NeighborFragmentIdx(field.domain.index, neighbor.index),
        IR_ExpressionIndex(IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDims), src.begin, _ + _), dest.begin, _ - _))
      val ret = new IR_LoopOverDimensions(numDims, dest, ListBuffer[IR_Statement](IR_Assignment(fieldAccess, trafo.applyLocalTrafo(neighFieldAccess, neighbor))))
      ret.polyOptLevel = 1
      ret.parallelization.potentiallyParallel = true
      ret
    }

    var ifCondStmts = ListBuffer[IR_Statement]()
    // wait until the fragment to be read from is ready for communication
    if (Knowledge.comm_enableCommTransformations) {
      ifCondStmts += IR_FunctionCall(OMP_WaitForFlag.generateFctAccess(), IR_AddressOf(IR_IV_LocalCommReady(
        field, IR_IV_CommNeighNeighIdx(field.domain.index, neighbor.index), IR_IV_NeighborFragmentIdx(field.domain.index, neighbor.index)))) // TODO replace getOpposingNeigh
      val trafoId = IR_IV_CommTrafoId(field.domain.index, neighbor.index)
      ifCondStmts += IR_Switch(trafoId, IR_CommTransformationCollection.trafos.zipWithIndex.map {
        case (trafo, i) => IR_Case(i, ListBuffer[IR_Statement](loopWithCommTrafos(trafo)))
      })
    }
    else {
      ifCondStmts += IR_FunctionCall(OMP_WaitForFlag.generateFctAccess(), IR_AddressOf(IR_IV_LocalCommReady(
        field, DefaultNeighbors.getOpposingNeigh(neighbor.index).index, IR_IV_NeighborFragmentIdx(field.domain.index, neighbor.index))))
      ifCondStmts += loop
    }
    // signal other threads that the data reading step is completed
    ifCondStmts += IR_Assignment(IR_IV_LocalCommDone(field, neighbor.index), IR_BooleanConstant(true)) // TODO here too

    IR_IfCondition(IR_IV_NeighborIsValid(field.domain.index, neighbor.index) AndAnd IR_Negation(IR_IV_NeighborIsRemote(field.domain.index, neighbor.index)),
      ifCondStmts)

  }
}

