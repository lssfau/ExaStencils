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

package exastencils.boundary.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.NeighborInfo
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.domain.ir._
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.grid.ir._
import exastencils.parallelization.ir.IR_ParallelizationInfo

/// IR_HandleBoundaries

abstract class IR_HandleBoundariesLike extends IR_Statement with IR_Expandable {

  def field : IR_FieldLike
  def slot : IR_Expression
  def fragIdx : IR_Expression
  def neighbors : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange)]

  def numDims = field.layout.numDimsGrid

  def constructLoops : IR_Statement

  def setupFieldUpdate(neigh : NeighborInfo) : ListBuffer[IR_Statement] = {

    // apply local trafo and replace boundaryCoord
    val strat = QuietDefaultStrategy("ResolveBoundaryCoordinates")
    strat += new Transformation("SearchAndReplace", {
      case virtualField @ IR_VirtualFieldAccess(target : IR_VF_BoundaryPositionPerDim, index, fragIdx) =>
        val evalDim = target.dim

        field.layout.localization match {
          // TODO: adapt for grids that are not axis-parallel
          case IR_AtNode | IR_AtFaceCenter(`evalDim`) =>
            virtualField.target = IR_VF_NodePositionPerDim.find(target.level, evalDim)

          case IR_AtCellCenter | IR_AtFaceCenter(_) =>
            if (0 == neigh.dir(evalDim)) { // simple projection
              virtualField.target = IR_VF_CellCenterPerDim.find(target.level, evalDim)
            } else if (neigh.dir(evalDim) < 0) { // snap to left boundary
              virtualField.target = IR_VF_NodePositionPerDim.find(target.level, evalDim)
            } else { // snap to right boundary
              virtualField.target = IR_VF_NodePositionPerDim.find(target.level, evalDim)
              virtualField.index = IR_GridUtil.offsetIndex(virtualField.index, 1, evalDim)
            }
        }
        virtualField
      //Grid.getGridObject.invokeAccessResolve(virtualField)
    })

    val bc = Duplicate(field.boundary)
    strat.applyStandalone(IR_Root(bc))

    bc.generateFieldUpdate(field, slot, fragIdx, neigh)
  }


  override def expand() : Output[IR_Statement] = {
    field.boundary match {
      case _ : IR_NeumannBC                => constructLoops
      case _ : IR_DirichletBC              => constructLoops
      case IR_FunctionBC(boundaryFunction) => boundaryFunction : IR_Statement
      case IR_NoBC                         => IR_NullStatement
    }
  }
}

case class IR_HandleBoundaries(
    var field : IR_FieldLike,
    var slot : IR_Expression,
    var fragIdx : IR_Expression,
    var neighbors : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange)]) extends IR_HandleBoundariesLike {

  def constructLoops = {
    val layout = field.layout
    val indexOfRefinedNeighbor : Option[Int] = None

    IR_LoopOverFragments(
      ListBuffer[IR_Statement](IR_IfCondition(IR_IV_IsValidForDomain(field.domain.index),
        neighbors.map({ neigh =>
          val adaptedIndexRange = IR_ExpressionIndexRange(neigh._2.begin - field.referenceOffset, neigh._2.end - field.referenceOffset)
          // TODO: assumes equal bc's for all components
          adaptedIndexRange.begin.indices ++= (layout.numDimsGrid until numDims).map(dim => 0 : IR_Expression)
          adaptedIndexRange.end.indices ++= (layout.numDimsGrid until numDims).map(dim => layout.idxById("TOT", dim))
          val loopOverDims = new IR_LoopOverDimensions(
            numDims,
            adaptedIndexRange,
            setupFieldUpdate(neigh._1))
          loopOverDims.parallelization.gpuParallelizable = field.gpuCompatible
          loopOverDims.parallelization.potentiallyParallel = true
          loopOverDims.polyOptLevel = 1
          IR_IfCondition(IR_Negation(IR_IV_NeighborIsValid(field.domain.index, neigh._1.index, indexOfRefinedNeighbor)), loopOverDims) : IR_Statement
        }))), IR_ParallelizationInfo(potentiallyParallel = true))
  }
}
