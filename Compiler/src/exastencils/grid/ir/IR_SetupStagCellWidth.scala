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

package exastencils.grid.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.DefaultNeighbors
import exastencils.communication.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.ir._
import exastencils.fieldlike.ir.IR_FieldLikeAccess
import exastencils.logger.Logger

/// IR_SetupStagCellWidth

object IR_SetupStagCellWidth {
  /// node_pos        -> nodes of the original grid
  /// o   o   o   o   o
  /// cell_width      -> width of the control volumes of the original grid
  /// |---|   |---|
  /// stag_cv_width   -> width of the staggered control volumes
  /// |-|   |---|   |-|

  val indexOfRefinedNeighbor : Option[IR_Expression] = None

  def for_AA(level : Int, dim : Int) : ListBuffer[IR_Statement] = {
    if (IR_DomainCollection.objects.size > 1) Logger.warn("More than one domain is currently not supported for non-uniform grids; defaulting to global domain")

    val domain = IR_DomainCollection.getByIdentifier("global").get
    val numDims = domain.numDims

    val numCellsPerFrag = (1 << level) * Knowledge.domain_fragmentLengthAsVec(dim)
    val numCellsTotal = numCellsPerFrag * Knowledge.domain_rect_numFragsTotalAsVec(dim)

    // look up field and compile access to base element
    val baseIndex = IR_LoopOverDimensions.defIt(numDims)
    val field = IR_VF_StagCellWidthPerDim.find(level, dim, dim).associatedField
    val baseAccess = IR_FieldLikeAccess(field, 0, Duplicate(baseIndex))
    val npField = IR_VF_NodePositionPerDim.find(level, dim).associatedField
    val npBaseAccess = IR_FieldLikeAccess(npField, 0, Duplicate(baseIndex))

    // fix the inner iterator -> used for zone checks
    def innerIt =
      if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
        IR_LoopOverDimensions.defItForDim(dim)
      else
        IR_VariableAccess(s"global_i$dim", IR_IntegerDatatype)

    val innerItDecl =
      if (Knowledge.domain_rect_numFragsTotalAsVec(dim) <= 1)
        IR_NullStatement
      else
        IR_VariableDeclaration(innerIt, IR_LoopOverDimensions.defItForDim(dim) + IR_IV_FragmentIndex(dim) * numCellsPerFrag)

    // compile special boundary handling expressions
    val leftDir = Array.fill(numDims)(0)
    leftDir(dim) = -1
    val leftNeighIndex = DefaultNeighbors.getNeigh(leftDir).index

    val leftGhostIndex = IR_ExpressionIndex(Array.fill(numDims)(0))
    leftGhostIndex(dim) = -2
    val leftGhostAccess = IR_FieldLikeAccess(field, 0, leftGhostIndex)

    val leftBoundaryUpdate = IR_IfCondition(
      IR_Negation(IR_IV_NeighborIsValid(field.domain.index, leftNeighIndex, indexOfRefinedNeighbor)),
      ListBuffer[IR_Statement](
        IR_Assignment(IR_GridUtil.offsetAccess(leftGhostAccess, 1, dim), IR_GridUtil.offsetAccess(leftGhostAccess, 2, dim)),
        IR_Assignment(Duplicate(leftGhostAccess), IR_GridUtil.offsetAccess(leftGhostAccess, 1, dim))))

    val rightDir = Array.fill(numDims)(0)
    rightDir(dim) = 1
    val rightNeighIndex = DefaultNeighbors.getNeigh(rightDir).index

    val rightGhostIndex = IR_ExpressionIndex(Array.fill(numDims)(0))
    rightGhostIndex(dim) = numCellsPerFrag + 2
    val rightGhostAccess = IR_FieldLikeAccess(field, 0, rightGhostIndex)

    val rightBoundaryUpdate = IR_IfCondition(
      IR_Negation(IR_IV_NeighborIsValid(field.domain.index, rightNeighIndex, indexOfRefinedNeighbor)),
      ListBuffer[IR_Statement](
        IR_Assignment(IR_GridUtil.offsetAccess(rightGhostAccess, -1, dim), IR_GridUtil.offsetAccess(rightGhostAccess, -2, dim)),
        IR_Assignment(Duplicate(rightGhostAccess), IR_GridUtil.offsetAccess(rightGhostAccess, -1, dim))))

    var innerStatement : IR_Statement = IR_Assignment(Duplicate(baseAccess),
      0.5 * (Duplicate(npBaseAccess) + IR_GridUtil.offsetAccess(npBaseAccess, 1, dim))
        - 0.5 * (IR_GridUtil.offsetAccess(npBaseAccess, -1, dim) + Duplicate(npBaseAccess)))

    if (Knowledge.grid_halveStagBoundaryVolumes) {
      innerStatement =
        IR_IfCondition(IR_EqEq(0, innerIt),
          IR_Assignment(Duplicate(baseAccess),
            0.5 * (Duplicate(npBaseAccess) + IR_GridUtil.offsetAccess(npBaseAccess, 1, dim))
              - Duplicate(npBaseAccess)),
          IR_IfCondition(IR_EqEq(numCellsTotal, innerIt),
            IR_Assignment(Duplicate(baseAccess),
              Duplicate(npBaseAccess)
                - 0.5 * (IR_GridUtil.offsetAccess(npBaseAccess, -1, dim) + Duplicate(npBaseAccess))),
            innerStatement))
    }

    // compile final loop
    val innerLoop = IR_LoopOverPoints(field, None,
      IR_GridUtil.offsetIndex(IR_ExpressionIndex(Array.fill(numDims)(0)), -1, dim),
      IR_GridUtil.offsetIndex(IR_ExpressionIndex(Array.fill(numDims)(0)), -1, dim),
      IR_ExpressionIndex(Array.fill(numDims)(1)),
      ListBuffer(innerItDecl, innerStatement))
    innerLoop.parallelization.potentiallyParallel = false

    ListBuffer[IR_Statement](
      IR_LoopOverFragments(ListBuffer[IR_Statement](
        innerLoop,
        leftBoundaryUpdate,
        rightBoundaryUpdate)),
      IR_Communicate(field, 0, "both", ListBuffer(IR_CommunicateTarget("ghost", None, None)), None, ""))
  }
}
