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

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.communication.NeighborInfo
import exastencils.config.Knowledge
import exastencils.domain.ir._
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.parallelization.api.mpi.MPI_DataType

/// IR_ApplyRemoteCommunication

trait IR_ApplyRemoteCommunication {
  def isRemoteNeighbor(refinementCase : RefinementCase.Access, domainIdx : IR_Expression, neighborIdx : IR_Expression) = {
    val base = IR_IV_NeighborIsValid(domainIdx, neighborIdx) AndAnd IR_IV_NeighborIsRemote(domainIdx, neighborIdx)
    if (Knowledge.refinement_enabled)
      IR_IV_NeighborRefinementCase(IR_LoopOverFragments.defIt, domainIdx, neighborIdx) EqEq refinementCase.id AndAnd base
    else
      base
  }
}

/// IR_RemoteCommunication

abstract class IR_RemoteCommunication extends IR_Communication with IR_ApplyRemoteCommunication {
  def packInfos : ListBuffer[IR_RemotePackInfo]

  def requiresPacking(refinementCase : RefinementCase.Access, indices : IR_ExpressionIndexRange, condition : Option[IR_Expression]) = {
    refinementCase != RefinementCase.EQUAL ||
      Knowledge.data_genVariableFieldSizes ||
      (!MPI_DataType.shouldBeUsed(field, indices, condition) && IR_SimplifyExpression.evalIntegral(indices.getTotalSize) > 1)
  }

  def getIndexOfRefinedNeighbor(packInfo : IR_RemotePackInfo) : Option[Int] = packInfo match {
    case p : IR_RefinementPackInfo => Some(p.indexOfRefinedNeighbor)
    case _                         => None
  }

  def genCopy(packInfo : IR_RemotePackInfo, addCondition : Boolean, indexOfRefinedNeighbor : Option[Int] = None) : IR_Statement
  def genTransfer(packInfo : IR_RemotePackInfo, addCondition : Boolean, indexOfRefinedNeighbor : Option[Int] = None) : IR_Statement

  def wrapCond(neighbor : NeighborInfo, stmt : IR_Statement) : IR_Statement =
    IR_IfCondition(isRemoteNeighbor(refinementCase, field.domain.index, neighbor.index),
      stmt)

  def wrapCond(neighbor : NeighborInfo, body : ListBuffer[IR_Statement]) : IR_Statement =
    IR_IfCondition(isRemoteNeighbor(refinementCase, field.domain.index, neighbor.index),
      body)
}
