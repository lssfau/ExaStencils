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
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.communication.NeighborInfo
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_IV_NeighborIsRemote
import exastencils.domain.ir.IR_IV_NeighborIsValid
import exastencils.domain.ir.IR_IV_NeighborRefinementCase
import exastencils.domain.ir.RefinementCase

/// IR_ApplyLocalCommunication

trait IR_ApplyLocalCommunication {
  def isLocalNeighbor(refinementCase : RefinementCase.Access, domainIdx : IR_Expression, neighborIdx : IR_Expression, indexOfRefinedNeighbor : Option[Int]) = {
    val base = IR_IV_NeighborIsValid(domainIdx, neighborIdx, indexOfRefinedNeighbor) AndAnd IR_Negation(IR_IV_NeighborIsRemote(domainIdx, neighborIdx, indexOfRefinedNeighbor))
    if (Knowledge.refinement_enabled)
      IR_IV_NeighborRefinementCase(IR_LoopOverFragments.defIt, domainIdx, neighborIdx) EqEq refinementCase.id AndAnd base
    else
      base
  }
}

/// IR_LocalCommunication

abstract class IR_LocalCommunication extends IR_Communication with IR_ApplyLocalCommunication {
  def sendPackInfos : ListBuffer[IR_LocalPackInfo]

  def recvPackInfos : ListBuffer[IR_LocalPackInfo]

  def wrapCond(neighbor : NeighborInfo, indexOfRefinedNeighbor : Option[Int], stmt : IR_Statement) : IR_Statement =
    IR_IfCondition(isLocalNeighbor(refinementCase, field.domain.index, neighbor.index, indexOfRefinedNeighbor),
      stmt)

  def wrapCond(neighbor : NeighborInfo, indexOfRefinedNeighbor : Option[Int], body : ListBuffer[IR_Statement]) : IR_Statement =
    IR_IfCondition(isLocalNeighbor(refinementCase, field.domain.index, neighbor.index, indexOfRefinedNeighbor),
      body)
}
