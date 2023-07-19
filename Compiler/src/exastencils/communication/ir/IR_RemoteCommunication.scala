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
import exastencils.communication.NeighborInfo
import exastencils.domain.ir._

/// IR_ApplyRemoteCommunication

trait IR_ApplyRemoteCommunication {
  def isRemoteNeighbor(domainIdx : IR_Expression, neighborIdx : IR_Expression) = IR_IV_NeighborIsValid(domainIdx, neighborIdx) AndAnd IR_IV_NeighborIsRemote(domainIdx, neighborIdx)
}

/// IR_RemoteCommunication

abstract class IR_RemoteCommunication extends IR_Communication with IR_ApplyRemoteCommunication {
  def packInfos : ListBuffer[IR_RemotePackInfo]

  def genCopy(packInfo : IR_RemotePackInfo, addCondition : Boolean) : IR_Statement
  def genTransfer(packInfo : IR_RemotePackInfo, addCondition : Boolean) : IR_Statement

  def wrapCond(neighbor : NeighborInfo, stmt : IR_Statement) : IR_Statement =
    IR_IfCondition(isRemoteNeighbor(field.domain.index, neighbor.index),
      stmt)

  def wrapCond(neighbor : NeighborInfo, body : ListBuffer[IR_Statement]) : IR_Statement =
    IR_IfCondition(isRemoteNeighbor(field.domain.index, neighbor.index),
      body)
}
