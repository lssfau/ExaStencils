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
import exastencils.communication.RefinementCases
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir._
import exastencils.domain.ir._
import exastencils.field.ir.IR_Field

/// IR_LocalCommunicationStart

case class IR_LocalCommunicationStart(
    var field : IR_Field,
    var slot : IR_Expression,
    var refinementCase : RefinementCases.Access,
    var sendPackInfos : ListBuffer[IR_LocalPackInfo],
    var recvPackInfos : ListBuffer[IR_LocalPackInfo],
    var insideFragLoop : Boolean,
    var cond : Option[IR_Expression]) extends IR_LocalCommunication {

  def setLocalCommReady(packInfos : ListBuffer[IR_LocalPackInfo]) : ListBuffer[IR_Statement] = {
    wrapFragLoop(
      packInfos.map(packInfo => {
        val neighbor = packInfo.neighbor
        val neighborIdx = neighbor.index
        val domainIdx = field.domain.index

        IR_IfCondition(IR_IV_NeighborIsValid(domainIdx, neighborIdx) AndAnd IR_Negation(IR_IV_NeighborIsRemote(domainIdx, neighborIdx)),
          IR_Assignment(IR_IV_LocalCommReady(field, neighborIdx), IR_BooleanConstant(true)))
      }))
  }

  override def expand() : Output[StatementList] = {
    var output = ListBuffer[IR_Statement]()

    if (!Knowledge.domain_canHaveLocalNeighs) return output // nothing to do

    // set LocalCommReady to signal neighbors readiness for communication
    if (!Knowledge.comm_pushLocalData)
      output ++= setLocalCommReady(sendPackInfos)
    else
      output ++= setLocalCommReady(recvPackInfos)

    if (Knowledge.comm_pushLocalData) {
      // distribute this fragment's data - if enabled
      output += wrapFragLoop(
        IR_IfCondition(IR_IV_IsValidForDomain(field.domain.index),
          sendPackInfos.map(packInfo =>
            IR_LocalSend(field, Duplicate(slot), packInfo, insideFragLoop, Duplicate(cond)) : IR_Statement)))
    } else {
      // pull data for this fragment - otherwise
      output += wrapFragLoop(
        IR_IfCondition(IR_IV_IsValidForDomain(field.domain.index),
          recvPackInfos.map(packInfo =>
            IR_LocalRecv(field, Duplicate(slot), packInfo, insideFragLoop, Duplicate(cond)) : IR_Statement)))
    }

    output
  }
}
