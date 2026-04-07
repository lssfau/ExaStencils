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
import exastencils.communication._
import exastencils.config._
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir._
import exastencils.domain.ir._
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.logger.Logger
import exastencils.parallelization.api.omp.OMP_WaitForFlag

/// IR_LocalCommunicationFinish

case class IR_LocalCommunicationFinish(
    var field : IR_FieldLike,
    var slot : IR_Expression,
    var sendPackInfos : ListBuffer[IR_LocalPackInfo],
    var recvPackInfos : ListBuffer[IR_LocalPackInfo],
    var insideFragLoop : Boolean,
    var cond : Option[IR_Expression]) extends IR_LocalCommunication {

  def waitForLocalComm(neighbors : ListBuffer[IR_LocalPackInfo]) : ListBuffer[IR_Statement] = {
    wrapFragLoop(
      neighbors.map(packInfo => {
        val neighbor = packInfo.neighbor
        val neighborIdx = neighbor.index
        val domainIdx = field.domain.index
        val indexOfRefinedNeighbor = getIndexOfRefinedNeighbor(packInfo)

        wrapCond(
          packInfo.refinementCase,
          neighbor,
          indexOfRefinedNeighbor,
          ListBuffer[IR_Statement](
            IR_FunctionCall(OMP_WaitForFlag.generateFctAccess(),
              IR_AddressOf(IR_IV_LocalCommDone(field,
                if (Knowledge.comm_enableCommTransformations)
                  IR_IV_CommNeighNeighIdx(domainIdx, neighborIdx, indexOfRefinedNeighbor)
                else
                  DefaultNeighbors.getOpposingNeigh(neighbor).index,
                IR_IV_NeighborFragmentIdx(domainIdx, neighborIdx, indexOfRefinedNeighbor))))))
      }))
  }

  override def expand() : Output[StatementList] = {
    var output = ListBuffer[IR_Statement]()

    if (!Knowledge.domain_canHaveLocalNeighs) return output // nothing to do

    // wait until all neighbors signal that they are finished
    if (Knowledge.refinement_enabled) {
      // TODO: implement pull scheme

      // default to push scheme
      output ++= waitForLocalComm(recvPackInfos)
    } else {
      if (!Knowledge.comm_pushLocalData)
        output ++= waitForLocalComm(sendPackInfos)
      else
        output ++= waitForLocalComm(recvPackInfos)
    }

    output
  }
}
