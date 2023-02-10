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

package exastencils.domain.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.ir.IR_IV_CommunicationId
import exastencils.config.Knowledge
import exastencils.globals.ir.IR_AllocateDataFunction
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.parallelization.api.mpi.MPI_IV_MpiSize
import exastencils.parallelization.ir.IR_ParallelizationInfo

case class IR_InitGeneratedDomain() extends IR_FuturePlainFunction {
  override var name = "initDomain"
  override def prettyprint_decl() = prettyprint

  def globalSize = IR_DomainCollection.getByIdentifier("global").get.asInstanceOf[IR_DomainFromAABB].aabb
  def fragWidth(dim : Int) = globalSize.width(dim) / Knowledge.domain_rect_numFragsTotalAsVec(dim)

  def setupFragmentPosition() = {
    def localFragIndex(dim : Int) = (IR_LoopOverFragments.defIt / (0 until dim).map(Knowledge.domain_rect_numFragsPerBlockAsVec(_)).product) Mod Knowledge.domain_rect_numFragsPerBlockAsVec(dim)
    def rankIndex(dim : Int) = (MPI_IV_MpiRank / (0 until dim).map(dim => Knowledge.domain_rect_numBlocksAsVec(dim)).product) Mod Knowledge.domain_rect_numBlocksAsVec(dim)

    Knowledge.dimensions.map(dim =>
      IR_Assignment(IR_IV_FragmentPosition(dim),
        ((rankIndex(dim) * Knowledge.domain_rect_numFragsPerBlockAsVec(dim) + 0.5 + localFragIndex(dim)) * fragWidth(dim)) + globalSize.lower(dim))
    )
  }

  def setupFragmentPosBeginAndEnd() = {
    val begin = Knowledge.dimensions.map(dim =>
      IR_Assignment(IR_IV_FragmentPositionBegin(dim), IR_IV_FragmentPosition(dim) - 0.5 * fragWidth(dim)))
    val end = Knowledge.dimensions.map(dim =>
      IR_Assignment(IR_IV_FragmentPositionEnd(dim), IR_IV_FragmentPosition(dim) + 0.5 * fragWidth(dim)))
    begin ++ end
  }

  def setupFragmentId() = {
    IR_Assignment(IR_IV_FragmentId(),
      Knowledge.dimensions.map(dim =>
        IR_ToInt((IR_IV_FragmentPosition(dim) - globalSize.lower(dim)) / fragWidth(dim))
          * (0 until dim).map(Knowledge.domain_rect_numFragsTotalAsVec(_)).product : IR_Expression).reduce(_ + _))
  }

  def setupFragmentIndex() = {
    Knowledge.dimensions.map(dim =>
      IR_Assignment(IR_IV_FragmentIndex(dim),
        IR_ToInt((IR_IV_FragmentPosition(dim) - globalSize.lower(dim)) / fragWidth(dim))))
  }

  def setupCommId() = {
    IR_Assignment(IR_IV_CommunicationId(),
      Knowledge.dimensions.map(dim =>
        (IR_ToInt((IR_IV_FragmentPosition(dim) - globalSize.lower(dim)) / fragWidth(dim))
          Mod Knowledge.domain_rect_numFragsPerBlockAsVec(dim)) * (0 until dim).map(Knowledge.domain_rect_numFragsPerBlockAsVec(_)).product : IR_Expression).reduce(_ + _))
  }

  override def generateFct() = {
    var body = ListBuffer[IR_Statement]()

    // TODO: move to main application
    if (Knowledge.mpi_enabled)
      body += IR_Assert(IR_EqEq(MPI_IV_MpiSize, Knowledge.domain_numBlocks),
        ListBuffer("\"Invalid number of MPI processes (\"", MPI_IV_MpiSize, "\") should be \"", Knowledge.mpi_numThreads),
        IR_FunctionCall("exit", 1))

    // compose fragment loop setting basic fragment information
    var fragStatements = ListBuffer[IR_Statement]()

    fragStatements ++= setupFragmentPosition()
    fragStatements ++= setupFragmentIndex()
    fragStatements += setupFragmentId()
    fragStatements += setupCommId()
    fragStatements ++= setupFragmentPosBeginAndEnd()

    body += IR_LoopOverFragments(fragStatements, IR_ParallelizationInfo(potentiallyParallel = true))

    body += IR_ConnectFragments()

    // FIXME: move to app
    body += IR_FunctionCall(IR_AllocateDataFunction.fctName)

    IR_PlainFunction(name, IR_UnitDatatype, body)
  }
}
