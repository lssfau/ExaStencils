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

package exastencils.applications.swe.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.DefaultNeighbors
import exastencils.communication.ir._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.domain.ir._
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi._
import exastencils.parallelization.ir.IR_PotentiallyCritical
import exastencils.prettyprinting.PpStream

/// IR_IV_FragmentOrder

case class IR_IV_FragmentOrder(var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName() = s"fragmentOrder" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(-1)
}

/// IR_IV_NeighFragOrder

case class IR_IV_NeighFragOrder(var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, true) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, neighIdx)
  override def resolveName() = s"neighFragOrder" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", neighIdx.prettyprint)
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(-1)
}

/// IR_CommunicateFragmentOrder

case object IR_CommunicateFragmentOrder extends IR_FuturePlainFunction with IR_ApplyLocalCommunication with IR_ApplyRemoteCommunication {
  override var name = "commFragOrderInternal"
  def returnType : IR_Datatype = IR_UnitDatatype

  // TODO: adapt for refinement
  def refinementCase : RefinementCase.Access = RefinementCase.EQUAL

  def concurrencyId : Int = 0

  def indexOfRefinedNeighbor : Option[Int] = None

  override def prettyprint_decl() : String = {
    returnType.prettyprint + ' ' + name + "( );\n"
  }

  def generateFctAccess() = IR_PlainInternalFunctionReference(name, returnType)

  private def neighNeighIdx(neighIdx : Int) : IR_Expression = if (Knowledge.comm_enableCommTransformations) IR_IV_CommNeighNeighIdx(0, neighIdx) else DefaultNeighbors.getOpposingNeigh(neighIdx).index

  private def localComm() = {
    def compose(neighIdx : Int) = {
      IR_Assignment(
        IR_IV_NeighFragOrder(neighNeighIdx(neighIdx), IR_IV_NeighborFragmentIdx(0, neighIdx)),
        IR_IV_FragmentOrder())
    }

    IR_LoopOverFragments(
      DefaultNeighbors.neighbors.map(_.index).map(neighIdx =>
        IR_IfCondition(isLocalNeighbor(refinementCase, 0, neighIdx),
          compose(neighIdx)) : IR_Statement))
  }

  private def beginRemoteComm() = {
    def compose(neighIdx : Int) = {
      val sendTag = MPI_GeneratedTag(IR_IV_CommunicationId(), IR_IV_NeighborFragmentIdx(0, neighIdx), neighIdx, concurrencyId, indexOfRefinedNeighbor)
      val sendReq = MPI_RequestNoField(send = true, neighIdx)

      val recvTag = MPI_GeneratedTag(IR_IV_NeighborFragmentIdx(0, neighIdx), IR_IV_CommunicationId(), neighNeighIdx(neighIdx), concurrencyId, indexOfRefinedNeighbor)
      val recvReq = MPI_RequestNoField(send = false, neighIdx)

      val sendCall = MPI_Send(IR_AddressOf(IR_IV_FragmentOrder()), 1, IR_IntegerDatatype, IR_IV_NeighborRemoteRank(0, neighIdx), sendTag, sendReq)
      val recvCall = MPI_Receive(IR_AddressOf(IR_IV_NeighFragOrder(neighIdx)), 1, IR_IntegerDatatype, IR_IV_NeighborRemoteRank(0, neighIdx), recvTag, recvReq)

      ListBuffer[IR_Statement](
        IR_PotentiallyCritical(sendCall),
        IR_Assignment(IR_IV_RemoteReqOutstandingNoField(send = true, neighIdx, concurrencyId, indexOfRefinedNeighbor), true),
        IR_PotentiallyCritical(recvCall),
        IR_Assignment(IR_IV_RemoteReqOutstandingNoField(send = false, neighIdx, concurrencyId, indexOfRefinedNeighbor), true))
    }

    IR_LoopOverFragments(
      DefaultNeighbors.neighbors.map(_.index).map(neighIdx =>
        IR_IfCondition(isRemoteNeighbor(refinementCase, 0, neighIdx),
          compose(neighIdx)) : IR_Statement))
  }

  private def finishRemoteComm() = {
    def compose(neighIdx : Int) = {
      ListBuffer(false, true).map(isSend =>
        IR_IfCondition(IR_IV_RemoteReqOutstandingNoField(isSend, neighIdx, concurrencyId, indexOfRefinedNeighbor),
          ListBuffer[IR_Statement](
            IR_FunctionCall(MPI_WaitForRequest.generateFctAccess(), IR_AddressOf(MPI_RequestNoField(isSend, neighIdx))),
            IR_Assignment(IR_IV_RemoteReqOutstandingNoField(isSend, neighIdx, concurrencyId, indexOfRefinedNeighbor), false))) : IR_Statement)
    }

    IR_LoopOverFragments(
      DefaultNeighbors.neighbors.map(_.index).map(neighIdx =>
        IR_IfCondition(isRemoteNeighbor(refinementCase, 0, neighIdx),
          compose(neighIdx)) : IR_Statement))
  }

  override def generateFct() = {
    var body = ListBuffer[IR_Statement]()

    body += beginRemoteComm()
    body += localComm()
    body += finishRemoteComm()

    IR_PlainFunction(name, returnType, body)
  }
}

/// IR_ResolveFragmentOrder

object IR_ResolveFragmentOrder extends DefaultStrategy("ResolveFragmentOrder") {
  this += new Transformation("ResolveFunctionCalls", {
    case IR_FunctionCall(IR_UnresolvedFunctionReference("getFragmentOrder", _), args) =>
      // usage: getFragmentOrder ( fragmentIdx )
      IR_IV_FragmentOrder(args(0))

    case IR_FunctionCall(IR_UnresolvedFunctionReference("getNeighFragmentOrder", _), args) =>
      // usage: getNeighFragmentOrder ( fragmentIdx, neighIdx )
      IR_IV_NeighFragOrder(args(1), args(0))

    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("setFragmentOrder", _), args)) =>
      // usage: setFragmentOrder ( fragmentIdx, order )
      IR_Assignment(IR_IV_FragmentOrder(args(0)), args(1))

    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("setNeighFragmentOrder", _), args)) =>
      // usage: setNeighFragmentOrder ( fragmentIdx, neighIdx, order )
      IR_Assignment(IR_IV_NeighFragOrder(args(1), args(0)), args(2))

    case IR_FunctionCall(IR_UnresolvedFunctionReference("getNeighIsValid", _), args) =>
      // usage: getNeighIsValid ( neighIdx )
      IR_IV_NeighborIsValid(0, args(0))

    case IR_ExpressionStatement(call : IR_FunctionCall) if "communicateFragOrder" == call.name =>
      if (Knowledge.refinement_enabled)
        Logger.error("Statement \"communicateFragOrder\" not available when mesh refinement is enabled.")

      if (!IR_CommunicationFunctions.get.functions.exists(_.name == IR_CommunicateFragmentOrder.name))
        IR_CommunicationFunctions.get.functions += IR_CommunicateFragmentOrder

      IR_ExpressionStatement(IR_FunctionCall(IR_CommunicateFragmentOrder.generateFctAccess()))
  })
}
