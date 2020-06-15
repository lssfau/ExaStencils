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
import exastencils.communication.ir.IR_IV_CommNeighNeighIdx
import exastencils.communication.ir.IR_IV_CommVariable
import exastencils.communication.ir.IR_IV_CommunicationId
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures._
import exastencils.datastructures.ir.StatementList
import exastencils.domain.ir._
import exastencils.field.ir.IR_FieldAccess
import exastencils.parallelization.api.mpi.MPI_Receive
import exastencils.parallelization.api.mpi.MPI_Send
import exastencils.parallelization.api.mpi.MPI_WaitForRequest
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

case class RemoteFragmentCommunicationBegin(
    var frag_index : IR_Expression,
    var neigh_idx : Int) extends IR_Statement with IR_Expandable {

  override def expand() : Output[StatementList] = {
    ListBuffer[IR_Statement](
      IR_PotentiallyCritical(MPI_Send(IR_AddressOf(IR_IV_FragmentOrder(frag_index)), 1, IR_RealDatatype, IR_IV_NeighborRemoteRank(0, neigh_idx),
        MPI_GeneratedTag_New(IR_IV_CommunicationId(), IR_IV_NeighborFragmentIdx(0, neigh_idx), neigh_idx),
        MPI_Request_New(s"Send", neigh_idx)),
        IR_Assignment(IR_IV_RemoteReqOutstanding_New(s"Send", neigh_idx), true),
        MPI_Receive(IR_AddressOf(IR_IV_NeighFragOrder(neigh_idx, frag_index)), 1, IR_RealDatatype, IR_IV_NeighborRemoteRank(0, neigh_idx),
          MPI_GeneratedTag_New(IR_IV_NeighborFragmentIdx(0, neigh_idx), IR_IV_CommunicationId(),
            if (Knowledge.comm_enableCommTransformations)
              IR_IV_CommNeighNeighIdx(0, neigh_idx)
            else
              DefaultNeighbors.getOpposingNeigh(neigh_idx).index),
          MPI_Request_New(s"Recv", neigh_idx)),
        IR_Assignment(IR_IV_RemoteReqOutstanding_New(s"Recv", neigh_idx), true))
    )
  }
}

case class RemoteFragmentCommunicationFinish(
    var direction : String,
    var frag_index : IR_Expression,
    var neigh_idx : Int,
) extends IR_Statement with IR_Expandable {
  override def expand() : Output[IR_Statement] = {
    IR_IfCondition(
      IR_IV_RemoteReqOutstanding_New(s"${ direction }", neigh_idx),
      ListBuffer[IR_Statement](
        IR_FunctionCall(MPI_WaitForRequest.generateFctAccess(), IR_AddressOf(MPI_Request_New(s"${ direction }", neigh_idx))),
        IR_Assignment(IR_IV_RemoteReqOutstanding_New(s"${ direction }", neigh_idx), false)))
  }
}

/// IR_ResolveFragmentOrder

object IR_ResolveFragmentOrder extends DefaultStrategy("ResolveFragmentOrder") {
  def getIndex(fieldAccess : IR_FieldAccess) = {
    val index = fieldAccess.index
    if (fieldAccess.offset.isDefined)
      for (i <- 0 until Math.min(fieldAccess.index.length, fieldAccess.offset.get.length))
        index(i) += fieldAccess.offset.get(i)
    index
  }

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
      // usage: getNeighIsValid (  neighIdx )
      IR_IV_NeighborIsValid(0, args(0))

    case IR_ExpressionStatement(call : IR_FunctionCall) if "communicateFragOrder" == call.name =>
      def fragmentIdx = IR_LoopOverFragments.defIt

      var statements = ListBuffer[IR_Statement]()
      var statements_begin = ListBuffer[IR_Statement]()
      var statements_finish = ListBuffer[IR_Statement]()
      for (neigh <- DefaultNeighbors.neighbors) {
        statements_begin += IR_IfCondition(
          IR_IV_NeighborIsValid(0, neigh.index),
          IR_IfCondition(
            IR_IV_NeighborIsRemote(0, neigh.index), RemoteFragmentCommunicationBegin(fragmentIdx, neigh.index), //TODO MPI part
            IR_Assignment(
              IR_IV_NeighFragOrder(if (Knowledge.comm_enableCommTransformations)
                IR_IV_CommNeighNeighIdx(0, neigh.index)
              else
                DefaultNeighbors.getOpposingNeigh(neigh.index).index, IR_IV_NeighborFragmentIdx(0, neigh.index)),
              IR_IV_FragmentOrder())))
        statements_finish += IR_IfCondition(
          IR_IV_NeighborIsValid(0, neigh.index),
          IR_IfCondition(
            IR_IV_NeighborIsRemote(0, neigh.index), RemoteFragmentCommunicationFinish(s"Send", fragmentIdx, neigh.index), //TODO MPI part
          ))
        statements_finish += IR_IfCondition(
          IR_IV_NeighborIsValid(0, neigh.index),
          IR_IfCondition(
            IR_IV_NeighborIsRemote(0, neigh.index), RemoteFragmentCommunicationFinish(s"Recv", fragmentIdx, neigh.index), //TODO MPI part
          ))

      }

      statements += IR_LoopOverFragments(statements_begin)
      statements += IR_LoopOverFragments(statements_finish)
      statements
  })
}

case class MPI_Request_New(var direction : String, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, 0, 0, neighIdx)

  override def resolveName() = s"mpiRequest_${ direction }" + resolvePostfix(fragmentIdx.prettyprint, "", "0", "0", neighIdx.prettyprint)
  override def resolveDatatype() = "MPI_Request"
}

case class IR_IV_RemoteReqOutstanding_New(
    var direction : String,
    var neighIdx : IR_Expression,
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_CommVariable {

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, 0, 0, neighIdx)

  override def resolveName() = s"remoteReqOutstanding_${ direction }" + resolvePostfix(fragmentIdx.prettyprint, "", "0", "0", neighIdx.prettyprint)
  override def resolveDatatype() = IR_BooleanDatatype
  override def resolveDefValue() = Some(false)
}

case class MPI_GeneratedTag_New(var from : IR_Expression, var to : IR_Expression, var dirOfSend : IR_Expression) extends IR_Expression with IR_Expandable {
  override def datatype = IR_UnitDatatype

  def expand() : Output[IR_Expression] = {
    (IR_Cast(IR_SpecialDatatype("unsigned int"), from << IR_IntegerConstant(31 - 8))
      + IR_Cast(IR_SpecialDatatype("unsigned int"), to << IR_IntegerConstant(31 - 16))
      + IR_Cast(IR_SpecialDatatype("unsigned int"), dirOfSend << IR_IntegerConstant(31 - 21)))
  }
}