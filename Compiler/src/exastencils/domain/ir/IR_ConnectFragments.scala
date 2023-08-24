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
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.communication.DefaultNeighbors
import exastencils.communication.ir.IR_IV_CommNeighNeighIdx
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.parallelization.ir.IR_ParallelizationInfo

case class IR_ConnectFragments() extends IR_Statement with IR_Expandable {
  def globalSize = IR_DomainCollection.getByIdentifier("global").get.asInstanceOf[IR_DomainFromAABB].aabb
  def fragWidth(dim : Int) = globalSize.width(dim) / Knowledge.domain_rect_numFragsTotalAsVec(dim)

  def isPointInsideDomain(position : (Int => IR_Expression), domain : IR_Domain) = {
    val size = domain.asInstanceOf[IR_DomainFromAABB].aabb

    Knowledge.dimensions.map(dim =>
      (position(dim) >= size.lower(dim))
        AndAnd (position(dim) <= size.upper(dim))).reduce(_ AndAnd _)
  }

  def owningRankForPoint(position : (Int => IR_Expression), domain : IR_Domain) = {
    IR_TernaryCondition(IR_Negation(isPointInsideDomain(position, domain)),
      s"MPI_PROC_NULL",
      Knowledge.dimensions.map(dim =>
        IR_ToInt(((position(dim) - globalSize.lower(dim)) / fragWidth(dim)) / Knowledge.domain_rect_numFragsPerBlockAsVec(dim))
          * (0 until dim).map(Knowledge.domain_rect_numBlocksAsVec(_)).product : IR_Expression).reduce(_ + _))
  }

  def localFragmentIdxForPoint(position : (Int => IR_Expression)) = {
    Knowledge.dimensions.map(dim =>
      (IR_ToInt((position(dim) - globalSize.lower(dim)) / fragWidth(dim)) Mod Knowledge.domain_rect_numFragsPerBlockAsVec(dim))
        * (0 until dim).map(Knowledge.domain_rect_numFragsPerBlockAsVec(_)).product : IR_Expression).reduce(_ + _)
  }

  def setIterationOffset(location : Int, domain : IR_Expression, fragment : IR_Expression) = {
    val neigh = DefaultNeighbors.neighbors(location)
    // FIXME: neighbor directions are always 3D vectors
    // invalid directions are not part of the given collection
    neigh.dir match {
      case Array(0, 0, 1)  => IR_Assignment(IR_IV_IterationOffsetEnd(2, domain, fragment), 0)
      case Array(-1, 0, 0) => IR_Assignment(IR_IV_IterationOffsetBegin(0, domain, fragment), 0)
      case Array(1, 0, 0)  => IR_Assignment(IR_IV_IterationOffsetEnd(0, domain, fragment), 0)
      case Array(0, -1, 0) => IR_Assignment(IR_IV_IterationOffsetBegin(1, domain, fragment), 0)
      case Array(0, 1, 0)  => IR_Assignment(IR_IV_IterationOffsetEnd(1, domain, fragment), 0)
      case Array(0, 0, -1) => IR_Assignment(IR_IV_IterationOffsetBegin(2, domain, fragment), 0)
      case _               => IR_NullStatement
    }
  }

  // TODO: no default value for refinement index
  def connectLocalElement(localFragmentIdx : IR_Expression, neighFragmentIdx : IR_Expression, neighIdx : Int, domain : Int, indexOfRefinedNeighbor : Option[IR_Expression] = None) = {
    ListBuffer[IR_Statement](
      IR_Assignment(IR_IV_NeighborIsValid(domain, neighIdx, indexOfRefinedNeighbor, Duplicate(localFragmentIdx)), true),
      IR_Assignment(IR_IV_NeighborIsRemote(domain, neighIdx, indexOfRefinedNeighbor, Duplicate(localFragmentIdx)), false),
      IR_Assignment(IR_IV_NeighborFragmentIdx(domain, neighIdx, indexOfRefinedNeighbor, Duplicate(localFragmentIdx)), neighFragmentIdx),
      IR_Assignment(IR_IV_CommNeighNeighIdx(domain, neighIdx, indexOfRefinedNeighbor, Duplicate(localFragmentIdx)), DefaultNeighbors.getOpposingNeigh(neighIdx).index),
      setIterationOffset(neighIdx, domain, Duplicate(localFragmentIdx)))
  }

  // TODO: no default value for refinement index
  def connectRemoteElement(localFragmentIdx : IR_Expression, localNeighIdx : IR_Expression, remoteRank : IR_Expression, neighIdx : Int, domain : Int, indexOfRefinedNeighbor : Option[IR_Expression] = None) = {
    ListBuffer[IR_Statement](
      IR_Assignment(IR_IV_NeighborIsValid(domain, neighIdx, indexOfRefinedNeighbor, Duplicate(localFragmentIdx)), true),
      IR_Assignment(IR_IV_NeighborIsRemote(domain, neighIdx, indexOfRefinedNeighbor, Duplicate(localFragmentIdx)), true),
      IR_Assignment(IR_IV_NeighborFragmentIdx(domain, neighIdx, indexOfRefinedNeighbor, Duplicate(localFragmentIdx)), localNeighIdx),
      IR_Assignment(IR_IV_NeighborRemoteRank(domain, neighIdx, indexOfRefinedNeighbor, Duplicate(localFragmentIdx)), remoteRank),
      IR_Assignment(IR_IV_CommNeighNeighIdx(domain, neighIdx, indexOfRefinedNeighbor, Duplicate(localFragmentIdx)), DefaultNeighbors.getOpposingNeigh(neighIdx).index),
      setIterationOffset(neighIdx, domain, Duplicate(localFragmentIdx)))
  }

  override def expand() : Output[IR_LoopOverFragments] = {
    var body = new ListBuffer[IR_Statement]

    val neighbors = DefaultNeighbors.neighbors
    val domains = IR_DomainCollection.objects
    for (d <- domains.indices)
      body += IR_Assignment(IR_IV_IsValidForDomain(d), isPointInsideDomain(IR_IV_FragmentPosition(_), domains(d)))

    if (Knowledge.domain_canHaveLocalNeighs || Knowledge.domain_canHaveRemoteNeighs || Knowledge.domain_rect_hasPeriodicity) {
      for (neigh <- neighbors) {
        var statements = ListBuffer[IR_Statement]()

        // store offset position to allow for implementation of periodic domains
        def offsetPos(dim : Int) = IR_VariableAccess(s"offsetPos_$dim", IR_RealDatatype)

        for (dim <- Knowledge.dimensions) {
          statements += IR_VariableDeclaration(offsetPos(dim), IR_IV_FragmentPosition(dim) + neigh.dir(dim) * fragWidth(dim))
          if (Knowledge.domain_rect_periodicAsVec(dim)) {
            // implement simple wrap-around for periodic domains
            statements += IR_IfCondition(IR_Greater(offsetPos(dim), globalSize.upper(dim)),
              IR_Assignment(offsetPos(dim), globalSize.upper(dim) - globalSize.lower(dim), "-="))
            statements += IR_IfCondition(IR_Lower(offsetPos(dim), globalSize.lower(dim)),
              IR_Assignment(offsetPos(dim), globalSize.upper(dim) - globalSize.lower(dim), "+="))
          }
        }

        // compile connect calls

        def localConnect(domainIdx : Int) = connectLocalElement(IR_LoopOverFragments.defIt,
          localFragmentIdxForPoint(offsetPos), neigh.index, domainIdx)

        def remoteConnect(domainIdx : Int) = connectRemoteElement(IR_LoopOverFragments.defIt,
          localFragmentIdxForPoint(offsetPos), owningRankForPoint(offsetPos, domains(domainIdx)), neigh.index, domainIdx)

        for (d <- domains.indices) {
          // for each domain: check if original point and neighbor point are valid and if true connect according to possible configurations
          statements += IR_IfCondition(IR_IV_IsValidForDomain(d) AndAnd isPointInsideDomain(offsetPos, domains(d)),
            if (Knowledge.domain_canHaveRemoteNeighs && Knowledge.domain_canHaveLocalNeighs)
              ListBuffer[IR_Statement](
                IR_IfCondition(IR_EqEq(MPI_IV_MpiRank, owningRankForPoint(offsetPos, domains(d))),
                  localConnect(d), remoteConnect(d)))
            else if (Knowledge.domain_canHaveRemoteNeighs)
              remoteConnect(d)
            else if (Knowledge.domain_canHaveLocalNeighs)
              localConnect(d)
            else
              ListBuffer[IR_Statement]())
        }

        // wrap in scope due to local variable declarations
        body += IR_Scope(statements)
      }
    }

    IR_LoopOverFragments(body, IR_ParallelizationInfo(potentiallyParallel = true))
  }
}
