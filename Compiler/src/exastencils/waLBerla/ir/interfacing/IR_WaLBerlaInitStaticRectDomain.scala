package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.communication.DefaultNeighbors
import exastencils.communication.ir.IR_IV_CommunicationId
import exastencils.config.Knowledge
import exastencils.domain.ir._
import exastencils.parallelization.api.mpi.MPI_IV_MpiComm
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.parallelization.ir.IR_ParallelizationInfo
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverBlocks
import exastencils.waLBerla.ir.grid.IR_WaLBerlaBlockAABB

// TODO refactor

object IR_WaLBerlaInitStaticRectDomain {
  var fctName : String = "initStaticRectDomain"
}

case class IR_WaLBerlaInitStaticRectDomain() extends IR_WaLBerlaFuturePlainFunction {

  // TODO: only meant for static and regular domain partitioning
  // TODO: assumes totalNumFrags = totalNumWbBlocks

  override def name : String = IR_WaLBerlaInitStaticRectDomain.fctName
  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint

  def globalSize = IR_DomainCollection.getByIdentifier("global").get.asInstanceOf[IR_DomainFromAABB].aabb
  def fragWidth(dim : Int) = globalSize.width(dim) / Knowledge.domain_rect_numFragsTotalAsVec(dim)

  // fragment info

  def block = IR_WaLBerlaLoopOverBlocks.block
  def defIt = IR_WaLBerlaLoopOverBlocks.defIt
  def getBlockAABB = IR_WaLBerlaBlockAABB(block)

  def setupFragmentPosition() = {
    Knowledge.dimensions.map(dim =>
      IR_Assignment(IR_IV_FragmentPosition(dim),
        getBlockAABB.center(dim)))
  }

  def setupFragmentId() = {
    IR_Assignment(IR_IV_FragmentId(),
      Knowledge.dimensions.map(dim => {
        val revertedDims = (Knowledge.dimensionality-1) until dim by -1
        IR_ToInt((IR_IV_FragmentPosition(dim) - globalSize.lower(dim)) / fragWidth(dim)) *
          (if (revertedDims.isEmpty) 1 else revertedDims.map(Knowledge.domain_rect_numFragsTotalAsVec(_)).product) : IR_Expression
      }).reduce(_ + _))
  }

  def setupFragmentPosBeginAndEnd() = {
    val begin = Knowledge.dimensions.map(dim =>
      IR_Assignment(IR_IV_FragmentPositionBegin(dim),
        getBlockAABB.min(dim)))
    val end = Knowledge.dimensions.map(dim =>
      IR_Assignment(IR_IV_FragmentPositionEnd(dim),
        getBlockAABB.max(dim)))
    begin ++ end
  }

  // fragment connection

  def owningRankForPoint(position : (Int => IR_Expression), domain : IR_Domain) = {
    IR_TernaryCondition(IR_Negation(IR_ConnectFragments().isPointInsideDomain(position, domain)),
      s"MPI_PROC_NULL",
      Knowledge.dimensions.map(dim => {
        val revertedDims = (Knowledge.dimensionality-1) until dim by -1
        IR_ToInt(((position(dim) - globalSize.lower(dim)) / fragWidth(dim)) / Knowledge.domain_rect_numFragsPerBlockAsVec(dim)) *
          (if (revertedDims.isEmpty) 1 else revertedDims.map(Knowledge.domain_rect_numBlocksAsVec(_)).product) : IR_Expression
      }).reduce(_ + _))
  }

  override def isInterfaceFunction : Boolean = true
  override def inlineImplementation : Boolean = true

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    var init = ListBuffer[IR_Statement]()
    var connect = ListBuffer[IR_Statement]()

    /* set basic fragment info */
    var fragStatements = ListBuffer[IR_Statement]()

    fragStatements ++= setupFragmentPosition()
    fragStatements ++= IR_InitGeneratedDomain().setupFragmentIndex()
    fragStatements += IR_InitGeneratedDomain().setupCommId()
    fragStatements += setupFragmentId()
    fragStatements ++= setupFragmentPosBeginAndEnd()

    if (Knowledge.mpi_enabled) {
      if (Knowledge.waLBerla_createCartComm) {
        init += IR_MemberFunctionCallArrow(IR_VariableAccess("MPIManager::instance()", IR_UnknownDatatype), "resetMPI")
        init += IR_MemberFunctionCallArrow(IR_VariableAccess("MPIManager::instance()", IR_UnknownDatatype), "createCartesianComm",
          ListBuffer[IR_Expression](
            Knowledge.domain_rect_numBlocks_x, Knowledge.domain_rect_numBlocks_y, Knowledge.domain_rect_numBlocks_z, // number of processes
            Knowledge.domain_rect_periodic_x, Knowledge.domain_rect_periodic_y, Knowledge.domain_rect_periodic_z)) // periodicity
      }

      // assign exas MPI comm IV to waLBerla's communicator
      init += IR_Assignment(MPI_IV_MpiComm,
        IR_MemberFunctionCallArrowWithDt(IR_VariableAccess("MPIManager::instance()", IR_UnknownDatatype), "comm", MPI_IV_MpiComm.datatype))
    }

    init += IR_WaLBerlaLoopOverBlocks(fragStatements, IR_ParallelizationInfo(potentiallyParallel = true))

    /* set fragment connection */

    val neighbors = DefaultNeighbors.neighbors
    val domains = IR_DomainCollection.objects
    for (d <- domains.indices)
      connect += IR_Assignment(IR_IV_IsValidForDomain(d), IR_ConnectFragments().isPointInsideDomain(IR_IV_FragmentPosition(_), domains(d)))

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
        def localConnect(domainIdx : Int) = IR_ConnectFragments().connectLocalElement(defIt,
          IR_ConnectFragments().localFragmentIdxForPoint(offsetPos), neigh.index, domainIdx)

        def remoteConnect(domainIdx : Int) = IR_ConnectFragments().connectRemoteElement(defIt,
          IR_ConnectFragments().localFragmentIdxForPoint(offsetPos), owningRankForPoint(offsetPos, domains(domainIdx)), neigh.index, domainIdx)

        for (d <- domains.indices) {
          // check if original point and neighbor point are valid and connect according to possible configurations
          statements += IR_IfCondition(IR_IV_IsValidForDomain(d) AndAnd IR_ConnectFragments().isPointInsideDomain(offsetPos, domains(d)),
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
        connect += IR_WaLBerlaLoopOverBlocks(IR_Scope(statements), IR_ParallelizationInfo(potentiallyParallel = true))
      }
    }

    IR_WaLBerlaPlainFunction(name, IR_UnitDatatype, ListBuffer(), init ++ connect)
  }
}
