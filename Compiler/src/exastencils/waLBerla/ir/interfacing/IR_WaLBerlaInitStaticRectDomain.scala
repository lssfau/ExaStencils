package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_Linearization
import exastencils.communication.DefaultNeighbors
import exastencils.communication.ir.IR_IV_CommunicationId
import exastencils.config.Knowledge
import exastencils.domain.ir._
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi.MPI_IV_MpiComm
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.parallelization.api.mpi.MPI_IV_MpiSize
import exastencils.parallelization.ir.IR_ParallelizationInfo
import exastencils.util.ir.IR_Print
import exastencils.util.ir.IR_Read
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverBlockNeighborhoodSection
import exastencils.waLBerla.ir.blockforest._
import exastencils.waLBerla.ir.grid.IR_WaLBerlaAABB
import exastencils.waLBerla.ir.grid.IR_WaLBerlaBlockAABB
import exastencils.waLBerla.ir.refinement.IR_WaLBerlaRefinementIndexForCoarseNeighbor
import exastencils.waLBerla.ir.refinement.IR_WaLBerlaRefinementLevel

/// IR_WaLBerlaInitStaticRectDomain
// only meant for static and regular domain partitioning
// TODO: refactor for refinement

object IR_WaLBerlaInitStaticRectDomain {
  var fctName : String = "initStaticRectDomain"
}

case class IR_WaLBerlaInitStaticRectDomain() extends IR_WaLBerlaWrapperFunction {
  override def name : String = IR_WaLBerlaInitStaticRectDomain.fctName

  // iterate over neighborhood section and connect fragments
  val neighbors = DefaultNeighbors.neighbors
  val domain = IR_DomainCollection.getByIdentifier("global").get
  val domainIdx = domain.index
  val globalSize = domain.asInstanceOf[IR_DomainFromAABB].aabb

  // block info
  val invalidIndex = -10000

  def blockForest = IR_WaLBerlaBlockForest()
  def block = IR_WaLBerlaLoopOverLocalBlocks.block
  def blockID = IR_WaLBerlaBlockID("blockID", block)
  def defIt = IR_WaLBerlaLoopOverLocalBlockArray.defIt
  def getBlockAABB = IR_WaLBerlaBlockAABB(block)

  // flags signaling potential neighbors
  def canHaveLocalNeighs = !Knowledge.waLBerla_useGridPartFromExa || Knowledge.domain_canHaveLocalNeighs
  def canHaveRemoteNeighs = !Knowledge.waLBerla_useGridPartFromExa || Knowledge.domain_canHaveRemoteNeighs
  def canHavePeriodicity = !Knowledge.waLBerla_useGridPartFromExa || Knowledge.domain_rect_hasPeriodicity

  // wb block info
  def wbNeighborIdx = IR_WaLBerlaLoopOverBlockNeighborhoodSection.defIt

  def wbNeighborBlockId = IR_VariableAccess("neighBlockID", IR_SpecialDatatype("const auto"))

  def hasLocalNeighbor(wbNeighSectionIdx : IR_Expression) = IR_MemberFunctionCallArrow(block, "neighborExistsLocally", wbNeighSectionIdx, wbNeighborIdx)

  def hasRemoteNeighbor(wbNeighSectionIdx : IR_Expression) = IR_MemberFunctionCallArrow(block, "neighborExistsRemotely", wbNeighSectionIdx, wbNeighborIdx)

  // setup functions
  def setupFragmentPosition() = {
    Knowledge.dimensions.map(dim => IR_Assignment(IR_IV_FragmentPosition(dim), getBlockAABB.center(dim)))
  }
  def setupFragmentIndex() = {
    Logger.warning("IR_IV_FragmentIndex currently not properly set up in waLBerla coupling.")
    for (d <- Knowledge.dimensions) yield IR_Assignment(IR_IV_FragmentIndex(d), invalidIndex)
  }
  def setupFragmentId() = {
    Logger.warning("IR_IV_FragmentId currently set to a waLBerla block's ID.")
    IR_Assignment(IR_IV_FragmentId(), blockID.getTreeId())
  }
  def setupCommId() = {
    IR_Assignment(IR_IV_CommunicationId(defIt), defIt)
  }
  def setupFragmentPosBeginAndEnd() = {
    val begin = Knowledge.dimensions.map(dim => IR_Assignment(IR_IV_FragmentPositionBegin(dim), getBlockAABB.min(dim)))
    val end = Knowledge.dimensions.map(dim => IR_Assignment(IR_IV_FragmentPositionEnd(dim), getBlockAABB.max(dim)))
    begin ++ end
  }

  override def isInterfaceFunction : Boolean = true
  override def inlineIncludeImplementation : Boolean = true

  override def generateWaLBerlaFct() : IR_WaLBerlaPlainFunction = {
    var checks = ListBuffer[IR_Statement]()
    var init = ListBuffer[IR_Statement]()
    var communicate = ListBuffer[IR_Statement]()
    var connect = ListBuffer[IR_Statement]()

    /* error checks to ensure consistency */

    if (Knowledge.waLBerla_useGridPartFromExa) {
      def checkError(cond : IR_Expression, msg : String) = IR_Assert(cond, ListBuffer(IR_StringConstant(msg)), IR_FunctionCall("exit", 1))

      // check if number of fragments, blocks and processes coincide
      checks += checkError(blockForest.getNumberOfAllLocalBlocks() EqEq Knowledge.domain_numFragmentsPerBlock,
        "Number of local waLBerla blocks does not match with number of fragments.")
      checks += checkError(blockForest.getNumberOfAllRootBlocks() EqEq Knowledge.domain_numFragmentsTotal,
        "Number of total waLBerla blocks does not match with total number of fragments.")
      checks += checkError(
        IR_MemberFunctionCallArrow(IR_VariableAccess("MPIManager::instance()", IR_UnknownDatatype), "numProcesses") EqEq Knowledge.domain_numBlocks,
        "Number of processes does not match with knowledge specification.")
    }

    /* set basic fragment info */
    var fragStatements = ListBuffer[IR_Statement]()

    fragStatements ++= setupFragmentPosition()
    fragStatements ++= setupFragmentIndex()
    fragStatements += setupCommId()
    fragStatements += setupFragmentId()
    fragStatements ++= setupFragmentPosBeginAndEnd()

    if (Knowledge.mpi_enabled) {
      if (Knowledge.waLBerla_useGridPartFromExa && Knowledge.waLBerla_createCartComm) {
        init += IR_MemberFunctionCallArrow(IR_VariableAccess("MPIManager::instance()", IR_UnknownDatatype), "resetMPI")
        init += IR_MemberFunctionCallArrow(IR_VariableAccess("MPIManager::instance()", IR_UnknownDatatype), "createCartesianComm",
          ListBuffer[IR_Expression](
            Knowledge.domain_rect_numBlocks_x, Knowledge.domain_rect_numBlocks_y, Knowledge.domain_rect_numBlocks_z, // number of processes
            Knowledge.domain_rect_periodic_x, Knowledge.domain_rect_periodic_y, Knowledge.domain_rect_periodic_z)) // periodicity
      }

      // assign exas MPI comm IV to waLBerla's communicator
      init += IR_Assignment(MPI_IV_MpiComm,
        IR_MemberFunctionCallArrowWithDt(IR_VariableAccess("MPIManager::instance()", IR_UnknownDatatype), "comm", MPI_IV_MpiComm.datatype))
      init += IR_Assignment(MPI_IV_MpiRank,
        IR_MemberFunctionCallArrowWithDt(IR_VariableAccess("MPIManager::instance()", IR_UnknownDatatype), "rank", MPI_IV_MpiRank.datatype))
      init += IR_Assignment(MPI_IV_MpiSize,
        IR_MemberFunctionCallArrowWithDt(IR_VariableAccess("MPIManager::instance()", IR_UnknownDatatype), "numProcesses", MPI_IV_MpiSize.datatype))
    }

    init += IR_WaLBerlaLoopOverLocalBlockArray(fragStatements, IR_ParallelizationInfo(potentiallyParallel = true))

    /* get local block list indices from remote neighbors via wb communication */
    if (Knowledge.mpi_enabled) {
      val ranksToComm = IR_VariableAccess("ranksToComm", IR_SpecialDatatype("std::set<int>"))
      val bufferSystem = IR_VariableAccess("bufferSystem", IR_SpecialDatatype("mpi::BufferSystem"))
      val bufferSystemTag = "commLocalBlockIdx".toCharArray.sum

      communicate += IR_ObjectInstantiation(bufferSystem, MPI_IV_MpiComm, bufferSystemTag)
      communicate += IR_VariableDeclaration(ranksToComm)

      // pack map entries (i.e. BlockID and local block list index) into buffer
      if (canHaveLocalNeighs || canHaveRemoteNeighs || canHavePeriodicity) {
        val packBufferStream = IR_VariableAccess("packBufferStream", IR_SpecialDatatype("auto &"))

        val fillBufferForNeigh = neighbors.map(neigh => {
          val wbNeighborHoodSectionIdx = IR_WaLBerlaNeighborHoodSectionIndex(neigh.dir)
          val wbNeighborProcess = block.getNeighborProcess(wbNeighborHoodSectionIdx, wbNeighborIdx)

          IR_WaLBerlaLoopOverBlockNeighborhoodSection(neigh.dir,
            IR_IfCondition(hasRemoteNeighbor(wbNeighborHoodSectionIdx),
              ListBuffer[IR_Statement](
                IR_VariableDeclaration(packBufferStream, IR_MemberFunctionCall(bufferSystem, "sendBuffer", wbNeighborProcess)),
                IR_MemberFunctionCall(ranksToComm, "insert", wbNeighborProcess), // insert neigh process to list of comm participants
                IR_Print(packBufferStream, block.getId(), defIt))))
        })

        communicate += IR_WaLBerlaLoopOverLocalBlockArray(IR_Scope(fillBufferForNeigh : _*), IR_ParallelizationInfo(potentiallyParallel = true))
      }

      // transmit list of comm participants and communicate
      communicate += IR_MemberFunctionCall(bufferSystem, "setReceiverInfo", ranksToComm, false)
      communicate += IR_MemberFunctionCall(bufferSystem, "sendAll")

      // unpack values and insert into map
      val unpackIt = IR_VariableAccess("recvIt", IR_SpecialDatatype("auto"))
      val unpackBufferStream = IR_VariableAccess("unpackBufferStream", IR_SpecialDatatype("auto"))
      val unpackedBlockID = IR_VariableAccess("bId", IR_SpecialDatatype("BlockID"))
      val unpackedLocalIdx = IR_VariableAccess("localBlockIndex", IR_IntegerDatatype)

      communicate += IR_ForLoop(IR_VariableDeclaration(unpackIt, IR_MemberFunctionCall(bufferSystem, "begin")),
        unpackIt Neq IR_MemberFunctionCall(bufferSystem, "end"),
        IR_PreIncrement(unpackIt),
        ListBuffer[IR_Statement](
          IR_VariableDeclaration(unpackBufferStream, IR_MemberFunctionCall(unpackIt, "buffer")),
          IR_WhileLoop(
            IR_Negation(IR_MemberFunctionCall(unpackBufferStream, "isEmpty")),
            ListBuffer[IR_Statement](
              IR_VariableDeclaration(unpackedBlockID),
              IR_VariableDeclaration(unpackedLocalIdx),
              IR_Read(unpackBufferStream, unpackedBlockID, unpackedLocalIdx),
              IR_Assignment(
                IR_ArrayAccess(IR_WaLBerlaLocalBlockIndicesFromRemote(), unpackedBlockID),
                unpackedLocalIdx)))))
    }

    /* set fragment connection */

    // TODO: masked blocks (or fragments)
    connect += IR_Assignment(IR_IV_IsValidForDomain(domainIdx), IR_ConnectFragments().isPointInsideDomain(IR_IV_FragmentPosition(_), domain))

    if (canHaveLocalNeighs || canHaveRemoteNeighs || canHavePeriodicity) {
      for (neigh <- neighbors) {
        var statements = ListBuffer[IR_Statement]()

        // find array index of neighbor block in local block vector
        val wbLocalNeighborBlockIdx = IR_VariableAccess("localNeighborBlockIdx", IR_IntegerDatatype)
        val compareBlockIDs = IR_Native(s"[&${ wbNeighborBlockId.name }](Block* b) { return b->getId().getID() == ${ wbNeighborBlockId.name }.getID(); }")

        def findLocalNeighborBlockIndex() = {
          val findEntry = IR_FunctionCall("std::find_if",
            IR_MemberFunctionCall(IR_WaLBerlaLocalBlocks(), "begin"), IR_MemberFunctionCall(IR_WaLBerlaLocalBlocks(), "end"),
            compareBlockIDs)

          IR_TernaryCondition(findEntry Neq IR_MemberFunctionCall(IR_WaLBerlaLocalBlocks(), "end"),
            IR_FunctionCall("std::distance", IR_MemberFunctionCall(IR_WaLBerlaLocalBlocks(), "begin"), findEntry),
            invalidIndex)
        }

        // find array index of neighbor block in remote block vector
        val wbRemoteNeighborBlockIdx = IR_VariableAccess("remoteNeighborBlockIdx", IR_IntegerDatatype)

        val wbNeighborHoodSectionIdx = IR_WaLBerlaNeighborHoodSectionIndex(neigh.dir)
        val wbNeighborProcess = block.getNeighborProcess(wbNeighborHoodSectionIdx, wbNeighborIdx)

        def findRemoteNeighborBlockIndex() = IR_ArrayAccess(IR_WaLBerlaLocalBlockIndicesFromRemote(), wbNeighborBlockId)

        // compile connect calls
        def localConnect() = IR_ConnectFragments().connectLocalElement(
          defIt, wbLocalNeighborBlockIdx, neigh.index, domainIdx, Some(wbNeighborIdx))

        def remoteConnect() = IR_ConnectFragments().connectRemoteElement(
          defIt, wbRemoteNeighborBlockIdx, wbNeighborProcess, neigh.index, domainIdx, Some(wbNeighborIdx))

        // assemble loop over neighbors and connect frags
        var bodyNeighborHoodLoop = ListBuffer[IR_Statement]()
        if (canHaveRemoteNeighs || canHaveLocalNeighs) {
          // get neighbor block id
          bodyNeighborHoodLoop += IR_VariableDeclaration(wbNeighborBlockId, block.getNeighborId(wbNeighborHoodSectionIdx, wbNeighborIdx))
          // find local neighbor index in block vector
          bodyNeighborHoodLoop += IR_VariableDeclaration(wbLocalNeighborBlockIdx, findLocalNeighborBlockIndex())
          bodyNeighborHoodLoop += IR_VariableDeclaration(wbRemoteNeighborBlockIdx, findRemoteNeighborBlockIndex())

          // determine (static) refinement case per block and neighbor dir
          val refLevel = IR_WaLBerlaRefinementLevel()
          val neighborRefLevel = blockForest.getLevelFromBlockId(wbNeighborBlockId)
          bodyNeighborHoodLoop += IR_IfCondition(refLevel < neighborRefLevel,
            IR_Assignment(IR_IV_NeighborRefinementCase(defIt, domainIdx, neigh.index), RefinementCase.C2F.id),
            IR_IfCondition(refLevel > neighborRefLevel,
              IR_Assignment(IR_IV_NeighborRefinementCase(defIt, domainIdx, neigh.index), RefinementCase.F2C.id),
              IR_Assignment(IR_IV_NeighborRefinementCase(defIt, domainIdx, neigh.index), RefinementCase.EQUAL.id)))

          // init refinement index of coarse neighbor with geometry info
          // 1. compare coarse/fine block AABBs (0 if current block aabb center is smaller than neighbor in current dim)
          // 2. disregard comm dir
          // 3. linearize index with [maxFineNeighborsPerDim, ..., maxFineNeighborsPerDim, 1]
          val neighAABB = IR_VariableAccess("neighAABB", IR_WaLBerlaAABB.datatype)
          val dimFromDir = neigh.dir.indexWhere(_ != 0)
          val refinementIndexOffsetPerDim = Knowledge.dimensions.filter(d => d != dimFromDir).map(d =>
            IR_VariableDeclaration(IR_IntegerDatatype, s"refIdxOff_$d",
              IR_TernaryCondition(
                IR_Lower(
                  getBlockAABB.center(d),
                  IR_ArrayAccess(IR_MemberFunctionCall(neighAABB, "center"), d)),
                0,
                1)))
          val stride : Seq[IR_Expression] = ((0 until Knowledge.dimensionality - 1).map(_ => Knowledge.refinement_maxFineNeighborsPerDim) :+ 1).map(IR_IntegerConstant(_))

          bodyNeighborHoodLoop += IR_IfCondition(refLevel > neighborRefLevel,
            ListBuffer[IR_Statement](
              IR_VariableDeclaration(neighAABB),
              blockForest.getAABBFromBlockId(neighAABB, wbNeighborBlockId)) ++ (
              refinementIndexOffsetPerDim :+
              IR_Assignment(IR_WaLBerlaRefinementIndexForCoarseNeighbor(neigh.index, defIt),
                IR_Linearization.linearizeIndex(
                  IR_ExpressionIndex(refinementIndexOffsetPerDim.map(IR_VariableAccess(_)) : _*),
                  IR_ExpressionIndex(stride : _*))))
          )

          // connect local/remote fragments
          if (canHaveRemoteNeighs && canHaveLocalNeighs)
            bodyNeighborHoodLoop ++= ListBuffer[IR_Statement](
              IR_IfCondition(hasLocalNeighbor(wbNeighborHoodSectionIdx), localConnect(),
                IR_IfCondition(hasRemoteNeighbor(wbNeighborHoodSectionIdx), remoteConnect())))
          else if (canHaveRemoteNeighs)
            bodyNeighborHoodLoop += IR_IfCondition(hasRemoteNeighbor(wbNeighborHoodSectionIdx), remoteConnect())
          else if (canHaveLocalNeighs)
            bodyNeighborHoodLoop += IR_IfCondition(hasLocalNeighbor(wbNeighborHoodSectionIdx), localConnect())
        }
        statements += IR_WaLBerlaLoopOverBlockNeighborhoodSection(neigh.dir, bodyNeighborHoodLoop : _*)

        // wrap in scope due to local variable declarations
        connect += IR_WaLBerlaLoopOverLocalBlockArray(IR_Scope(statements), IR_ParallelizationInfo(potentiallyParallel = true))
      }
    }

    IR_WaLBerlaPlainFunction(name, IR_UnitDatatype, ListBuffer(), checks ++ init ++ communicate ++ connect)
  }
}
