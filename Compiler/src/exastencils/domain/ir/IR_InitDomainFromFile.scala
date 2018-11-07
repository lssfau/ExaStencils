package exastencils.domain.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.DefaultNeighbors
import exastencils.communication.ir.IR_IV_CommunicationId
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.deprecated.domain.ir.IR_ReadValueFrom
import exastencils.globals.ir.IR_AllocateDataFunction
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.parallelization.ir.IR_ParallelizationInfo
import exastencils.prettyprinting.PpStream

case class IR_ReadStream(var stream : IR_Access, var toPrint : ListBuffer[IR_Access]) extends IR_Statement {
  override def prettyprint(out : PpStream) = out << stream << " >> " <<< (toPrint, " >> ") << ';'
}

case class IR_InitDomainFromFile() extends IR_FuturePlainFunction {
  override var name = "initDomain"
  override def prettyprint_decl() = prettyprint

  def setIterationOffset() = {
    IR_Comment("Iteration offset not implemented")
  }

  def connectLocalElement(localFragmentIdx : IR_Expression, neighFragmentIdx : IR_Expression, neighIdx : Int, domain : Int) = {
    // localFragmentIdx = my index
    // neighIdx = neigh.index
    // neighFragmentIdx <-- from file
    ListBuffer[IR_Statement](
      IR_Assignment(IR_IV_NeighborIsValid(domain, neighIdx, Duplicate(localFragmentIdx)), true),
      IR_Assignment(IR_IV_NeighborIsRemote(domain, neighIdx, Duplicate(localFragmentIdx)), false),
      IR_Assignment(IR_IV_NeighborFragmentIdx(domain, neighIdx, Duplicate(localFragmentIdx)), neighFragmentIdx),
      setIterationOffset())
  }

  // remoteRank = blockID
  def connectRemoteElement(localFragmentIdx : IR_Expression, localNeighIdx : IR_Expression, remoteRank : IR_Expression, neighIdx : Int, domain : Int) = {
    ListBuffer[IR_Statement](
      IR_Assignment(IR_IV_NeighborIsValid(domain, neighIdx, Duplicate(localFragmentIdx)), true),
      IR_Assignment(IR_IV_NeighborIsRemote(domain, neighIdx, Duplicate(localFragmentIdx)), true),
      IR_Assignment(IR_IV_NeighborFragmentIdx(domain, neighIdx, Duplicate(localFragmentIdx)), localNeighIdx),
      IR_Assignment(IR_IV_NeighborRemoteRank(domain, neighIdx, Duplicate(localFragmentIdx)), remoteRank),
      setIterationOffset())
  }

  def connectFragmentFromFile(neighbor_blockID : IR_VariableAccess, neighbor_commID : IR_VariableAccess) = {
    var body = new ListBuffer[IR_Statement]

    val neighbors = DefaultNeighbors.neighbors
    val domains = IR_DomainCollection.objects

    for (d <- domains.indices) {
      body += IR_Assignment(IR_IV_IsValidForDomain(d), IR_ReadValueFrom(IR_BooleanDatatype, "data"))
    }

    if (Knowledge.domain_canHaveLocalNeighs || Knowledge.domain_canHaveRemoteNeighs || Knowledge.domain_rect_hasPeriodicity) {
      for (neigh <- neighbors) {
        var statements = ListBuffer[IR_Statement]()

        // compile connect calls
        val neigh_commID = IR_ArrayAccess(neighbor_commID, neigh.index)
        val neigh_blockID = IR_ArrayAccess(neighbor_blockID, neigh.index)

        def localConnect(domainIdx : Int) = connectLocalElement(IR_LoopOverFragments.defIt,
          neigh_commID, neigh.index, domainIdx)
        def remoteConnect(domainIdx : Int) = connectRemoteElement(IR_LoopOverFragments.defIt,
          neigh_commID, neigh_blockID, neigh.index, domainIdx)

        for (d <- domains.indices) {
          statements += IR_IfCondition(IR_EqEq(neigh_blockID, -1),
            IR_Assignment(IR_IV_NeighborIsValid(d, neigh.index, Duplicate(IR_LoopOverFragments.defIt)), false),
            IR_IfCondition(IR_EqEq(neigh_blockID, MPI_IV_MpiRank),
              localConnect(d),
              IR_IfCondition(IR_Neq(neigh_blockID, MPI_IV_MpiRank),
              remoteConnect(d))
            )
          )

          //statements += IR_IfCondition(IR_EqEq(neigh_blockID, IR_LoopOverFragments.defIt),
          //  localConnect(d),
          //  remoteConnect(d)
          //)

        }

        // wrap in scope due to local variable declarations
        body += IR_Scope(statements)
      }
    }

    body
  }


  override def generateFct() = {
    var body = ListBuffer[IR_Statement]()

    //Logger.warn("Test: I was executed in InitDomainFromFile!")

    // TODO: move to main application
    if (Knowledge.mpi_enabled)
      body += IR_Assert(IR_EqEq(s"mpiSize", Knowledge.domain_numBlocks),
        ListBuffer("\"Invalid number of MPI processes (\"", "mpiSize", "\") should be \"", Knowledge.mpi_numThreads),
        IR_FunctionCall("exit", 1))

    // open file
    def file = IR_VariableAccess("file", IR_SpecialDatatype("std::ifstream"))   // I think I could also use val here
    body += IR_VariableDeclaration(file)
    body += IR_MemberFunctionCall(file, "open", IR_StringConstant(Knowledge.experimental_domain_file + "/b") + IR_FunctionCall("std::to_string", MPI_IV_MpiRank) + IR_StringConstant(".block"))

    // FIXME bad assert, should cover all mpi-ranks and also give information about file (like its name)
    body += IR_Assert(IR_MemberFunctionCall(file, "is_open"), ListBuffer("\"Unable to open file\""), IR_FunctionCall("exit", 1))

    def iss = IR_VariableAccess("iss", IR_SpecialDatatype("std::istringstream"))
    body += IR_VariableDeclaration(iss)

    def strBuf = IR_VariableAccess("strBuf", IR_SpecialDatatype("std::string"))
    body += IR_VariableDeclaration(strBuf)

    //get_line(file)
    val read_line = IR_FunctionCall(IR_ReadLineFromFile.name, file, iss)

    def n_fragments = IR_VariableAccess("n_fragments", IR_IntegerDatatype)      // TODO find this
    body += IR_VariableDeclaration(n_fragments)
    def n_grid_nodes = IR_VariableAccess("n_grid_nodes", IR_IntegerDatatype)    // TODO find this
    body += IR_VariableDeclaration(n_grid_nodes)

    // jump over block_id information
    body += read_line

    body += read_line
    body += IR_ReadStream(iss, ListBuffer(strBuf, n_fragments))

    body += read_line
    body += IR_ReadStream(iss, ListBuffer(strBuf, n_grid_nodes))

    ////////////////////
    // read fragments //
    var fragment_statements = ListBuffer[IR_Statement]()

    def fragment_id = IR_VariableAccess("fragment_id", IR_IntegerDatatype)
    def neighbor_blockID = IR_VariableAccess("neighbor_blockID", IR_ArrayDatatype(IR_IntegerDatatype, 4))
    def neighbor_commID = IR_VariableAccess("neighbor_commID", IR_ArrayDatatype(IR_IntegerDatatype, 4))
    fragment_statements += IR_VariableDeclaration(fragment_id)
    fragment_statements += IR_VariableDeclaration(neighbor_blockID)
    fragment_statements += IR_VariableDeclaration(neighbor_commID)

    def x_nodes = IR_VariableAccess("x_nodes", IR_PointerDatatype(IR_FloatDatatype))    // TODO find this
    def y_nodes = IR_VariableAccess("y_nodes", IR_PointerDatatype(IR_FloatDatatype))    // TODO find this
    fragment_statements += IR_VariableDeclaration(x_nodes)
    fragment_statements += IR_VariableDeclaration(y_nodes)
    fragment_statements += IR_ArrayAllocation(x_nodes, IR_FloatDatatype, n_grid_nodes * n_grid_nodes)
    fragment_statements += IR_ArrayAllocation(y_nodes, IR_FloatDatatype, n_grid_nodes * n_grid_nodes)

    fragment_statements += read_line
    fragment_statements += IR_ReadStream(iss, ListBuffer(strBuf, fragment_id))

    // Assign values to the "real" variables
    fragment_statements += IR_Assignment(IR_IV_FragmentId(IR_LoopOverFragments.defIt), fragment_id)
    fragment_statements += IR_Assignment(IR_IV_CommunicationId(IR_LoopOverFragments.defIt), IR_LoopOverFragments.defIt)

    def i = IR_VariableAccess("i", IR_IntegerDatatype)
    // get comm_ids of neighbors
    fragment_statements += IR_ForLoop(
      IR_VariableDeclaration(i, 0),
      IR_Lower(i, 4),
      IR_PreIncrement(i),
      ListBuffer[IR_Statement](
        read_line,
        IR_ReadStream(iss, ListBuffer(strBuf, IR_ArrayAccess(neighbor_blockID, i), IR_ArrayAccess(neighbor_commID, i)))
      )
    )

    // get all grid-points
    fragment_statements += IR_ForLoop(
      IR_VariableDeclaration(i, 0),
      IR_Lower(i, n_grid_nodes * n_grid_nodes),
      IR_PreIncrement(i),
      ListBuffer[IR_Statement](
        read_line,
        IR_ReadStream(iss, ListBuffer(IR_ArrayAccess(x_nodes, i), IR_ArrayAccess(y_nodes, i)))
      )
    )

    // get fragmentPosBegin_x, fragmentPosBegin_y, fragmentPosEnd_x, fragmentPosEnd_y
    // FIXME HACK for uniform grid test. It is not wrong but actually not required. Might be removed later on.
    fragment_statements += IR_Assignment(IR_IV_FragmentPositionBegin(0), IR_ArrayAccess(x_nodes, 0))
    fragment_statements += IR_Assignment(IR_IV_FragmentPositionBegin(1), IR_ArrayAccess(y_nodes, 0))
    fragment_statements += IR_Assignment(IR_IV_FragmentPositionEnd(0), IR_ArrayAccess(x_nodes, n_grid_nodes * n_grid_nodes - 1))
    fragment_statements += IR_Assignment(IR_IV_FragmentPositionEnd(1), IR_ArrayAccess(y_nodes, n_grid_nodes * n_grid_nodes - 1))



    // free memory
    fragment_statements += IR_ArrayFree(x_nodes)
    fragment_statements += IR_ArrayFree(y_nodes)

    fragment_statements ++= connectFragmentFromFile(neighbor_blockID, neighbor_commID)


    body += IR_LoopOverFragments(fragment_statements, IR_ParallelizationInfo())

    body += IR_MemberFunctionCall(file, "close")


    // FIXME: move to app
    body += IR_FunctionCall(IR_AllocateDataFunction.fctName)

    IR_PlainFunction(name, IR_UnitDatatype, body)
  }
}
