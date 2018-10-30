package exastencils.domain.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.prettyprinting.PpStream

case class IR_ReadStream(var stream : IR_Access, var toPrint : ListBuffer[IR_Access]) extends IR_Statement {
  override def prettyprint(out : PpStream) = out << stream << " >> " <<< (toPrint, " >> ") << ';'
}

case class IR_InitDomainFromFile() extends IR_FuturePlainFunction {
  override var name = "initDomainFromFile"
  override def prettyprint_decl() = prettyprint


  override def generateFct() = {
    var body = ListBuffer[IR_Statement]()

    Logger.warn("Test: I was executed in InitDomainFromFile!")

    // TODO: move to main application
    if (Knowledge.mpi_enabled)
      body += IR_Assert(IR_EqEq(s"mpiSize", Knowledge.domain_numBlocks),
        ListBuffer("\"Invalid number of MPI processes (\"", "mpiSize", "\") should be \"", Knowledge.mpi_numThreads),
        IR_FunctionCall("exit", 1))

    // open file
    def file = IR_VariableAccess("file", IR_SpecialDatatype("std::ifstream"))   // I think I could also use val here
    body += IR_VariableDeclaration(file)
    body += IR_MemberFunctionCall(file, "open", IR_StringConstant(s"BSG_FOLDER/b") + MPI_IV_MpiRank + IR_StringConstant(".block"), IR_StringLiteral("std::ios::binary | std::ios::ate | std::ios::in"))

    def iss = IR_VariableAccess("iss", IR_SpecialDatatype("std::istringstream&"))
    body += IR_VariableDeclaration(iss)

    def strBuf = IR_VariableAccess("strBuf", IR_SpecialDatatype("std::string"))
    body += IR_VariableDeclaration(strBuf)

    //get_line(file)
    val read_line = IR_FunctionCall("DomainGenerated_readLine", file, iss)

    def n_fragments = IR_VariableAccess("n_fragments", IR_IntegerDatatype)
    body += IR_VariableDeclaration(n_fragments)
    def n_grid_nodes = IR_VariableAccess("n_grid_nodes", IR_IntegerDatatype)
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

    def x_nodes = IR_VariableAccess("x_nodes", IR_PointerDatatype(IR_FloatDatatype))
    def y_nodes = IR_VariableAccess("y_nodes", IR_PointerDatatype(IR_FloatDatatype))
    def z_nodes = IR_VariableAccess("z_nodes", IR_PointerDatatype(IR_FloatDatatype))
    fragment_statements += IR_VariableDeclaration(x_nodes)
    fragment_statements += IR_VariableDeclaration(y_nodes)
    fragment_statements += IR_VariableDeclaration(z_nodes)
    fragment_statements += IR_ArrayAllocation(x_nodes, IR_FloatDatatype, n_grid_nodes * n_grid_nodes)
    fragment_statements += IR_ArrayAllocation(y_nodes, IR_FloatDatatype, n_grid_nodes * n_grid_nodes)
    fragment_statements += IR_ArrayAllocation(z_nodes, IR_FloatDatatype, n_grid_nodes * n_grid_nodes)

    fragment_statements += read_line
    fragment_statements += IR_ReadStream(iss, ListBuffer(strBuf, fragment_id))

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
        IR_ReadStream(iss, ListBuffer(strBuf, IR_ArrayAccess(x_nodes, i), IR_ArrayAccess(y_nodes, i), IR_ArrayAccess(z_nodes, i)))
      )
    )

    // free memory
    fragment_statements += IR_ArrayFree(x_nodes)
    fragment_statements += IR_ArrayFree(y_nodes)
    fragment_statements += IR_ArrayFree(z_nodes)


    def comm_id = IR_VariableAccess("comm_id", IR_IntegerDatatype)
    body += IR_ForLoop(
      IR_VariableDeclaration(comm_id, 0),
      IR_Lower(comm_id, n_fragments),
      IR_PreIncrement(comm_id),
      fragment_statements
    )

    body += IR_MemberFunctionCall(iss, "close")
    body += IR_MemberFunctionCall(file, "close")




    // set up stmts per fragment
    //var fragStatements = ListBuffer[IR_Statement]()

    //fragStatements += IR_Assignment(IR_IV_FragmentId(), ???)
    // ... other variables

    // add fragment statements and loop
    //body += IR_LoopOverFragments(fragStatements, IR_ParallelizationInfo())

    //body += IR_ConnectFragments()

    // FIXME: move to app
    //body += IR_FunctionCall(IR_AllocateDataFunction.fctName)

    IR_PlainFunction(name, IR_UnitDatatype, body)
  }
}
