package exastencils.domain.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.globals.ir.IR_AllocateDataFunction
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.parallelization.ir.IR_ParallelizationInfo

case class IR_InitDomainFromFile() extends IR_FuturePlainFunction {
  override var name = "initDomain"
  override def prettyprint_decl() = prettyprint

  override def generateFct() = {
    var body = ListBuffer[IR_Statement]()

    // TODO: move to main application
    if (Knowledge.mpi_enabled)
      body += IR_Assert(IR_EqEq(s"mpiSize", Knowledge.domain_numBlocks),
        ListBuffer("\"Invalid number of MPI processes (\"", "mpiSize", "\") should be \"", Knowledge.mpi_numThreads),
        IR_FunctionCall("exit", 1))

    // open file
    // cf deprecated.domain.ir.IR_InitDomainFromFragmentFile
    def file = IR_VariableAccess("file", IR_SpecialDatatype("std::ifstream"))
    body += IR_VariableDeclaration(file)
    body += IR_MemberFunctionCall(file, "open", IR_StringConstant(s"./myfile_") + MPI_IV_MpiRank + IR_StringConstant(".whatever"), IR_StringLiteral("std::ios::binary | std::ios::ate | std::ios::in"))

    // set up stmts per fragment
    var fragStatements = ListBuffer[IR_Statement]()

    fragStatements += IR_Assignment(IR_IV_FragmentId(), ???)
    // ... other variables

    // add fragment statements and loop
    body += IR_LoopOverFragments(fragStatements, IR_ParallelizationInfo())

    body += IR_ConnectFragments()

    // FIXME: move to app
    body += IR_FunctionCall(IR_AllocateDataFunction.fctName)

    IR_PlainFunction(name, IR_UnitDatatype, body)
  }
}
