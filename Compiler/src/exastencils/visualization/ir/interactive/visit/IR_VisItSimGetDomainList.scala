package exastencils.visualization.ir.interactive.visit

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.parallelization.api.mpi._
import exastencils.visualization.ir.interactive.visit.IR_VisItGlobals._
import exastencils.visualization.ir.interactive.visit.IR_VisItUtil.callExtFunction


/// IR_VisItSimGetDomainList
// set number of domains per processor, mandatory
// with 1 block per MPI Rank, a 1:1 ratio is used

case class IR_VisItSimGetDomainList() extends IR_VisItFuturePlainFunction {

  override def generateFct() : IR_PlainFunction = {
    val fctBody = ListBuffer[IR_Statement]()
    val h = IR_VariableAccess("h", visitHandle)
    val domainList = IR_VariableAccess("domain_list", visitHandle)

    fctBody += IR_VariableDeclaration(h, visitInvalidHandle)
    fctBody += IR_VariableDeclaration(domainList, visitInvalidHandle)

    fctBody += IR_IfCondition(
      IR_AndAnd(
        IR_EqEq(callExtFunction("VisIt_DomainList_alloc", IR_AddressOf(h)), visitOkay),
        IR_EqEq(callExtFunction("VisIt_VariableData_alloc", IR_AddressOf(domainList)), visitOkay)),
      ListBuffer[IR_Statement](
        callExtFunction("VisIt_VariableData_setDataI", domainList, IR_Native("VISIT_OWNER_COPY"), IR_IntegerConstant(1), IR_IntegerConstant(1), IR_AddressOf(MPI_IV_MpiRank)),
        callExtFunction("VisIt_DomainList_setDomains", h, Knowledge.mpi_numThreads, domainList))
    )

    fctBody += IR_Return(h)

    IR_PlainFunction(
      name,
      IR_SpecialDatatype("visit_handle"),
      ListBuffer(IR_FunctionArgument("name", IR_SpecialDatatype("const char*")), IR_FunctionArgument("cbdata", IR_PointerDatatype(IR_UnitDatatype))),
      fctBody
    )
  }

  override def name : String = "SimGetDomainList"
}
