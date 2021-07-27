package exastencils.visualization.ir.visit

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.parallelization.api.mpi._

/// IR_VisItSimGetDomainList
// set number of domains per processor, mandatory
// with 1 block per MPI Rank, a 1:1 ratio is used

case class IR_VisItSimGetDomainList() extends IR_FuturePlainFunction {

  import exastencils.visualization.ir.visit.IR_VisItUtil._

  override def generateFct() : IR_PlainFunction =  {
    val fctBody = ListBuffer[IR_Statement]()
    val hDecl = IR_VariableDeclaration(visitHandle, "h", visitInvalidHandle)
    val domainListDecl = IR_VariableDeclaration(visitHandle, "domain_list", visitInvalidHandle)

    fctBody += hDecl
    fctBody += domainListDecl

    fctBody += IR_IfCondition(
      IR_AndAnd(
        IR_EqEq(IR_FunctionCall(IR_ExternalFunctionReference("VisIt_DomainList_alloc"), IR_AddressOf(IR_VariableAccess(hDecl))), visitOkay),
        IR_EqEq(IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableData_alloc"), IR_AddressOf(IR_VariableAccess(domainListDecl))), visitOkay)),
      ListBuffer[IR_Statement](
        IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableData_setDataI"), IR_VariableAccess(domainListDecl), IR_Native("VISIT_OWNER_COPY"), IR_IntegerConstant(1), IR_IntegerConstant(1), IR_AddressOf(MPI_IV_MpiRank)),
        IR_FunctionCall(IR_ExternalFunctionReference("VisIt_DomainList_setDomains"), IR_VariableAccess(hDecl), Knowledge.mpi_numThreads, IR_VariableAccess(domainListDecl)))
    )

    fctBody += IR_Return(IR_VariableAccess(hDecl))

    IR_PlainFunction(
      name,
      IR_SpecialDatatype("visit_handle"),
      ListBuffer(IR_FunctionArgument("name", IR_SpecialDatatype("const char*")), IR_FunctionArgument("cbdata", IR_PointerDatatype(IR_UnitDatatype))),
      fctBody
    )
  }

  override def name : String = "SimGetDomainList"
  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint()
}
