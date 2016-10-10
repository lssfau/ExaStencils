package exastencils.mpi.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.{ IR_Function, _ }
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation.Output
import exastencils.parallelization.ir.IR_PotentiallyCritical
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_RawPrint

/// MPI_WaitForRequest

case object MPI_WaitForRequest extends IR_AbstractFunction with IR_Expandable {
  exastencils.core.Duplicate.registerImmutable(this.getClass)

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
  override def prettyprint_decl : String = prettyprint
  override def name = "waitForMPIReq"

  def request = IR_VariableAccess("request", IR_PointerDatatype(IR_SpecialDatatype("MPI_Request")))
  def stat = IR_VariableAccess("stat", IR_SpecialDatatype("MPI_Status"))
  def flag = IR_VariableAccess("flag", IR_IntegerDatatype)
  def result = IR_VariableAccess("result", IR_IntegerDatatype)

  def msg = IR_VariableAccess("msg", IR_ArrayDatatype(IR_SpecialDatatype("char"), 64 * 1024))
  def len = IR_VariableAccess("len", IR_IntegerDatatype)

  def checkError = {
    IR_IfCondition("MPI_ERR_IN_STATUS" EqEq result, ListBuffer[IR_Statement](
      IR_VariableDeclaration(msg),
      IR_VariableDeclaration(len),
      IR_FunctionCall(MPI_FunctionAccess("MPI_Error_string", IR_IntegerDatatype),
        IR_MemberAccess(stat, "MPI_ERROR"), msg, IR_AddressofExpression(len)),
      IR_RawPrint("\"MPI Error encountered (\"", msg, "\")\"")))
  }

  override def expand : Output[IR_Function] = {
    val fct = IR_Function(IR_UnitDatatype, name, IR_FunctionArgument(request), ListBuffer[IR_Statement]())
    fct.allowInlining = false

    // add declarations for local variables
    fct.body += IR_VariableDeclaration(stat)
    fct.body += IR_VariableDeclaration(result)

    if (Knowledge.mpi_useBusyWait) {
      // busy wait => generate while loop polling through MPI_Test
      fct.body += IR_VariableDeclaration(flag, 0)
      fct.body += IR_WhileLoop(0 EqEq flag,
        IR_PotentiallyCritical(IR_Assignment(result, IR_FunctionCall("MPI_Test",
          request, IR_AddressofExpression(flag), IR_AddressofExpression(stat)))),
        checkError)
    } else {
      fct.body += IR_PotentiallyCritical(IR_Assignment(result, IR_FunctionCall("MPI_Wait", request, IR_AddressofExpression(stat))))
      fct.body += checkError
    }
    fct.body += IR_Assignment(IR_DerefAccess(request), IR_FunctionCall("MPI_Request"))

    fct
  }
}
