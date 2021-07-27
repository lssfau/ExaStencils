package exastencils.visualization.ir.visit

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._

abstract class IR_VisItCommunication extends IR_FuturePlainFunction {
  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint()
}

case class IR_VisItBroadcastIntCallback() extends IR_VisItCommunication {
  override def generateFct() : IR_PlainFunction = {
    val fctBody = ListBuffer[IR_Statement]()

    fctBody += IR_Return(IR_FunctionCall(IR_ExternalFunctionReference("MPI_Bcast"), IR_VariableAccess("value", IR_PointerDatatype(IR_IntegerDatatype)), IR_IntegerConstant(1), IR_Native("MPI_INT"), IR_VariableAccess("sender", IR_IntegerDatatype), Knowledge.mpi_defaultCommunicator))

    IR_PlainFunction(
      name,
      IR_IntegerDatatype,
      ListBuffer(IR_FunctionArgument("value", IR_PointerDatatype(IR_IntegerDatatype)), IR_FunctionArgument("sender", IR_IntegerDatatype)),
      fctBody
    )

  }

  override def name : String = "visit_broadcast_int_callback"
}

case class IR_VisItBroadcastStringCallback() extends IR_VisItCommunication {
  override def generateFct() : IR_PlainFunction = {
    val fctBody = ListBuffer[IR_Statement]()

    fctBody += IR_Return(IR_FunctionCall(IR_ExternalFunctionReference("MPI_Bcast"), IR_VariableAccess("str", IR_PointerDatatype(IR_CharDatatype)), IR_VariableAccess("len", IR_IntegerDatatype), IR_Native("MPI_CHAR"), IR_VariableAccess("sender", IR_IntegerDatatype), Knowledge.mpi_defaultCommunicator))

    IR_PlainFunction(
      name,
      IR_IntegerDatatype,
      ListBuffer(IR_FunctionArgument("str", IR_PointerDatatype(IR_CharDatatype)), IR_FunctionArgument("len", IR_IntegerDatatype), IR_FunctionArgument("sender", IR_IntegerDatatype)),
      fctBody
    )
  }

  override def name : String = "visit_broadcast_string_callback"
}

case class IR_VisItSlaveProcessCallback() extends IR_VisItCommunication {
  override def generateFct() : IR_PlainFunction = {
    val fctBody = ListBuffer[IR_Statement]()
    val cmdDecl = IR_VariableDeclaration(IR_IntegerDatatype, "command", IR_IntegerConstant(0))

    fctBody += cmdDecl
    fctBody += IR_FunctionCall(IR_ExternalFunctionReference("MPI_Bcast"), IR_AddressOf(IR_VariableAccess(cmdDecl)), IR_IntegerConstant(1), IR_Native("MPI_INT"), IR_IntegerConstant(0), Knowledge.mpi_defaultCommunicator)

    IR_PlainFunction(
      name,
      IR_UnitDatatype,
      fctBody
    )
  }

  override def name : String = "slave_process_callback"
}