package exastencils.visualization.ir.visit

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.visualization.ir.visit.IR_VisItUtil._
import exastencils.config._

case class IR_VisItBroadcastIntCallback() extends IR_VisItFuturePlainFunction {
  val intValue = IR_FunctionArgument("value", IR_PointerDatatype(IR_IntegerDatatype))
  val sender = IR_FunctionArgument("sender", IR_IntegerDatatype)

  override def generateFct() : IR_PlainFunction = {
    val fctBody = ListBuffer[IR_Statement]()

    fctBody += IR_Return(callExtFunction("MPI_Bcast",
      intValue.access, IR_IntegerConstant(1), IR_Native("MPI_INT"), sender.access, Knowledge.mpi_defaultCommunicator))

    IR_PlainFunction(
      name,
      IR_IntegerDatatype,
      ListBuffer(intValue, sender),
      fctBody
    )

  }

  override def name : String = "visit_broadcast_int_callback"
}

case class IR_VisItBroadcastStringCallback() extends IR_VisItFuturePlainFunction {
  val str = IR_FunctionArgument("str", IR_PointerDatatype(IR_CharDatatype))
  val len = IR_FunctionArgument("len", IR_IntegerDatatype)
  val sender = IR_FunctionArgument("sender", IR_IntegerDatatype)

  override def generateFct() : IR_PlainFunction = {
    val fctBody = ListBuffer[IR_Statement]()

    fctBody += IR_Return(callExtFunction("MPI_Bcast",
      str.access, len.access, IR_Native("MPI_CHAR"), sender.access, Knowledge.mpi_defaultCommunicator))

    IR_PlainFunction(
      name,
      IR_IntegerDatatype,
      ListBuffer(str, len, sender),
      fctBody
    )
  }

  override def name : String = "visit_broadcast_string_callback"
}

case class IR_VisItSlaveProcessCallback() extends IR_VisItFuturePlainFunction {
  override def generateFct() : IR_PlainFunction = {
    val fctBody = ListBuffer[IR_Statement]()
    val cmdDecl = IR_VariableDeclaration(IR_IntegerDatatype, "command", IR_IntegerConstant(0))

    fctBody += cmdDecl
    fctBody += callExtFunction("MPI_Bcast",
      IR_AddressOf(IR_VariableAccess(cmdDecl)), IR_IntegerConstant(1), IR_Native("MPI_INT"), IR_IntegerConstant(0), Knowledge.mpi_defaultCommunicator)

    IR_PlainFunction(
      name,
      IR_UnitDatatype,
      fctBody
    )
  }

  override def name : String = "slave_process_callback"
}