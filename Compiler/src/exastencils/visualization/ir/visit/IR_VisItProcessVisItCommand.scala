package exastencils.visualization.ir.visit

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.parallelization.api.mpi._

/// IR_VisItProcessVisItCommand
// additional handling for parallel simulations because only root communicates with VisIt

case class IR_VisItProcessVisItCommand() extends IR_FuturePlainVisItFunction {

  override def generateFct() : IR_PlainFunction =  {
    val fctBody = ListBuffer[IR_Statement]()

    val command = IR_VariableAccess("command", IR_IntegerDatatype)

    fctBody += IR_VariableDeclaration(command)
    fctBody += IR_IfCondition(
      MPI_IsRootProc.apply(),
      ListBuffer[IR_Statement](
        IR_IfCondition(
          IR_EqEq(IR_IntegerConstant(1), IR_FunctionCall(IR_ExternalFunctionReference("VisItProcessEngineCommand"))),
          ListBuffer[IR_Statement](
            IR_Assignment(command, IR_IntegerConstant(1)),
            IR_FunctionCall(IR_ExternalFunctionReference("MPI_Bcast"), IR_AddressOf(command), IR_IntegerConstant(1), IR_Native("MPI_INT"), IR_IntegerConstant(0), Knowledge.mpi_defaultCommunicator),
            IR_Return(IR_IntegerConstant(1))),
          ListBuffer[IR_Statement](
            IR_Assignment(command, IR_IntegerConstant(0)),
            IR_FunctionCall(IR_ExternalFunctionReference("MPI_Bcast"), IR_AddressOf(command), IR_IntegerConstant(1), IR_Native("MPI_INT"), IR_IntegerConstant(0), Knowledge.mpi_defaultCommunicator),
            IR_Return(IR_IntegerConstant(0)))
        )
      ),
      ListBuffer[IR_Statement](
        IR_WhileLoop(
          IR_IntegerConstant(1),
          ListBuffer[IR_Statement](
            IR_FunctionCall(IR_ExternalFunctionReference("MPI_Bcast"), IR_AddressOf(command), IR_IntegerConstant(1), IR_Native("MPI_INT"), IR_IntegerConstant(0), Knowledge.mpi_defaultCommunicator),
            IR_IfCondition(
              IR_EqEq(command, IR_IntegerConstant(0)),
              IR_FunctionCall(IR_ExternalFunctionReference("VisItProcessEngineCommand"))),
            IR_IfCondition(
              IR_EqEq(command, IR_IntegerConstant(1)),
              IR_Return(IR_IntegerConstant(1))),
            IR_IfCondition(
              IR_EqEq(command, IR_IntegerConstant(2)),
              IR_Return(IR_IntegerConstant(0))
            ))
        ),
        IR_Return(IR_IntegerConstant(1))
      )
    )

    IR_PlainFunction(
      name,
      IR_IntegerDatatype,
      fctBody
    )
  }
  override def name : String = "ProcessVisItCommand"
}
