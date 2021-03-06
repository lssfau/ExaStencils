package exastencils.visualization.ir.interactive.visit

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.parallelization.api.mpi._
import exastencils.visualization.ir.interactive.visit.IR_VisItUtil.callExtFunction

/// IR_VisItProcessVisItCommand
// additional handling for parallel simulations because only root communicates with VisIt

case class IR_VisItProcessVisItCommand() extends IR_VisItFuturePlainFunction {

  override def generateFct() : IR_PlainFunction = {
    val fctBody = ListBuffer[IR_Statement]()

    val command = IR_VariableAccess("command", IR_IntegerDatatype)

    fctBody += IR_VariableDeclaration(command)
    fctBody += IR_IfCondition(
      MPI_IsRootProc.apply(),
      ListBuffer[IR_Statement](
        IR_IfCondition(
          IR_EqEq(1, callExtFunction("VisItProcessEngineCommand")),
          ListBuffer[IR_Statement](
            IR_Assignment(command, 1),
            callExtFunction("MPI_Bcast", IR_AddressOf(command), 1, IR_Native("MPI_INT"), 0, MPI_IV_MpiComm),
            IR_Return(1)),
          ListBuffer[IR_Statement](
            IR_Assignment(command, 0),
            callExtFunction("MPI_Bcast", IR_AddressOf(command), 1, IR_Native("MPI_INT"), 0, MPI_IV_MpiComm),
            IR_Return(0))
        )
      ),
      ListBuffer[IR_Statement](
        IR_WhileLoop(
          1,
          ListBuffer[IR_Statement](
            callExtFunction("MPI_Bcast", IR_AddressOf(command), 1, IR_Native("MPI_INT"), 0, MPI_IV_MpiComm),
            IR_IfCondition(
              IR_EqEq(command, 0),
              callExtFunction("VisItProcessEngineCommand")),
            IR_IfCondition(
              IR_EqEq(command, 1),
              IR_Return(1)),
            IR_IfCondition(
              IR_EqEq(command, 2),
              IR_Return(0)
            ))
        ),
        IR_Return(1)
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
