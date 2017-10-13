package exastencils.timing.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core.StateManager
import exastencils.parallelization.api.mpi.MPI_AllReduce
import exastencils.util.ir.IR_RawPrint

/// IR_PrintAllTimers

case class IR_PrintAllTimers() extends IR_TimerFunction {
  override var name = "printAllTimers"
  override def prettyprint_decl() : String = prettyprint

  def genPrintTimerCode(timer : IR_IV_Timer) : IR_Statement = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    val timeToPrint = "getTotalTime"
    def timerValue = IR_VariableAccess("timerValue", IR_DoubleDatatype)
    statements += IR_VariableDeclaration(timerValue, IR_FunctionCall(timeToPrint, timer.resolveName()))

    if (Knowledge.mpi_enabled) {
      statements += MPI_AllReduce(IR_AddressOf(timerValue), timerValue.datatype, 1, "+")
      statements += IR_Assignment(timerValue, "mpiSize", "/=")
    }

    statements += IR_RawPrint("\"Mean mean total time for Timer " + timer.name + ":\"", "timerValue")

    IR_Scope(statements)
  }

  override def generateFct() = {
    IR_CollectTimers.applyStandalone(StateManager.root)
    val timers = IR_CollectTimers.timers

    val body = timers.toList.sortBy(_._1).map(t => genPrintTimerCode(t._2)).to[ListBuffer]

    val fct = IR_PlainFunction(name, IR_UnitDatatype, body)
    fct.allowFortranInterface = false
    fct
  }
}
