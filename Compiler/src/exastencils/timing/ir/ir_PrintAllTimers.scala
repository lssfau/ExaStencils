package exastencils.timing.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core.StateManager
import exastencils.datastructures.Transformation.Output
import exastencils.mpi.ir.MPI_AllReduce
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_RawPrint

/// IR_PrintAllTimers

case class IR_PrintAllTimers() extends IR_TimerFunction with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
  override def prettyprint_decl() : String = prettyprint
  override def name = "printAllTimers"

  def genPrintTimerCode(timer : IR_IV_Timer) : IR_Statement = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    val timeToPrint = "getTotalTime"
    def timerValue = IR_VariableAccess("timerValue", IR_DoubleDatatype)
    statements += IR_VariableDeclaration(timerValue, IR_FunctionCall(timeToPrint, timer.resolveName))

    if (Knowledge.mpi_enabled) {
      statements += MPI_AllReduce(IR_AddressofExpression(timerValue), timerValue.datatype, 1, "+")
      statements += IR_Assignment(timerValue, "mpiSize", "/=")
    }

    statements += IR_RawPrint("\"Mean mean total time for Timer " + timer.name + ":\"", "timerValue")

    IR_Scope(statements)
  }

  override def expand() : Output[IR_Function] = {
    IR_CollectTimers.applyStandalone(StateManager.root)
    val timers = IR_CollectTimers.timers

    val body = timers.toList.sortBy(_._1).map(t => genPrintTimerCode(t._2)).to[ListBuffer]

    val fct = IR_Function(IR_UnitDatatype, name, body)
    fct.allowFortranInterface = false
    fct
  }
}
