package exastencils.timing.ir

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.config._
import exastencils.core.StateManager
import exastencils.datastructures.Transformation.Output
import exastencils.mpi.ir._
import exastencils.prettyprinting.PpStream
import exastencils.util.ir._

/// IR_PrintAllTimersToFile

case class IR_PrintAllTimersToFile() extends IR_TimerFunction with IR_Expandable {

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
  override def prettyprint_decl() : String = prettyprint
  override def name = "printAllTimersToFile"

  def genDataCollect(timers : HashMap[String, IR_IV_Timer]) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    var it = 0
    for (timer <- timers.toList.sortBy(_._1)) {
      statements += IR_Assignment(IR_ArrayAccess("timesToPrint", it), IR_FunctionCall("getTotalTime", timer._2.resolveName))
      it += 1
      statements += IR_Assignment(IR_ArrayAccess("timesToPrint", it), IR_FunctionCall("getMeanTime", timer._2.resolveName))
      it += 1
    }

    statements
  }

  def genPrint(timers : HashMap[String, IR_IV_Timer]) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    val stride : IR_Expression = if (Knowledge.mpi_enabled && Knowledge.l3tmp_printTimersToFileForEachRank) "mpiIt" else 0

    var it = 0
    val sep = "\"" + Settings.csvSeparatorEscaped() + "\""
    for (timer <- timers.toList.sortBy(_._1)) {
      statements += IR_Print(IR_VariableAccess("outFile"), ListBuffer[IR_Expression](
        IR_StringConstant(timer._2.name), sep,
        IR_ArrayAccess("timesToPrint", (stride * (2 * timers.size)) + it), sep,
        IR_ArrayAccess("timesToPrint", (stride * (2 * timers.size)) + it + 1), IR_StringConstant("\\n")))

      it += 2
    }

    // wrap in loop over each rank if required
    if (Knowledge.mpi_enabled && Knowledge.l3tmp_printTimersToFileForEachRank) {
      statements = ListBuffer[IR_Statement](
        IR_ForLoop(
          IR_VariableDeclaration(IR_IntegerDatatype, stride.prettyprint, 0),
          IR_LowerExpression(stride, Knowledge.mpi_numThreads),
          IR_PreIncrementExpression(stride),
          statements))
    }

    statements.prepend(IR_MemberFunctionCall(IR_VariableAccess("outFile"), "open", "\"" + Knowledge.l3tmp_timerOuputFile + "\""))
    statements.prepend(IR_VariableDeclaration(IR_SpecialDatatype("std::ofstream"), "outFile"))
    statements.append(IR_MemberFunctionCall(IR_VariableAccess("outFile"), "close"))

    statements
  }

  override def expand() : Output[IR_Function] = {
    IR_CollectTimers.applyStandalone(StateManager.root)
    val timers = IR_CollectTimers.timers

    var body : ListBuffer[IR_Statement] = ListBuffer()

    if (timers.nonEmpty) {
      if (Knowledge.l3tmp_printTimersToFileForEachRank) {
        body += IR_IfCondition(MPI_IsRootProc(),
          ListBuffer[IR_Statement](
            IR_VariableDeclaration(IR_ArrayDatatype(IR_DoubleDatatype, Knowledge.mpi_numThreads * 2 * timers.size), "timesToPrint"))
            ++ genDataCollect(timers)
            ++ ListBuffer[IR_Statement](MPI_Gather("timesToPrint", IR_DoubleDatatype, 2 * timers.size))
            ++ genPrint(timers),
          ListBuffer[IR_Statement](IR_VariableDeclaration(IR_ArrayDatatype(IR_DoubleDatatype, 2 * timers.size), "timesToPrint"))
            ++ genDataCollect(timers)
            ++ ListBuffer[IR_Statement](MPI_Gather("timesToPrint", "timesToPrint", IR_DoubleDatatype, 2 * timers.size)))
      } else {
        body += IR_VariableDeclaration(IR_ArrayDatatype(IR_DoubleDatatype, 2 * timers.size), "timesToPrint")
        body ++= genDataCollect(timers)
        body += MPI_Reduce(0, "timesToPrint", IR_DoubleDatatype, 2 * timers.size, "+")
        def timerId = IR_VariableAccess("timerId", IR_IntegerDatatype)
        body += IR_ForLoop(IR_VariableDeclaration(timerId, 0), IR_LowerExpression(timerId, 2 * timers.size), IR_PreIncrementExpression(timerId),
          IR_Assignment(IR_ArrayAccess("timesToPrint", timerId), Knowledge.mpi_numThreads, "/="))
        body += IR_IfCondition(MPI_IsRootProc(), genPrint(timers))
      }
    }

    val fct = IR_Function(IR_UnitDatatype, name, body)
    fct.allowFortranInterface = false
    fct
  }
}
