package exastencils.timing.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core.StateManager
import exastencils.parallelization.api.mpi._
import exastencils.util.ir.IR_RawPrint

object CommTimerAnnot extends Enumeration {
  type Access = Value
  final val ANNOT : String = "CommTimer"
  final val COMM, APPLYBC = Value

  exastencils.core.Duplicate.registerConstant(this)
}

case class IR_PrintAllCommunicationTimers() extends IR_TimerFunction {
  override var name = "printAllCommunicationTimers"
  override def prettyprint_decl() : String = prettyprint

  private val commTimeAccum = IR_VariableAccess("totalCommTime", IR_DoubleDatatype)
  private val applyBCTimeAccum = IR_VariableAccess("totalApplyBCTime", IR_DoubleDatatype)

  def genPrintTimerCode(timer : IR_IV_Timer, annot : CommTimerAnnot.Access) : IR_Statement = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    def accum = if (annot == CommTimerAnnot.COMM) commTimeAccum else applyBCTimeAccum

    val timeToPrint = "getTotalTime"
    def timerValue = IR_VariableAccess("timerValue", IR_DoubleDatatype)
    statements += IR_VariableDeclaration(timerValue, IR_FunctionCall(timeToPrint, timer.resolveName()))

    if (Knowledge.mpi_enabled) {
      statements += MPI_AllReduce(IR_AddressOf(timerValue), timerValue.datatype, 1, "+")
      statements += IR_Assignment(timerValue, MPI_IV_MpiSize, "/=")
    }

    statements += IR_RawPrint("\"Mean mean total time for Timer " + timer.name + ":\"", timerValue)
    statements += IR_Assignment(accum, timerValue, "+=")

    IR_Scope(statements)
  }

  override def generateFct() = {
    IR_CollectTimers.applyStandalone(StateManager.root)
    val timers = IR_CollectTimers.timers

    val body = ListBuffer[IR_Statement]()
    body += IR_VariableDeclaration(commTimeAccum, 0.0)
    body += IR_VariableDeclaration(applyBCTimeAccum, 0.0)
    timers.values.filter(_.hasAnnotation(CommTimerAnnot.ANNOT)).toList.sortBy(_.name).foreach { timer =>
      val annot = timer.popAnnotationAs[CommTimerAnnot.Access](CommTimerAnnot.ANNOT)
      body += genPrintTimerCode(timer, annot)
    }

    body += IR_RawPrint("\"Mean mean total time for all communication :\"", commTimeAccum)
    body += IR_RawPrint("\"Mean mean total time for all applyBC :\"", applyBCTimeAccum)
    body += IR_RawPrint("\"Mean mean total time for all communication + applyBC :\"", commTimeAccum + applyBCTimeAccum)

    val fct = IR_PlainFunction(name, IR_UnitDatatype, body)
    fct.allowFortranInterface = false
    fct
  }
}
