package exastencils.timing.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.config.Knowledge
import exastencils.core.StateManager
import exastencils.parallelization.api.mpi._
import exastencils.util.ir.IR_RawPrint

object IR_AutomaticFunctionTimingCategory extends Enumeration {
  type Access = Value
  final val ANNOT : String = "TimingCategory"
  final val COMM, APPLYBC, IO = Value

  def categoryEnabled(category : Access) = {
    if (Knowledge.timer_automaticFunctionTiming) {
      category match {
        case COMM    => Knowledge.timer_automaticCommTiming
        case APPLYBC => Knowledge.timer_automaticBCsTiming
        case IO      => Knowledge.timer_automaticIOTiming
        case _       => false
      }
    } else {
      false
    }
  }

  exastencils.core.Duplicate.registerConstant(this)
}

case class IR_PrintAllAutomaticFunctionTimers() extends IR_TimerFunction {
  import IR_AutomaticFunctionTimingCategory._

  override var name = "printAllAutomaticFunctionTimers"
  override def prettyprint_decl() : String = prettyprint

  private val accumulators : Map[Access, IR_VariableAccess] = values.map(enum => enum -> IR_VariableAccess(s"accum_${enum.toString}", IR_DoubleDatatype)).toMap

  def genPrintTimerCode(timer : IR_IV_Timer, annot : Access) : IR_Statement = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    def accum = accumulators(annot)

    val timeToPrint = "getTotalTime"

    def timerValue = IR_VariableAccess("timerValue", IR_DoubleDatatype)

    if (categoryEnabled(annot)) {
      statements += IR_VariableDeclaration(timerValue, IR_FunctionCall(timeToPrint, timer.resolveName()))

      if (Knowledge.mpi_enabled) {
        statements += MPI_AllReduce(IR_AddressOf(timerValue), timerValue.datatype, 1, "+")
        statements += IR_Assignment(timerValue, MPI_IV_MpiSize, "/=")
      }

      statements += IR_RawPrint("\"Mean mean total time for Timer " + timer.name + ":\"", timerValue)
      statements += IR_Assignment(accum, timerValue, "+=")
    }

    IR_Scope(statements)
  }

  override def generateFct() = {
    IR_CollectTimers.applyStandalone(StateManager.root)
    val timers = IR_CollectTimers.timers

    val body = ListBuffer[IR_Statement]()
    body ++= accumulators.map{ case (_, vAcc) => IR_VariableDeclaration(vAcc) }
    timers.values.filter(_.hasAnnotation(ANNOT)).toList.sortBy(_.name).foreach { timer =>
      val annot = timer.popAnnotationAs[Access](ANNOT)
      body += genPrintTimerCode(timer, annot)
    }

    // print out mean total timers for automatic function timers
    def rawPrint(msg : String, expr : IR_Expression) = IR_RawPrint(ListBuffer[IR_Expression](IR_StringConstant(msg), expr))
    for ((annot, vAcc) <- accumulators)
      if (categoryEnabled(annot))
        body += rawPrint(s"Mean mean total time for all automatic ${vAcc.name} timers: ", vAcc)
    body += rawPrint("Mean mean total time for all automatic timers: ", accumulators.map(_._2 : IR_Expression).reduce(_ + _))

    val fct = IR_PlainFunction(name, IR_UnitDatatype, body)
    fct.allowFortranInterface = false
    fct
  }
}