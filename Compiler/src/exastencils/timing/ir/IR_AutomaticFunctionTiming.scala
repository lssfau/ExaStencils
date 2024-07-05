package exastencils.timing.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_InternalVariable
import exastencils.config.Knowledge
import exastencils.core.StateManager
import exastencils.parallelization.api.mpi._
import exastencils.util.ir.IR_RawPrint

/// IR_AutomaticTimingCategory

object IR_AutomaticTimingCategory extends Enumeration {
  type Access = Value
  final val ANNOT : String = "TimingCategory"
  final val COMM, APPLYBC, IO = Value

  def categoryEnabled(category : Access) = {
    if (Knowledge.timer_automaticTiming) {
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

/// IR_HasAutomaticTimingCategory

trait IR_HasAutomaticTimingCategory {
  def timingCategory : IR_AutomaticTimingCategory.Access
}

/// IR_IV_AutomaticTimer

case class IR_IV_AutomaticTimer(
    var name : String,
    var timingCategory : IR_AutomaticTimingCategory.Access
) extends IR_PlainTimingIV with IR_HasAutomaticTimingCategory

case class IR_IV_AutomaticLeveledTimer(
    var name : String,
    var timingCategory : IR_AutomaticTimingCategory.Access,
    var level : Int
) extends IR_InternalVariable(false, false, false, true, false) with IR_LeveledTimingIV with IR_HasAutomaticTimingCategory
/// IR_PrintAllAutomaticTimers

case class IR_PrintAllAutomaticTimers() extends IR_TimerFunction {
  import IR_AutomaticTimingCategory._

  override var name = "printAllAutomaticTimers"
  override def prettyprint_decl() : String = prettyprint

  private val accumulators : Map[(Access, Option[Int]), IR_VariableAccess] = values
    // create a variable for each category and level pair and adds it to a list
    .flatMap(enum =>
      if (enum != IR_AutomaticTimingCategory.IO) {
        for (n <- Range.inclusive(Knowledge.minLevel, Knowledge.maxLevel))
          yield (enum, Some(n)) -> IR_VariableAccess(s"accum_${enum.toString}_$n", IR_DoubleDatatype) : ((Access, Option[Int]), IR_VariableAccess)
      } else {
        Seq.empty
      }
    )
    // add one more variable for IO
    .+((IR_AutomaticTimingCategory.IO, None) -> IR_VariableAccess(s"accum_${IR_AutomaticTimingCategory.IO.toString}", IR_DoubleDatatype))
    .toMap


  def genPrintTimerCode(automaticTimer : IR_HasAutomaticTimingCategory) : IR_Statement = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    val timeToPrint = "getTotalTime"
    def timerValue = IR_VariableAccess("timerValue", IR_DoubleDatatype)

    if (categoryEnabled(automaticTimer.timingCategory)) {
      automaticTimer match {
        case automaticPlainTimer : IR_IV_AutomaticTimer =>
          def accum = accumulators((automaticTimer.timingCategory, None))
          statements += IR_VariableDeclaration(timerValue, IR_FunctionCall(IR_TimerFunctionReference(timeToPrint, IR_DoubleDatatype, None), automaticPlainTimer.resolveName()))

          if (Knowledge.mpi_enabled) {
            statements += MPI_AllReduce(IR_AddressOf(timerValue), timerValue.datatype, 1, "+")
            statements += IR_Assignment(timerValue, MPI_IV_MpiSize, "/=")
          }

          statements += IR_RawPrint("\"Mean mean total time for Timer " + automaticPlainTimer.name + ":\"", timerValue)
          statements += IR_Assignment(accum, timerValue, "+=")

        case automaticLeveledTimer @ IR_IV_AutomaticLeveledTimer(name, timingCategory, level) =>
          def accum = accumulators((timingCategory, Some(level)))
          statements += IR_VariableDeclaration(timerValue, IR_FunctionCall(IR_TimerFunctionReference(timeToPrint, IR_DoubleDatatype, Option(level)), automaticLeveledTimer.accessTimerAtLevel()))

          if (Knowledge.mpi_enabled) {
            statements += MPI_AllReduce(IR_AddressOf(timerValue), timerValue.datatype, 1, "+")
            statements += IR_Assignment(timerValue, MPI_IV_MpiSize, "/=")
          }

          statements += IR_RawPrint("\"Mean mean total time for Timer " + name + ":\"", timerValue)
          statements += IR_Assignment(accum, timerValue, "+=")
      }
    }
    IR_Scope(statements)
  }

  override def generateFct() = {
    IR_CollectTimers.applyStandalone(StateManager.root)
    val timers = IR_CollectTimers.timers
    val sortedAccumulators = accumulators.toList.sortBy(_._1)

    val body = ListBuffer[IR_Statement]()
    body ++= sortedAccumulators.map { case (_, vAcc) => IR_VariableDeclaration(vAcc, 0.0) }
    timers.values.foreach {
      case timer : IR_HasAutomaticTimingCategory =>
        body += genPrintTimerCode(timer)
      case _                            =>
    }

    // print out mean total timers for automatic function timers
    def rawPrint(msg : String, expr : IR_Expression) = IR_RawPrint(ListBuffer[IR_Expression](IR_StringConstant(msg), expr))
    for (((category, optionalLevel), vAcc) <- sortedAccumulators) {
      if (categoryEnabled(category)) {
        val levelDescription = if (optionalLevel.isDefined) s" at level ${optionalLevel.get}" else ""
        body += rawPrint(s"Mean mean total time for automatic timer ${vAcc.name}${levelDescription}:", vAcc)
      }
    }
    if (Knowledge.timer_automaticCommTiming) {
      body += rawPrint("Mean mean total time for all automatic communication timers:",
        sortedAccumulators
          .filter(elem => elem._1._1 == IR_AutomaticTimingCategory.COMM)
          .map(_._2 : IR_Expression).reduce(_ + _))
    }
    if (Knowledge.timer_automaticBCsTiming) {
      body += rawPrint("Mean mean total time for all automatic bound checking timers:",
        sortedAccumulators
          .filter(elem => elem._1._1 == IR_AutomaticTimingCategory.APPLYBC)
          .map(_._2 : IR_Expression).reduce(_ + _))
    }
    body += rawPrint("Mean mean total time for all automatic timers:", sortedAccumulators.map(_._2 : IR_Expression).reduce(_ + _))

    val fct = IR_PlainFunction(name, IR_UnitDatatype, body)
    fct.allowFortranInterface = false
    fct
  }
}