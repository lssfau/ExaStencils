package exastencils.timing.ir

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ArrayAccess
import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_DoubleDatatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringLiteral
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_StdVectorDatatype_VS
import exastencils.base.ir.IR_ImplicitConversion._

trait IR_TimerGatherer {
  val dataStructureName = "timerValues"

  def createTimerValueStorage(size : IR_Expression) : IR_VariableDeclaration = IR_VariableDeclaration(IR_StdVectorDatatype_VS(IR_DoubleDatatype, size), dataStructureName)
  def accessTimerValue(index: IR_Expression) : IR_ArrayAccess = IR_ArrayAccess(dataStructureName, index)
  def timerValuesPointer : String = dataStructureName + ".data()"


  def genDataCollect(timers : HashMap[(String, Option[Int]), IR_TimingIV], includeMeanTime : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    var it = 0
    for (timer <- timers.toList.sortBy(_._1)) {
      val (access, level) = timer._2 match {
        case plainTimer : IR_PlainTimingIV     =>
          (IR_StringLiteral(plainTimer.resolveName()), None)
        case leveledTimer : IR_LeveledTimingIV =>
          (leveledTimer.accessTimerAtLevel(), Some(leveledTimer.level))
      }
      statements += IR_Assignment(accessTimerValue(it), IR_FunctionCall(IR_TimerFunctionReference("getTotalTime", IR_DoubleDatatype, level), access))
      it += 1
      if (includeMeanTime) {
        statements += IR_Assignment(accessTimerValue(it), IR_FunctionCall(IR_TimerFunctionReference("getMeanTime", IR_DoubleDatatype, level), access))
        it += 1
      }
    }

    statements
  }
}
