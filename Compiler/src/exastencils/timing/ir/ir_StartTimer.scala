package exastencils.timing.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation.Output
import exastencils.prettyprinting.PpStream
import exastencils.util._

/// IR_StartTimer

case class IR_StartTimer() extends IR_TimerFunction with IR_Expandable {

  import IR_TimerFunction._

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
  override def prettyprint_decl() : String = prettyprint
  override def name = "startTimer"

  override def expand() : Output[IR_Function] = {
    val statements = ListBuffer[IR_Statement](
      IR_IfCondition(IR_EqEqExpression(0, accessMember("numEntries")), ListBuffer(
        IR_AssignNowToTimer(accessMember("timerStarted")),
        IR_Assignment(accessMember("lastTimeMeasured"), IR_ZeroTimerValue()))),
      if (Knowledge.experimental_timerEnableCallStacks) "CallTracker::StartTimer(&stopWatch)" else "",
      IR_PreIncrementExpression(accessMember("numEntries")))

    val fct = IR_Function(IR_UnitDatatype, name, ListBuffer(IR_FunctionArgument("stopWatch", IR_SpecialDatatype("StopWatch&"))), statements)
    fct.allowFortranInterface = false
    fct
  }
}
