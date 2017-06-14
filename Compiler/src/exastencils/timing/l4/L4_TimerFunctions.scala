package exastencils.timing.l4

import scala.collection.immutable.HashMap

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.timing.ir.IR_TimerFunctionAccess

// L4_TimerFunctions

object L4_TimerFunctions {
  // TODO: add/ check call parameters?
  val functions = HashMap[String, L4_Datatype](
    "startTimer" -> L4_UnitDatatype,
    "stopTimer" -> L4_UnitDatatype,
    "getTotalFromTimer" -> L4_DoubleDatatype,
    "getTotalTime" -> L4_DoubleDatatype,
    "getMeanFromTimer" -> L4_DoubleDatatype,
    "getMeanTime" -> L4_DoubleDatatype,
    "getLastFromTimer" -> L4_DoubleDatatype,
    "getLastTime" -> L4_DoubleDatatype,
    "printAllTimers" -> L4_UnitDatatype,
    "printAllTimersToFile" -> L4_UnitDatatype
  )

  def getDatatype(fctName : String) = functions(fctName)
  def exists(fctName : String) = functions.contains(fctName)
}

/// L4_TimerFunctionAccess

case class L4_TimerFunctionAccess(var name : String, var datatype : L4_Datatype) extends L4_PlainFunctionAccess {
  override def progress = IR_TimerFunctionAccess(name, datatype.progress)
}

/// L4_ResolveTimerFunctions

object L4_ResolveTimerFunctions extends DefaultStrategy("Resolve timer function accesses") {
  this += new Transformation("Resolve function accesses", {
    case access : L4_UnresolvedAccess if L4_TimerFunctions.exists(access.name) =>
      if (access.level.isDefined) Logger.warn(s"Found leveled timing function ${ access.name } with level ${ access.level.get }; level is ignored")
      L4_TimerFunctionAccess(access.name, L4_TimerFunctions.getDatatype(access.name))
  })

  this += new Transformation("Convert string constants in function call arguments", {
    case fctCall @ L4_FunctionCall(_ : L4_TimerFunctionAccess, args) =>
      L4_ConvertStringConstantsToLiterals.applyStandalone(args)
      fctCall
  })
}

