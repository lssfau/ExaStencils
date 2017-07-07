package exastencils.timing.l4

import scala.collection.immutable.HashMap

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.timing.ir.IR_TimerFunctionReference

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

/// L4_TimerFunctionReference

case class L4_TimerFunctionReference(var name : String, var returnType : L4_Datatype) extends L4_PlainFunctionReference {
  override def progress = IR_TimerFunctionReference(name, returnType.progress)
}

/// L4_ResolveTimerFunctions

object L4_ResolveTimerFunctions extends DefaultStrategy("Resolve timer function references") {
  this += new Transformation("Resolve", {
    case L4_UnresolvedFunctionReference(fctName, level) if L4_TimerFunctions.exists(fctName) =>
      if (level.isDefined) Logger.warn(s"Found leveled timing function ${ fctName } with level ${ level.get }; level is ignored")
      L4_TimerFunctionReference(fctName, L4_TimerFunctions.getDatatype(fctName))
  })

  this += new Transformation("Convert string constants in function call arguments", {
    case fctCall @ L4_FunctionCall(_ : L4_TimerFunctionReference, args) =>
      L4_ConvertStringConstantsToLiterals.applyStandalone(args)
      fctCall
  })
}

