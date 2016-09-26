package exastencils.timing.l4

import scala.collection.immutable.HashMap

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures._
import exastencils.logger.Logger

// L4_TimerFunctions

object L4_TimerFunctions {
  // TODO: add/ check call parameters?
  val functions = HashMap[String, L4_Datatype](
    "startTimer" -> L4_UnitDatatype,
    "stopTimer" -> L4_UnitDatatype,
    "getTotalTime" -> L4_DoubleDatatype,
    "getMeanTime" -> L4_DoubleDatatype,
    "getLastTime" -> L4_DoubleDatatype,
    "printAllTimers" -> L4_UnitDatatype,
    "printAllTimersToFile" -> L4_UnitDatatype
  )

  def getValue(fctName : String) = functions.get(fctName)
  def exists(fctName : String) = functions.contains(fctName)
}

/// L4_TimerFunctionAccess

object L4_TimerFunctionAccess {
  def apply(name : String, datatype : L4_Datatype) =
    new L4_TimerFunctionAccess(name, None, datatype)
  def apply(name : String, level : Int, datatype : L4_Datatype) =
    new L4_TimerFunctionAccess(name, Some(level), datatype)
}

case class L4_TimerFunctionAccess(var name : String, level : Option[Int], var datatype : L4_Datatype) extends L4_FunctionAccess

/// L4_ResolveTimerFunctions

object L4_ResolveTimerFunctions extends DefaultStrategy("Resolve timer function accesses") {
  this += new Transformation("Resolve function accesses", {
    case access @ L4_UnresolvedAccess(accessName, _, level, _, _, _) if L4_TimerFunctions.exists(accessName) =>
      if (level.isDefined)
        Logger.warn(s"Found leveled timing function $accessName with level ${ level.get }; level is ignored")
      L4_TimerFunctionAccess(accessName, L4_TimerFunctions.getValue(accessName).get)
  })

  this += new Transformation("Convert string constants in function call arguments", {
    case fctCall @ L4_FunctionCall(_ : L4_TimerFunctionAccess, args) =>
      L4_ConvertStringConstantsToLiterals.applyStandalone(args)
      fctCall
  })
}

