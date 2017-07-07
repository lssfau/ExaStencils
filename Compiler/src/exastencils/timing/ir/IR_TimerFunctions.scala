package exastencils.timing.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.datastructures._
import exastencils.logger.Logger

/// IR_TimerFunctionReference

case class IR_TimerFunctionReference(var name : String, var returnType : IR_Datatype) extends IR_FunctionReference

/// IR_ResolveTimerFunctions

object IR_AdaptTimerFunctions extends DefaultStrategy("Adapt function calls to timer functions") {
  this += new Transformation("Handle function references", {
    case fctCall @ IR_FunctionCall(function : IR_TimerFunctionReference, args) =>
      // map aliases
      fctCall.function.name = function.name match {
        case "getMeanFromTimer"  => "getMeanTime"
        case "getTotalFromTimer" => "getTotalTime"
        case other               => other
      }

      // adapt arguments
      function.name match {
        case "startTimer" | "stopTimer" | "getMeanTime" | "getTotalTime" =>
          // functions expecting exactly one timer
          if (args.length != 1) Logger.warn("Ignoring invalid number of parameters in " + function.name + " timer function: " + args)
          fctCall.arguments = ListBuffer[IR_Expression](IR_IV_Timer(args(0)))
        case "printAllTimers" | "printAllTimersToFile"                   =>
          // functions expecting no parameters
          if (args.nonEmpty) Logger.warn("Ignoring invalid number of parameters in " + function.name + " timer function: " + args)
          fctCall.arguments = ListBuffer()
      }

      fctCall
  })
}

