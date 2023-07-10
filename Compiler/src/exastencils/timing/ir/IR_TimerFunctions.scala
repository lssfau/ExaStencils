//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

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
        case "printAllTimers" | "printAllTimersToFile" | "printAllAutomaticFunctionTimers" | "reduceTimers"  =>
          // functions expecting no parameters
          if (args.nonEmpty) Logger.warn("Ignoring invalid number of parameters in " + function.name + " timer function: " + args)
          fctCall.arguments = ListBuffer()
      }

      fctCall
  })
}
