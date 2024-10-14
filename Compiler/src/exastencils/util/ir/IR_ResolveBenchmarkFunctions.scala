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

package exastencils.util.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_UnduplicatedVariable
import exastencils.config.Knowledge
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.logger.Logger
import exastencils.parallelization.api.omp.OMP_Parallel
import exastencils.timing.ir.IR_TimerFunctionReference
import exastencils.util.ir.DLB_Monitor.getMonitorName

/// IR_ResolveBenchmarkFunctions

object IR_ResolveBenchmarkFunctions extends DefaultStrategy("ResolveBenchmarkFunctions") {
  def startFunction = "benchmarkStart"
  def stopFunction = "benchmarkStop"

  private def handleArgs(args : ListBuffer[IR_Expression]) : ListBuffer[IR_Expression] = args.map {
    case strConst : IR_StringConstant => strConst
    case strLit : IR_StringLiteral    => IR_StringConstant(strLit.value)
    case arg                          => Logger.error("Unknown argument type for benchmark function: " + arg.prettyprint)
  }

  this += Transformation("Add markers for timers", {
    case stmt @ IR_ExpressionStatement(_ @ IR_FunctionCall(function : IR_TimerFunctionReference, args)) if List("startTimer", "stopTimer").contains(function.name) =>
      if (Knowledge.timer_addBenchmarkMarkers) {
        function.name match {
          case "startTimer" =>
            ListBuffer(
              stmt,
              IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference(startFunction, IR_UnitDatatype), args)))
          case "stopTimer"  =>
            ListBuffer(
              IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference(stopFunction, IR_UnitDatatype), args)),
              stmt)
        }
      } else {
        stmt
      }
  })

  private def likwidFunctionBuilder(functionCall : IR_FunctionCall, functionName : String) : IR_Statement = {
    val args = functionCall.arguments
    if (1 != args.length)
      Logger.error(s"$startFunction takes a single argument of type String for benchmark_backend 'likwid'")
    functionCall.arguments = handleArgs(args)

    // change function name
    functionCall.function.name = functionName
    val funcCall = IR_ExpressionStatement(functionCall)

    if (Knowledge.omp_enabled)
      OMP_Parallel(ListBuffer(funcCall))
    else
      funcCall
  }

  private def talpFunctionBuilder(functionCall : IR_FunctionCall, functionName : String) : IR_Statement = {
    val args = functionCall.arguments
    if (1 == args.length) {
      val str = args.head match {
        case strConst : IR_StringConstant => strConst.value
        case strLit : IR_StringLiteral    => strLit.value
        case arg                          => Logger.error("Unknown argument type for benchmark function: " + arg.prettyprint)
      }
      functionCall.arguments = ListBuffer(DLB_Monitor(str))

      functionCall.function.name = functionName
      val funcCall = IR_ExpressionStatement(functionCall)

      if (Knowledge.omp_enabled)
        OMP_Parallel(ListBuffer(funcCall))
      else
        funcCall
    } else {
      Logger.error(s"$startFunction takes a single argument of type String for benchmark_backend 'talp'")
      IR_NullStatement
    }
  }


  this += new Transformation("ResolveFunctionCalls", {
    case IR_ExpressionStatement(f @ IR_FunctionCall(IR_UnresolvedFunctionReference(s : String, _), args)) if s == startFunction =>
      Knowledge.benchmark_backend match {
        case "likwid" =>
          likwidFunctionBuilder(f, "LIKWID_MARKER_START")
        case "talp" =>
          talpFunctionBuilder(f, "DLB_MonitoringRegionStart")
        case _ => IR_NullStatement
      }

    case IR_ExpressionStatement(f @ IR_FunctionCall(IR_UnresolvedFunctionReference(s : String, _), args)) if s == stopFunction =>
      Knowledge.benchmark_backend match {
        case "likwid" =>
          likwidFunctionBuilder(f, "LIKWID_MARKER_STOP")
        case "talp" =>
          talpFunctionBuilder(f, "DLB_MonitoringRegionStop")
        case _ => IR_NullStatement
      }
  })
}


case class DLB_Monitor(name : String) extends IR_UnduplicatedVariable {

  override def resolveName() : String = getMonitorName(name)
  override def resolveDatatype() : IR_Datatype = IR_SpecialDatatype("dlb_monitor_t *")
  override def getCtor() : Option[IR_Statement] = None
}

object DLB_Monitor {
  def getMonitorName(name : String) : String = "monitor_" + name.replaceAll("[^a-zA-Z0-9]", "_")
}
