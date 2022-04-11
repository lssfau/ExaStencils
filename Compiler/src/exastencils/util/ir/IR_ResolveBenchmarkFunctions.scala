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

import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.logger.Logger

/// IR_ResolveBenchmarkFunctions

object IR_ResolveBenchmarkFunctions extends DefaultStrategy("ResolveBenchmarkFunctions") {
  def startFunction = "benchmarkStart"
  def stopFunction = "benchmarkStop"

  this += new Transformation("ResolveFunctionCalls", {
    case IR_ExpressionStatement(f @ IR_FunctionCall(IR_UnresolvedFunctionReference(startFunction, _), args)) =>
      Knowledge.benchmark_backend match {
        case "likwid" =>
          if (1 != args.length || args.head.datatype != IR_StringDatatype)
            Logger.error(s"$startFunction takes a single argument of type String for benchmark_backend 'likwid'")

          f.function.name = "LIKWID_MARKER_START"
          IR_ExpressionStatement(f)

        case _ => IR_NullStatement
      }

    case IR_ExpressionStatement(f @ IR_FunctionCall(IR_UnresolvedFunctionReference(stopFunction, _), args)) =>
      Knowledge.benchmark_backend match {
        case "likwid" =>
          if (1 != args.length || args.head.datatype != IR_StringDatatype)
            Logger.error(s"$stopFunction takes a single argument of type String for benchmark_backend 'likwid'")

          f.function.name = "LIKWID_MARKER_STOP"
          IR_ExpressionStatement(f)

        case _ => IR_NullStatement
      }
  })
}
