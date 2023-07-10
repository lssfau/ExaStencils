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

package exastencils.base.ir

import exastencils.datastructures.Transformation.Output
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.timing.ir._

/// IR_FutureFunction

trait IR_FutureFunction extends IR_FunctionLike with IR_Expandable {
  override def expand() : Output[IR_Function] = {
    val fct = generateFct()

    // carry over parameters
    fct.isHeaderOnly = isHeaderOnly
    fct.allowInlining = allowInlining
    fct.allowFortranInterface = allowFortranInterface
    fct.functionQualifiers = functionQualifiers

    fct
  }

  def generateFct() : IR_Function
}

/// IR_FutureFunctionWithTiming

trait IR_FutureFunctionWithTiming extends IR_FutureFunction {

  def automaticTimingCategory : IR_AutomaticFunctionTimingCategory.Access

  override def expand() : Output[IR_Function] = {
    var genFct = generateFct()

    if (IR_AutomaticFunctionTimingCategory.categoryEnabled(automaticTimingCategory)) {
      val timer = IR_IV_Timer(s"autoTime_${automaticTimingCategory.toString}_$name")

      timer.annotate(IR_AutomaticFunctionTimingCategory.ANNOT, automaticTimingCategory)

      genFct.body.prepend(IR_FunctionCall(IR_StartTimer().name, timer))
      genFct.body.append(IR_FunctionCall(IR_StopTimer().name, timer))
    }

    genFct
  }
}

/// IR_FuturePlainFunction

trait IR_FuturePlainFunction extends IR_FutureFunction {
  override def generateFct() : IR_PlainFunction
}

/// IR_FuturePlainFunctionWithTiming

trait IR_FuturePlainFunctionWithTiming extends IR_FutureFunctionWithTiming {
  override def generateFct() : IR_PlainFunction
}

/// IR_FutureLeveledFunction

trait IR_FutureLeveledFunction extends IR_FutureFunction {
  def level : Int
  override def generateFct() : IR_LeveledFunction
}

/// IR_FutureLeveledFunctionWithTiming

trait IR_FutureLeveledFunctionWithTiming extends IR_FutureFunctionWithTiming {
  def level : Int
  override def generateFct() : IR_LeveledFunction
}
