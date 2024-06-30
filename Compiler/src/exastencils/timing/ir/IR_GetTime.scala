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

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._

/// IR_GetTotalTime

// in milliseconds
case class IR_GetTotalTime() extends IR_TimerFunction {

  import IR_TimerFunction._

  override var name = "getTotalTime"
  override def prettyprint_decl() : String = prettyprint

  override def generateFct() = {
    val body =
      IR_IfCondition(0 Neq accessMember("totalTimeAveraged"),
        IR_Return(Some(accessMember("totalTimeAveraged"))),
        IR_ReturnConvertToMS(accessMember("totalTimeMeasured")))

    val fct = IR_PlainFunction(name, IR_DoubleDatatype, ListBuffer(IR_FunctionArgument("stopWatch", IR_SpecialDatatype("StopWatch&"))), body)
    fct.allowFortranInterface = false
    fct
  }
}

/// IR_GetMeanTime

// in milliseconds
case class IR_GetMeanTime() extends IR_TimerFunction {

  import IR_TimerFunction._

  override var name = "getMeanTime"
  override def prettyprint_decl() : String = prettyprint

  override def generateFct() = {
    val body =
      IR_Return(IR_TernaryCondition(
        0 EqEq accessMember("numMeasurements"),
        0.0,
        IR_FunctionCall(IR_TimerFunctionReference("getTotalTime", IR_DoubleDatatype, None), "stopWatch") / accessMember("numMeasurements"))) // todo "None" should not work here

    val fct = IR_PlainFunction(name, IR_DoubleDatatype, ListBuffer(IR_FunctionArgument("stopWatch", IR_SpecialDatatype("StopWatch&"))), body)
    fct.allowFortranInterface = false
    fct
  }
}

/// IR_GetLastTime

// in milliseconds
case class IR_GetLastTime() extends IR_TimerFunction {

  import IR_TimerFunction._

  override var name = "getLastTime"
  override def prettyprint_decl() : String = prettyprint

  override def generateFct() = {
    val body = IR_ReturnConvertToMS(accessMember("lastTimeMeasured"))

    val fct = IR_PlainFunction(name, IR_DoubleDatatype, ListBuffer(IR_FunctionArgument("stopWatch", IR_SpecialDatatype("StopWatch&"))), body)
    fct.allowFortranInterface = false
    fct
  }
}
