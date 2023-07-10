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

package exastencils.timing.l4

import scala.collection.immutable.HashMap

import exastencils.base.ProgressLocation
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
    "printAllTimersToFile" -> L4_UnitDatatype,
    "printAllAutomaticFunctionTimers" -> L4_UnitDatatype,
    "reduceTimers" -> L4_UnitDatatype
  )

  def getDatatype(fctName : String) = functions(fctName)
  def exists(fctName : String) = functions.contains(fctName)
}

/// L4_TimerFunctionReference

case class L4_TimerFunctionReference(var name : String, var returnType : L4_Datatype) extends L4_PlainFunctionReference {
  override def progress = ProgressLocation(IR_TimerFunctionReference(name, returnType.progress))
}

/// L4_ResolveTimerFunctions

object L4_ResolveTimerFunctions extends DefaultStrategy("Resolve timer function references") {
  this += new Transformation("Resolve", {
    case L4_UnresolvedFunctionReference(fctName, level, offset) if L4_TimerFunctions.exists(fctName) =>
      if (level.isDefined) Logger.warn(s"Found leveled timing function ${ fctName } with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found timing function ${ fctName } with offset; offset is ignored")
      L4_TimerFunctionReference(fctName, L4_TimerFunctions.getDatatype(fctName))
  })

  this += new Transformation("Convert string constants in function call arguments", {
    case fctCall @ L4_FunctionCall(_ : L4_TimerFunctionReference, args) =>
      L4_ConvertStringConstantsToLiterals.applyStandalone(args)
      fctCall
  })
}
