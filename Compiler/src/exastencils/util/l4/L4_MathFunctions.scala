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

package exastencils.util.l4

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.util.ir.IR_MathFunctionReference

/// L4_MathFunctions

object L4_MathFunctions {
  val signatures = Map(
    "exp" -> (List(L4_RealDatatype) -> L4_RealDatatype),
    "exp2" -> (List(L4_RealDatatype) -> L4_RealDatatype),
    "exp10" -> (List(L4_RealDatatype) -> L4_RealDatatype),
    "log" -> (List(L4_RealDatatype) -> L4_RealDatatype),
    "log10" -> (List(L4_RealDatatype) -> L4_RealDatatype),
    "ldexp" -> (List(L4_RealDatatype, L4_RealDatatype) -> L4_RealDatatype),

    "pow" -> (List(L4_RealDatatype, L4_RealDatatype) -> L4_RealDatatype),
    "sqrt" -> (List(L4_RealDatatype) -> L4_RealDatatype),

    "sin" -> (List(L4_RealDatatype) -> L4_RealDatatype),
    "cos" -> (List(L4_RealDatatype) -> L4_RealDatatype),
    "tan" -> (List(L4_RealDatatype) -> L4_RealDatatype),
    "asin" -> (List(L4_RealDatatype) -> L4_RealDatatype),
    "acos" -> (List(L4_RealDatatype) -> L4_RealDatatype),
    "atan" -> (List(L4_RealDatatype) -> L4_RealDatatype),
    "sinh" -> (List(L4_RealDatatype) -> L4_RealDatatype),
    "cosh" -> (List(L4_RealDatatype) -> L4_RealDatatype),
    "tanh" -> (List(L4_RealDatatype) -> L4_RealDatatype),
    "atan2" -> (List(L4_RealDatatype, L4_RealDatatype) -> L4_RealDatatype),

    "fabs" -> (List(L4_RealDatatype) -> L4_RealDatatype))

  def getValue(fctName : String) = signatures.get(fctName)
  def exists(fctName : String) = signatures.contains(fctName)
}

/// L4_MathFunctionReference

case class L4_MathFunctionReference(var name : String, var returnType : L4_Datatype) extends L4_PlainFunctionReference {
  override def progress = ProgressLocation(IR_MathFunctionReference(name, returnType.progress))
}

/// L4_ResolveMathFunctions

object L4_ResolveMathFunctions extends DefaultStrategy("Resolve math function references") {
  this += new Transformation("Resolve", {
    case L4_FunctionCall(L4_UnresolvedFunctionReference(fname, level, offset), args) if "min" == fname || "fmin" == fname =>
      if (level.isDefined) Logger.warn(s"Found leveled min function with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found offset access on min function; offset is ignored")
      L4_Minimum(args)

    case L4_FunctionCall(L4_UnresolvedFunctionReference(fname, level, offset), args) if "max" == fname || "fmax" == fname =>
      if (level.isDefined) Logger.warn(s"Found leveled max function with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found offset access on max function; offset is ignored")
      L4_Maximum(args)

    case L4_UnresolvedFunctionReference(fctName, level, offset) if L4_MathFunctions.exists(fctName) =>
      if (level.isDefined) Logger.warn(s"Found leveled math function $fctName with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found offset access on math function $fctName; offset is ignored")
      L4_MathFunctionReference(fctName, L4_MathFunctions.getValue(fctName).get._2)
  })
}
