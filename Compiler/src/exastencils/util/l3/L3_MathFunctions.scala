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

package exastencils.util.l3

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.util.l4.L4_MathFunctionReference

/// L3_MathFunctions

object L3_MathFunctions {
  val signatures = Map(
    "exp" -> (List(L3_RealDatatype) -> L3_RealDatatype),
    "exp2" -> (List(L3_RealDatatype) -> L3_RealDatatype),
    "exp10" -> (List(L3_RealDatatype) -> L3_RealDatatype),
    "log" -> (List(L3_RealDatatype) -> L3_RealDatatype),
    "log10" -> (List(L3_RealDatatype) -> L3_RealDatatype),
    "ldexp" -> (List(L3_RealDatatype, L3_RealDatatype) -> L3_RealDatatype),

    "pow" -> (List(L3_RealDatatype, L3_RealDatatype) -> L3_RealDatatype),
    "sqrt" -> (List(L3_RealDatatype) -> L3_RealDatatype),

    "sin" -> (List(L3_RealDatatype) -> L3_RealDatatype),
    "cos" -> (List(L3_RealDatatype) -> L3_RealDatatype),
    "tan" -> (List(L3_RealDatatype) -> L3_RealDatatype),
    "asin" -> (List(L3_RealDatatype) -> L3_RealDatatype),
    "acos" -> (List(L3_RealDatatype) -> L3_RealDatatype),
    "atan" -> (List(L3_RealDatatype) -> L3_RealDatatype),
    "sinh" -> (List(L3_RealDatatype) -> L3_RealDatatype),
    "cosh" -> (List(L3_RealDatatype) -> L3_RealDatatype),
    "tanh" -> (List(L3_RealDatatype) -> L3_RealDatatype),
    "atan2" -> (List(L3_RealDatatype, L3_RealDatatype) -> L3_RealDatatype),

    "fabs" -> (List(L3_RealDatatype) -> L3_RealDatatype))

  def getValue(fctName : String) = signatures.get(fctName)
  def getDatatype(fctName : String) = getValue(fctName).get._2
  def exists(fctName : String) = signatures.contains(fctName)
}

/// L3_MathFunctionReference

object L3_MathFunctionReference {
  def pow = new L3_MathFunctionReference("pow", L3_MathFunctions.getDatatype("pow"))
  def sqrt = new L3_MathFunctionReference("sqrt", L3_MathFunctions.getDatatype("sqrt"))

  def fabs = new L3_MathFunctionReference("fabs", L3_MathFunctions.getDatatype("fabs"))
}

case class L3_MathFunctionReference(var name : String, var returnType : L3_Datatype) extends L3_PlainFunctionReference {
  override def progress = ProgressLocation(L4_MathFunctionReference(name, returnType.progress))
}

/// L3_ResolveMathFunctions

object L3_ResolveMathFunctions extends DefaultStrategy("Resolve math function references") {
  this += new Transformation("Resolve", {
    case L3_FunctionCall(L3_UnresolvedFunctionReference(fname, level, offset), args) if "min" == fname || "fmin" == fname =>
      if (level.isDefined) Logger.warn(s"Found leveled min function with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found offset access on min function; offset is ignored")
      L3_Minimum(args)

    case L3_FunctionCall(L3_UnresolvedFunctionReference(fname, level, offset), args) if "max" == fname || "fmax" == fname =>
      if (level.isDefined) Logger.warn(s"Found leveled max function with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found offset access on max function; offset is ignored")
      L3_Maximum(args)

    case L3_UnresolvedFunctionReference(fctName, level, offset) if L3_MathFunctions.exists(fctName) =>
      if (level.isDefined) Logger.warn(s"Found leveled math function $fctName with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found offset access on math function $fctName; offset is ignored")
      L3_MathFunctionReference(fctName, L3_MathFunctions.getValue(fctName).get._2)
  })
}
