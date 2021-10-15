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
import exastencils.prettyprinting.PpStream

object IR_MathFunctions {
  val signatures = Map(
    "exp" -> (List(IR_RealDatatype) -> IR_RealDatatype),
    "exp2" -> (List(IR_RealDatatype) -> IR_RealDatatype),
    "exp10" -> (List(IR_RealDatatype) -> IR_RealDatatype),
    "log" -> (List(IR_RealDatatype) -> IR_RealDatatype),
    "log10" -> (List(IR_RealDatatype) -> IR_RealDatatype),
    "ldexp" -> (List(IR_RealDatatype, IR_RealDatatype) -> IR_RealDatatype),

    "pow" -> (List(IR_RealDatatype, IR_RealDatatype) -> IR_RealDatatype),
    "sqrt" -> (List(IR_RealDatatype) -> IR_RealDatatype),

    "sin" -> (List(IR_RealDatatype) -> IR_RealDatatype),
    "cos" -> (List(IR_RealDatatype) -> IR_RealDatatype),
    "tan" -> (List(IR_RealDatatype) -> IR_RealDatatype),
    "asin" -> (List(IR_RealDatatype) -> IR_RealDatatype),
    "acos" -> (List(IR_RealDatatype) -> IR_RealDatatype),
    "atan" -> (List(IR_RealDatatype) -> IR_RealDatatype),
    "sinh" -> (List(IR_RealDatatype) -> IR_RealDatatype),
    "cosh" -> (List(IR_RealDatatype) -> IR_RealDatatype),
    "tanh" -> (List(IR_RealDatatype) -> IR_RealDatatype),
    "atan2" -> (List(IR_RealDatatype, IR_RealDatatype) -> IR_RealDatatype),

    "fabs" -> (List(IR_RealDatatype) -> IR_RealDatatype))

  def isEvaluable(args : IR_Number*) : Boolean = {
    args foreach (arg => arg.value match {
      case _ : Float | _ : Double =>
      case _                      =>
        return false
    })
    true
  }

  // TODO: find more generic math library
  def evaluateMathFunction(name : String, arg : Double) = name match {
    case "exp" => math.exp(arg)
    case "exp2" => math.pow(2.0, arg)
    case "exp10" => math.pow(10.0, arg)
    case "log" => math.log(arg)
    case "log10" => math.log10(arg)

    case "sqrt" => math.sqrt(arg)

    case "sin" => math.sin(arg)
    case "cos"  => math.cos(arg)
    case "tan" => math.tan(arg)
    case "asin" => math.asin(arg)
    case "acos" => math.acos(arg)
    case "atan" => math.atan(arg)
    case "sinh" => math.sinh(arg)
    case "cosh" => math.cosh(arg)
    case "tanh"  => math.tanh(arg)

    case "fabs" => math.abs(arg)
  }

  def evaluateMathFunction(name : String, arg1 : Double, arg2 : Double) = name match {
    case "ldexp" => arg1 * math.pow(2.0, arg2)
    case "pow" => math.pow(arg1, arg2)
    case "atan2" => math.atan2(arg1, arg2)
  }

  def getDatatype(fctName : String) = signatures(fctName)
  def exists(fctName : String) = signatures.contains(fctName)
}

/// IR_MathFunctionReference

object IR_MathFunctionReference {
  def fabs = new IR_MathFunctionReference("fabs", IR_MathFunctions.getDatatype("fabs")._2)
  def pow = new IR_MathFunctionReference("pow", IR_MathFunctions.getDatatype("pow")._2)
  def sqrt = new IR_MathFunctionReference("sqrt", IR_MathFunctions.getDatatype("sqrt")._2)
}

case class IR_MathFunctionReference(var name : String, var returnType : IR_Datatype) extends IR_FunctionReference {
  override def prettyprint(out : PpStream) = {
    name match {
      case "sqrt" => out << "std::sqrt"
      case "abs"  => out << "std::abs"
      case "fabs" => out << "std::fabs"
      case _      => super.prettyprint(out)
    }
  }
}
