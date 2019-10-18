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
