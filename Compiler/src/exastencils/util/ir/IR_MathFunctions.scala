package exastencils.util.ir

import exastencils.base.ir._
import exastencils.datastructures._

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

  def getValue(fctName : String) = signatures.get(fctName)
  def exists(fctName : String) = signatures.contains(fctName)
}

/// IR_MathFunctionAccess

case class IR_MathFunctionAccess(var name : String, var datatype : IR_Datatype) extends IR_FunctionAccess

/// IR_ResolveMathFunctions

object IR_ResolveMathFunctions extends DefaultStrategy("Resolve math function accesses") {
  this += new Transformation("Resolve function accesses", {
    case IR_FunctionCall(IR_UserFunctionAccess("min", _), args) =>
      IR_Minimum(args)

    case IR_FunctionCall(IR_UserFunctionAccess("max", _), args) =>
      IR_Maximum(args)

    case IR_UserFunctionAccess(accessName, _) if IR_MathFunctions.exists(accessName) =>
      IR_MathFunctionAccess(accessName, IR_MathFunctions.getValue(accessName).get._2)
  })
}
