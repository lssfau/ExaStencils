package exastencils.util.ir

import exastencils.base.ir._

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
}

case class IR_MathFunctionReference(var name : String, var returnType : IR_Datatype) extends IR_FunctionReference
