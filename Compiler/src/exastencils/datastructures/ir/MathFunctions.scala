package exastencils.datastructures.ir

object MathFunctions {
  val signatures = Map(
    "exp" -> (List(RealDatatype) -> RealDatatype),
    "exp2" -> (List(RealDatatype) -> RealDatatype),
    "exp10" -> (List(RealDatatype) -> RealDatatype),
    "log" -> (List(RealDatatype) -> RealDatatype),
    "log10" -> (List(RealDatatype) -> RealDatatype),
    "ldexp" -> (List(RealDatatype, RealDatatype) -> RealDatatype),
    "pow" -> (List(RealDatatype, RealDatatype) -> RealDatatype),
    "sqrt" -> (List(RealDatatype) -> RealDatatype),
    "sin" -> (List(RealDatatype) -> RealDatatype),
    "cos" -> (List(RealDatatype) -> RealDatatype),
    "tan" -> (List(RealDatatype) -> RealDatatype),
    "asin" -> (List(RealDatatype) -> RealDatatype),
    "acos" -> (List(RealDatatype) -> RealDatatype),
    "atan" -> (List(RealDatatype) -> RealDatatype),
    "sinh" -> (List(RealDatatype) -> RealDatatype),
    "cosh" -> (List(RealDatatype) -> RealDatatype),
    "tanh" -> (List(RealDatatype) -> RealDatatype),
    "atan2" -> (List(RealDatatype, RealDatatype) -> RealDatatype))
}
