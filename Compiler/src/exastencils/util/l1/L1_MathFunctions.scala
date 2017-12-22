package exastencils.util.l1

import exastencils.base.ProgressLocation
import exastencils.base.l1._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.util.l2.L2_MathFunctionReference

/// L1_MathFunctions

object L1_MathFunctions {
  val signatures = Map(
    "exp" -> (List(L1_RealDatatype) -> L1_RealDatatype),
    "exp2" -> (List(L1_RealDatatype) -> L1_RealDatatype),
    "exp10" -> (List(L1_RealDatatype) -> L1_RealDatatype),
    "log" -> (List(L1_RealDatatype) -> L1_RealDatatype),
    "log10" -> (List(L1_RealDatatype) -> L1_RealDatatype),
    "ldexp" -> (List(L1_RealDatatype, L1_RealDatatype) -> L1_RealDatatype),

    "pow" -> (List(L1_RealDatatype, L1_RealDatatype) -> L1_RealDatatype),
    "sqrt" -> (List(L1_RealDatatype) -> L1_RealDatatype),

    "sin" -> (List(L1_RealDatatype) -> L1_RealDatatype),
    "cos" -> (List(L1_RealDatatype) -> L1_RealDatatype),
    "tan" -> (List(L1_RealDatatype) -> L1_RealDatatype),
    "asin" -> (List(L1_RealDatatype) -> L1_RealDatatype),
    "acos" -> (List(L1_RealDatatype) -> L1_RealDatatype),
    "atan" -> (List(L1_RealDatatype) -> L1_RealDatatype),
    "sinh" -> (List(L1_RealDatatype) -> L1_RealDatatype),
    "cosh" -> (List(L1_RealDatatype) -> L1_RealDatatype),
    "tanh" -> (List(L1_RealDatatype) -> L1_RealDatatype),
    "atan2" -> (List(L1_RealDatatype, L1_RealDatatype) -> L1_RealDatatype),

    "fabs" -> (List(L1_RealDatatype) -> L1_RealDatatype))

  def getValue(fctName : String) = signatures.get(fctName)
  def exists(fctName : String) = signatures.contains(fctName)
}

/// L1_MathFunctionReference

case class L1_MathFunctionReference(var name : String, var returnType : L1_Datatype) extends L1_PlainFunctionReference {
  override def progress = ProgressLocation(L2_MathFunctionReference(name, returnType.progress))
}

/// L1_ResolveMathFunctions

object L1_ResolveMathFunctions extends DefaultStrategy("Resolve math function references") {
  this += new Transformation("Resolve", {
    case L1_FunctionCall(L1_UnresolvedFunctionReference("min", level), args) =>
      if (level.isDefined) Logger.warn(s"Found leveled min function with level ${ level.get }; level is ignored")
      L1_Minimum(args)

    case L1_FunctionCall(L1_UnresolvedFunctionReference("max", level), args) =>
      if (level.isDefined) Logger.warn(s"Found leveled max function with level ${ level.get }; level is ignored")
      L1_Maximum(args)

    case L1_UnresolvedFunctionReference(fctName, level) if L1_MathFunctions.exists(fctName) =>
      if (level.isDefined) Logger.warn(s"Found leveled math function $fctName with level ${ level.get }; level is ignored")
      L1_MathFunctionReference(fctName, L1_MathFunctions.getValue(fctName).get._2)
  })
}
