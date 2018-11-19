package exastencils.util.l2

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.util.l3.L3_MathFunctionReference

/// L2_MathFunctions

object L2_MathFunctions {
  val signatures = Map(
    "exp" -> (List(L2_RealDatatype) -> L2_RealDatatype),
    "exp2" -> (List(L2_RealDatatype) -> L2_RealDatatype),
    "exp10" -> (List(L2_RealDatatype) -> L2_RealDatatype),
    "log" -> (List(L2_RealDatatype) -> L2_RealDatatype),
    "log10" -> (List(L2_RealDatatype) -> L2_RealDatatype),
    "ldexp" -> (List(L2_RealDatatype, L2_RealDatatype) -> L2_RealDatatype),

    "pow" -> (List(L2_RealDatatype, L2_RealDatatype) -> L2_RealDatatype),
    "sqrt" -> (List(L2_RealDatatype) -> L2_RealDatatype),

    "sin" -> (List(L2_RealDatatype) -> L2_RealDatatype),
    "cos" -> (List(L2_RealDatatype) -> L2_RealDatatype),
    "tan" -> (List(L2_RealDatatype) -> L2_RealDatatype),
    "asin" -> (List(L2_RealDatatype) -> L2_RealDatatype),
    "acos" -> (List(L2_RealDatatype) -> L2_RealDatatype),
    "atan" -> (List(L2_RealDatatype) -> L2_RealDatatype),
    "sinh" -> (List(L2_RealDatatype) -> L2_RealDatatype),
    "cosh" -> (List(L2_RealDatatype) -> L2_RealDatatype),
    "tanh" -> (List(L2_RealDatatype) -> L2_RealDatatype),
    "atan2" -> (List(L2_RealDatatype, L2_RealDatatype) -> L2_RealDatatype),

    "fabs" -> (List(L2_RealDatatype) -> L2_RealDatatype))

  def getValue(fctName : String) = signatures.get(fctName)
  def exists(fctName : String) = signatures.contains(fctName)
}

/// L2_MathFunctionReference

case class L2_MathFunctionReference(var name : String, var returnType : L2_Datatype) extends L2_PlainFunctionReference {
  override def progress = ProgressLocation(L3_MathFunctionReference(name, returnType.progress))
}

/// L2_ResolveMathFunctions

object L2_ResolveMathFunctions extends DefaultStrategy("Resolve math function references") {
  this += new Transformation("Resolve", {
    case L2_FunctionCall(L2_UnresolvedFunctionReference(fname, level, offset), args) if "min" == fname || "fmin" == fname =>
      if (level.isDefined) Logger.warn(s"Found leveled min function with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found offset access on min function; offset is ignored")
      L2_Minimum(args)

    case L2_FunctionCall(L2_UnresolvedFunctionReference(fname, level, offset), args) if "max" == fname || "fmax" == fname =>
      if (level.isDefined) Logger.warn(s"Found leveled max function with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found offset access on max function; offset is ignored")
      L2_Maximum(args)

    case L2_UnresolvedFunctionReference(fctName, level, offset) if L2_MathFunctions.exists(fctName) =>
      if (level.isDefined) Logger.warn(s"Found leveled math function $fctName with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found offset access on math function $fctName; offset is ignored")
      L2_MathFunctionReference(fctName, L2_MathFunctions.getValue(fctName).get._2)
  })
}
