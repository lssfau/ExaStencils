package exastencils.util.l4

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
  override def progress = IR_MathFunctionReference(name, returnType.progress)
}

/// L4_ResolveMathFunctions

object L4_ResolveMathFunctions extends DefaultStrategy("Resolve math function references") {
  this += new Transformation("Resolve", {
    case L4_FunctionCall(L4_UnresolvedFunctionReference("min", level, offset), args) =>
      if (level.isDefined) Logger.warn(s"Found leveled min function with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found offset access on min function; offset is ignored")
      L4_Minimum(args)

    case L4_FunctionCall(L4_UnresolvedFunctionReference("max", level, offset), args) =>
      if (level.isDefined) Logger.warn(s"Found leveled max function with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found offset access on max function; offset is ignored")
      L4_Maximum(args)

    case L4_UnresolvedFunctionReference(fctName, level, offset) if L4_MathFunctions.exists(fctName) =>
      if (level.isDefined) Logger.warn(s"Found leveled math function $fctName with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found offset access on math function $fctName; offset is ignored")
      L4_MathFunctionReference(fctName, L4_MathFunctions.getValue(fctName).get._2)
  })
}
