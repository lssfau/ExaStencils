package exastencils.util.l3

import exastencils.base.l3._
import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.util.l4.L4_MathFunctionAccess

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
  def exists(fctName : String) = signatures.contains(fctName)
}

/// L3_MathFunctionAccess

case class L3_MathFunctionAccess(var name : String, var datatype : L3_Datatype) extends L3_FunctionAccess {
  override def progress = L4_MathFunctionAccess(name, datatype.progress)
}

/// L3_ResolveMathFunctions

object L3_ResolveMathFunctions extends DefaultStrategy("Resolve math function accesses") {
  this += new Transformation("Resolve function accesses", {
    case L3_FunctionCall(L3_UnresolvedAccess("min", level, _, _, _, _), args) =>
      if (level.isDefined) Logger.warn(s"Found leveled min function with level ${ level.get }; level is ignored")
      L3_Minimum(args)

    case L3_FunctionCall(L3_UnresolvedAccess("max", level, _, _, _, _), args) =>
      if (level.isDefined) Logger.warn(s"Found leveled max function with level ${ level.get }; level is ignored")
      L3_Maximum(args)

    case access @ L3_UnresolvedAccess(accessName, level, _, _, _, _) if L3_MathFunctions.exists(accessName) =>
      if (level.isDefined) Logger.warn(s"Found leveled math function $accessName with level ${ level.get }; level is ignored")
      L3_MathFunctionAccess(accessName, L3_MathFunctions.getValue(accessName).get._2)
  })
}
