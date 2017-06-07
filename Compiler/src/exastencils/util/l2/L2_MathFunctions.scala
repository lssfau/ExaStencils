package exastencils.util.l2

import exastencils.base.l2._
import exastencils.baseExt.l2.L2_UnresolvedAccess
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.util.l3.L3_MathFunctionAccess

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

/// L2_MathFunctionAccess

case class L2_MathFunctionAccess(var name : String, var datatype : L2_Datatype) extends L2_FunctionAccess {
  override def progress = L3_MathFunctionAccess(name, datatype.progress)
}

/// L2_ResolveMathFunctions

object L2_ResolveMathFunctions extends DefaultStrategy("Resolve math function accesses") {
  this += new Transformation("Resolve function accesses", {
    case L2_FunctionCall(L2_UnresolvedAccess("min", level, _, _, _, _), args) =>
      if (level.isDefined) Logger.warn(s"Found leveled min function with level ${ level.get }; level is ignored")
      L2_Minimum(args)

    case L2_FunctionCall(L2_UnresolvedAccess("max", level, _, _, _, _), args) =>
      if (level.isDefined) Logger.warn(s"Found leveled max function with level ${ level.get }; level is ignored")
      L2_Maximum(args)

    case access @ L2_UnresolvedAccess(accessName, level, _, _, _, _) if L2_MathFunctions.exists(accessName) =>
      if (level.isDefined) Logger.warn(s"Found leveled math function $accessName with level ${ level.get }; level is ignored")
      L2_MathFunctionAccess(accessName, L2_MathFunctions.getValue(accessName).get._2)
  })
}
