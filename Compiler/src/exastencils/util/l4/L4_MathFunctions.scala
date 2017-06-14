package exastencils.util.l4

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.util.ir.IR_MathFunctionAccess

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

/// L4_MathFunctionAccess

case class L4_MathFunctionAccess(var name : String, var datatype : L4_Datatype) extends L4_PlainFunctionAccess {
  override def progress = IR_MathFunctionAccess(name, datatype.progress)
}

/// L4_ResolveMathFunctions

object L4_ResolveMathFunctions extends DefaultStrategy("Resolve math function accesses") {
  this += new Transformation("Resolve function accesses", {
    case L4_FunctionCall(L4_UnresolvedAccess("min", _, level, _, _, _), args) =>
      if (level.isDefined) Logger.warn(s"Found leveled min function with level ${ level.get }; level is ignored")
      L4_Minimum(args)

    case L4_FunctionCall(L4_UnresolvedAccess("max", level, _, _, _, _), args) =>
      if (level.isDefined) Logger.warn(s"Found leveled max function with level ${ level.get }; level is ignored")
      L4_Maximum(args)

    case access @ L4_UnresolvedAccess(accessName, _, level, _, _, _) if L4_MathFunctions.exists(accessName) =>
      if (level.isDefined) Logger.warn(s"Found leveled math function $accessName with level ${ level.get }; level is ignored")
      L4_MathFunctionAccess(accessName, L4_MathFunctions.getValue(accessName).get._2)
  })
}
