package exastencils.util.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_MathFunctions extends Generatable {
  override def validLayers() = ListBuffer(L2, L3, L4, IR)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/util/|LAYER_LC|/|LAYER_UC|_MathFunctions.scala"

  override def generateForLayer(layer : Layer) = {
    val printer = new Printer
    printer <<< """package exastencils.util.|LAYER_LC|"""
    printer <<< """"""
    printer <<< """import exastencils.base.|LAYER_LC|._"""
    if (L2 == layer || L3 == layer) {
      printer <<< """import exastencils.baseExt.|LAYER_LC|.|LAYER_UC|_UnresolvedAccess"""
    }
    if (L4 == layer) {
      printer <<< """import exastencils.baseExt.|LAYER_LC|.|LAYER_UC|_UnresolvedAccess"""
    }
    printer <<< """import exastencils.datastructures._"""
    if (L2 == layer || L3 == layer) {
      printer <<< """import exastencils.logger.Logger"""
      printer <<< """import exastencils.util.|NEXT_LC|.|NEXT_UC|_MathFunctionAccess"""
    }
    if (L4 == layer) {
      printer <<< """import exastencils.logger.Logger"""
      printer <<< """import exastencils.util.|NEXT_LC|.|NEXT_UC|_MathFunctionAccess"""
    }
    printer <<< """"""
    if (L2 == layer || L3 == layer) {
      printer <<< """/// |LAYER_UC|_MathFunctions"""
      printer <<< """"""
    }
    if (L4 == layer) {
      printer <<< """/// |LAYER_UC|_MathFunctions"""
      printer <<< """"""
    }
    printer <<< """object |LAYER_UC|_MathFunctions {"""
    printer <<< """  val signatures = Map("""
    printer <<< """    "exp" -> (List(|LAYER_UC|_RealDatatype) -> |LAYER_UC|_RealDatatype),"""
    printer <<< """    "exp2" -> (List(|LAYER_UC|_RealDatatype) -> |LAYER_UC|_RealDatatype),"""
    printer <<< """    "exp10" -> (List(|LAYER_UC|_RealDatatype) -> |LAYER_UC|_RealDatatype),"""
    printer <<< """    "log" -> (List(|LAYER_UC|_RealDatatype) -> |LAYER_UC|_RealDatatype),"""
    printer <<< """    "log10" -> (List(|LAYER_UC|_RealDatatype) -> |LAYER_UC|_RealDatatype),"""
    printer <<< """    "ldexp" -> (List(|LAYER_UC|_RealDatatype, |LAYER_UC|_RealDatatype) -> |LAYER_UC|_RealDatatype),"""
    printer <<< """"""
    printer <<< """    "pow" -> (List(|LAYER_UC|_RealDatatype, |LAYER_UC|_RealDatatype) -> |LAYER_UC|_RealDatatype),"""
    printer <<< """    "sqrt" -> (List(|LAYER_UC|_RealDatatype) -> |LAYER_UC|_RealDatatype),"""
    printer <<< """"""
    printer <<< """    "sin" -> (List(|LAYER_UC|_RealDatatype) -> |LAYER_UC|_RealDatatype),"""
    printer <<< """    "cos" -> (List(|LAYER_UC|_RealDatatype) -> |LAYER_UC|_RealDatatype),"""
    printer <<< """    "tan" -> (List(|LAYER_UC|_RealDatatype) -> |LAYER_UC|_RealDatatype),"""
    printer <<< """    "asin" -> (List(|LAYER_UC|_RealDatatype) -> |LAYER_UC|_RealDatatype),"""
    printer <<< """    "acos" -> (List(|LAYER_UC|_RealDatatype) -> |LAYER_UC|_RealDatatype),"""
    printer <<< """    "atan" -> (List(|LAYER_UC|_RealDatatype) -> |LAYER_UC|_RealDatatype),"""
    printer <<< """    "sinh" -> (List(|LAYER_UC|_RealDatatype) -> |LAYER_UC|_RealDatatype),"""
    printer <<< """    "cosh" -> (List(|LAYER_UC|_RealDatatype) -> |LAYER_UC|_RealDatatype),"""
    printer <<< """    "tanh" -> (List(|LAYER_UC|_RealDatatype) -> |LAYER_UC|_RealDatatype),"""
    printer <<< """    "atan2" -> (List(|LAYER_UC|_RealDatatype, |LAYER_UC|_RealDatatype) -> |LAYER_UC|_RealDatatype),"""
    printer <<< """"""
    printer <<< """    "fabs" -> (List(|LAYER_UC|_RealDatatype) -> |LAYER_UC|_RealDatatype))"""
    printer <<< """"""
    printer <<< """  def getValue(fctName : String) = signatures.get(fctName)"""
    printer <<< """  def exists(fctName : String) = signatures.contains(fctName)"""
    printer <<< """}"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_MathFunctionAccess"""
    printer <<< """"""
    if (IR == layer) {
      printer <<< """case class |LAYER_UC|_MathFunctionAccess(var name : String, var datatype : |LAYER_UC|_Datatype) extends |LAYER_UC|_FunctionAccess"""
    }
    if (L2 == layer || L3 == layer || L4 == layer) {
      printer <<< """case class |LAYER_UC|_MathFunctionAccess(var name : String, var datatype : |LAYER_UC|_Datatype) extends |LAYER_UC|_FunctionAccess {"""
    }
    if (L4 == layer) {
      printer <<< """  override def level = None"""
    }
    if (L2 == layer || L3 == layer) {
      printer <<< """  override def progress = |NEXT_UC|_MathFunctionAccess(name, datatype.progress)"""
      printer <<< """}"""
    }
    if (L4 == layer) {
      printer <<< """  override def progress = |NEXT_UC|_MathFunctionAccess(name, datatype.progress)"""
      printer <<< """}"""
    }
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_ResolveMathFunctions"""
    printer <<< """"""
    printer <<< """object |LAYER_UC|_ResolveMathFunctions extends DefaultStrategy("Resolve math function accesses") {"""
    printer <<< """  this += new Transformation("Resolve function accesses", {"""
    if (IR == layer) {
      printer <<< """    case |LAYER_UC|_FunctionCall(|LAYER_UC|_UserFunctionAccess("min", _), args) =>"""
    }
    if (L2 == layer || L3 == layer) {
      printer <<< """    case |LAYER_UC|_FunctionCall(|LAYER_UC|_UnresolvedAccess("min", level), args) =>"""
    }
    if (L4 == layer) {
      printer <<< """    case |LAYER_UC|_FunctionCall(|LAYER_UC|_UnresolvedAccess("min", _, level, _, _, _), args) =>"""
    }
    if (L2 == layer || L3 == layer) {
      printer <<< """      if (level.isDefined) Logger.warn(s"Found leveled min function with level ${ level.get }; level is ignored")"""
    }
    if (L4 == layer) {
      printer <<< """      if (level.isDefined) Logger.warn(s"Found leveled min function with level ${ level.get }; level is ignored")"""
    }
    printer <<< """      |LAYER_UC|_Minimum(args)"""
    printer <<< """"""
    if (IR == layer) {
      printer <<< """    case |LAYER_UC|_FunctionCall(|LAYER_UC|_UserFunctionAccess("max", _), args) =>"""
    }
    if (L4 == layer) {
      printer <<< """    case |LAYER_UC|_FunctionCall(|LAYER_UC|_UnresolvedAccess("max", _, level, _, _, _), args) =>"""
    }
    if (L2 == layer || L3 == layer) {
      printer <<< """    case |LAYER_UC|_FunctionCall(|LAYER_UC|_UnresolvedAccess("max", level), args) =>"""
    }
    if (L2 == layer || L3 == layer) {
      printer <<< """      if (level.isDefined) Logger.warn(s"Found leveled max function with level ${ level.get }; level is ignored")"""
    }
    if (L4 == layer) {
      printer <<< """      if (level.isDefined) Logger.warn(s"Found leveled max function with level ${ level.get }; level is ignored")"""
    }
    printer <<< """      |LAYER_UC|_Maximum(args)"""
    printer <<< """"""
    if (L4 == layer) {
      printer <<< """    case access @ |LAYER_UC|_UnresolvedAccess(accessName, _, level, _, _, _) if |LAYER_UC|_MathFunctions.exists(accessName) =>"""
    }
    if (L2 == layer || L3 == layer) {
      printer <<< """    case access @ |LAYER_UC|_UnresolvedAccess(accessName, level) if |LAYER_UC|_MathFunctions.exists(accessName) =>"""
    }
    if (IR == layer) {
      printer <<< """    case |LAYER_UC|_UserFunctionAccess(accessName, _) if |LAYER_UC|_MathFunctions.exists(accessName) =>"""
    }
    if (L2 == layer || L3 == layer) {
      printer <<< """      if (level.isDefined) Logger.warn(s"Found leveled math function $accessName with level ${ level.get }; level is ignored")"""
    }
    if (L4 == layer) {
      printer <<< """      if (level.isDefined) Logger.warn(s"Found leveled math function $accessName with level ${ level.get }; level is ignored")"""
    }
    printer <<< """      |LAYER_UC|_MathFunctionAccess(accessName, |LAYER_UC|_MathFunctions.getValue(accessName).get._2)"""
    printer <<< """  })"""
    printer <<< """}"""
    printer.toString
  }
}
