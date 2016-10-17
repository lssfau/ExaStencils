package exastencils.stencil.l4

import scala.collection.mutable.HashSet

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.stencil.ir.IR_StencilFunctionAccess

/// L4_StencilFunctions

object L4_StencilFunctions {
  val functions = HashSet(
    "diag",
    "diag_inv", "diag_inverse")

  // TODO: datatype
  def getValue(fctName : String) = Some(L4_UnknownDatatype)
  def exists(fctName : String) = functions.contains(fctName)
}

/// L4_StencilFunctionAccess

object L4_StencilFunctionAccess {
  def apply(name : String, datatype : L4_Datatype) =
    new L4_StencilFunctionAccess(name, None, datatype)
  def apply(name : String, level : Int, datatype : L4_Datatype) =
    new L4_StencilFunctionAccess(name, Some(level), datatype)
}

case class L4_StencilFunctionAccess(var name : String, level : Option[Int], var datatype : L4_Datatype) extends L4_FunctionAccess {
  override def progress = IR_StencilFunctionAccess(resolvedName(), datatype.progress)
}

/// L4_ResolveStencilFunctions

object L4_ResolveStencilFunctions extends DefaultStrategy("Resolve stencil function accesses") {
  this += new Transformation("Resolve function accesses", {
    case access @ L4_UnresolvedAccess(accessName, _, level, _, _, _) if L4_StencilFunctions.exists(accessName) =>
      if (level.isDefined) Logger.warn(s"Found leveled stencil function $accessName with level ${ level.get }; level is ignored")
      L4_StencilFunctionAccess(accessName, L4_StencilFunctions.getValue(accessName).get)
  })
}
