package exastencils.operator.l4

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

  def getDatatype(fctName : String) =/* FIXME */ L4_UnknownDatatype
  def exists(fctName : String) = functions.contains(fctName)
}

/// L4_StencilFunctionAccess

case class L4_StencilFunctionAccess(var name : String, var datatype : L4_Datatype) extends L4_PlainFunctionAccess {
  override def progress = IR_StencilFunctionAccess(name, datatype.progress)
}

/// L4_ResolveStencilFunctions

object L4_ResolveStencilFunctions extends DefaultStrategy("Resolve stencil function accesses") {
  this += new Transformation("Resolve function accesses", {
    case access : L4_UnresolvedAccess if L4_StencilFunctions.exists(access.name) =>
      if (access.level.isDefined) Logger.warn(s"Found leveled stencil function ${ access.name } with level ${ access.level.get }; level is ignored")
      L4_StencilFunctionAccess(access.name, L4_StencilFunctions.getDatatype(access.name))
  })
}
