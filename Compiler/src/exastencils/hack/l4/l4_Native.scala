package exastencils.hack.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures._
import exastencils.hack.ir.HACK_IR_Native
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_Native

case class HACK_L4_Native(nativeCode : String) extends L4_Statement {
  override def prettyprint(out : PpStream) = out << "native ( " << nativeCode << " )"
  override def progress = HACK_IR_Native(nativeCode)
}

/// L4_ResolveNativeFunctions

object HACK_L4_ResolveNativeFunctions extends DefaultStrategy("Resolve native function accesses") {
  this += new Transformation("Resolve function accesses", {
    case L4_ExpressionStatement(L4_FunctionCall(L4_UnresolvedAccess("native", _, level, _, _, _), args)) =>
      if (level.isDefined) Logger.warn(s"Found leveled native function with level ${ level.get }; level is ignored")
      args match {
        case ListBuffer(code : L4_StringLiteral)  => HACK_L4_Native(code.value)
        case ListBuffer(code : L4_StringConstant) => HACK_L4_Native(code.value)
        case _                                    =>
          Logger.warn("Ignoring native function with unsupported arguments " + args)
          L4_NullStatement
      }
    case L4_FunctionCall(L4_UnresolvedAccess("native", _, _, _, _, _), args)                             =>
      Logger.warn("Ignoring native function call which is not a statement: " + args)
      L4_NullExpression
  })
}
