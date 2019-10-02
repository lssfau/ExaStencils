package exastencils.base.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.ir.IR_Native
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_Native

case class L4_Native(nativeCode : String) extends L4_Expression {
  override def prettyprint(out : PpStream) = out << "native ( " << nativeCode << " )"
  override def progress = ProgressLocation(IR_Native(nativeCode))
}

/// L4_ResolveNativeFunctions

object L4_ResolveNativeFunctions extends DefaultStrategy("Resolve native function references") {
  this += new Transformation("Resolve", {
    case L4_FunctionCall(L4_UnresolvedFunctionReference("native", level, offset), args) =>
      if (level.isDefined) Logger.warn(s"Found leveled native function with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found native function with offset; offset is ignored")
      args match {
        case ListBuffer(code : L4_StringLiteral)  => L4_Native(code.value)
        case ListBuffer(code : L4_StringConstant) => L4_Native(code.value)
        case _                                    =>
          Logger.warn("Ignoring native function with unsupported arguments " + args)
          L4_NullStatement
      }
  })
}
