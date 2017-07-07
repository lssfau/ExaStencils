package exastencils.util.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_RawPrint

/// L4_Print

case class L4_Print(var toPrint : ListBuffer[L4_Expression]) extends L4_Statement {
  override def prettyprint(out : PpStream) = out << "print ( " <<< (toPrint, ", ") << " )"
  override def progress = IR_RawPrint(toPrint.map(_.progress))
}

/// L4_ResolvePrintFunctions

object L4_ResolvePrintFunctions extends DefaultStrategy("Resolve print function references") {
  this += new Transformation("Resolve", {
    case L4_ExpressionStatement(L4_FunctionCall(L4_UnresolvedFunctionReference("print", level), args)) =>
      if (level.isDefined) Logger.warn(s"Found leveled print function with level ${ level.get }; level is ignored")
      L4_Print(args)
  })
}
