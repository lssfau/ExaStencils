package exastencils.util.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.ir._

/// L4_BuildString

case class L4_BuildString(var stringName : L4_Expression, var toPrint : ListBuffer[L4_Expression]) extends L4_Statement {
  override def prettyprint(out : PpStream) = out << "buildString ( " << stringName << ", " <<< (toPrint, ", ") << " )"
  override def progress = IR_BuildString(stringName.progress, toPrint.map(_.progress))
}

/// L4_ResolveBuildStringFunctions

object L4_ResolveBuildStringFunctions extends DefaultStrategy("Resolve build string function references") {
  this += new Transformation("Resolve", {
    case L4_ExpressionStatement(L4_FunctionCall(L4_UnresolvedFunctionReference("buildString", level, offset), args)) =>
      if (level.isDefined) Logger.warn(s"Found leveled buildString function with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found buildString function with offset; offset is ignored")
      L4_BuildString(args(0), args.slice(1, args.size))
  })
}
