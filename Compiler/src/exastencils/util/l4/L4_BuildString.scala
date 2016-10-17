package exastencils.util.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_UnresolvedAccess
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

object L4_ResolveBuildStringFunctions extends DefaultStrategy("Resolve build string function accesses") {
  this += new Transformation("Resolve function accesses", {
    case L4_ExpressionStatement(L4_FunctionCall(L4_UnresolvedAccess("buildString", _, level, _, _, _), args)) =>
      if (level.isDefined) Logger.warn(s"Found leveled buildString function with level ${ level.get }; level is ignored")
      L4_BuildString(args(0), args.slice(1, args.size))
  })
}
