package exastencils.field.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.field.ir.IR_PrintField
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_PrintField

case class L4_PrintField(var filename : L4_Expression, var field : L4_FieldAccess, var condition : Option[L4_Expression] = None) extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    out << "print ( "
    out << filename << ", " << field
    if (condition.isDefined) out << ", " << condition.get
    out << " )"
  }
  override def progress = ProgressLocation(IR_PrintField(filename.progress, field.progress.fieldSelection, condition.getOrElse(L4_BooleanConstant(true)).progress))
}

/// L4_ResolvePrintFieldFunctions

object L4_ResolvePrintFieldFunctions extends DefaultStrategy("Resolve print field function references") {
  this += new Transformation("Resolve", {
    case L4_ExpressionStatement(L4_FunctionCall(L4_UnresolvedFunctionReference("printField", level, offset), args)) =>
      if (level.isDefined) Logger.warn(s"Found leveled print field function with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found print field function with offset; offset is ignored")
      args match {
        case ListBuffer(field : L4_FieldAccess)                      => // option 1: only field -> deduce name
          L4_PrintField(L4_StringConstant(field.target.name + ".dat"), field)
        case ListBuffer(fileName, field : L4_FieldAccess)            => // option 2: filename and field
          L4_PrintField(fileName, field)
        case ListBuffer(fileName, field : L4_FieldAccess, condition) => // option 3: filename, file and condition
          L4_PrintField(fileName, field, Some(condition))
        case _                                                       =>
          Logger.warn("Ignoring call to printField with unsupported arguments: " + args.mkString(", "))
          L4_NullStatement
      }
  })
}
