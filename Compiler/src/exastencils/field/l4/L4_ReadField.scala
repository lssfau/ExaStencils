package exastencils.field.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.field.ir.IR_ReadField
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_ReadField

case class L4_ReadField(var filename : L4_Expression, var field : L4_FieldAccess, var condition : Option[L4_Expression] = None, var includeGhostLayers : Boolean = false) extends L4_Statement {
  override def prettyprint(out : PpStream) = {
    if (includeGhostLayers)
      out << "readFieldWithGhost ( "
    else
      out << "readField ( "
    out << filename << ", " << field
    if (condition.isDefined) out << ", " << condition.get
    out << " )"
  }
  override def progress = ProgressLocation(IR_ReadField(filename.progress, field.progress.fieldSelection, condition.getOrElse(L4_BooleanConstant(true)).progress, includeGhostLayers))
}

/// L4_ResolveReadFieldFunctions

object L4_ResolveReadFieldFunctions extends DefaultStrategy("Resolve read field function references") {
  this += new Transformation("Resolve", {
    case L4_ExpressionStatement(L4_FunctionCall(L4_UnresolvedFunctionReference(fctName, level, offset), args))
      if "readFieldWithGhost" == fctName || "readField" == fctName =>
      if (level.isDefined) Logger.warn(s"Found leveled read field function with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found read field function with offset; offset is ignored")
      args match {
        case ListBuffer(field : L4_FieldAccess)                      => // option 1: only field -> deduce name
          L4_ReadField(L4_StringConstant(field.target.name + ".dat"), field, includeGhostLayers = "readFieldWithGhost" == fctName)
        case ListBuffer(fileName, field : L4_FieldAccess)            => // option 2: filename and field
          L4_ReadField(fileName, field, includeGhostLayers = "readFieldWithGhost" == fctName)
        case ListBuffer(fileName, field : L4_FieldAccess, condition) => // option 3: filename, file and condition
          L4_ReadField(fileName, field, Some(condition), "readFieldWithGhost" == fctName)
        case _                                                       =>
          Logger.warn("Ignoring call to readField with unsupported arguments: " + args.mkString(", "))
          L4_NullStatement
      }
  })
}

