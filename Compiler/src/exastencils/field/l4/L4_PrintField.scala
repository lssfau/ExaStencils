package exastencils.field.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.field.ir.IR_PrintField
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_PrintField

case class L4_PrintField(
    var filename : L4_Expression,
    var field : L4_FieldAccess,
    var condition : Option[L4_Expression] = None,
    var includeGhostLayers : Boolean = false,
    var onlyValues : Boolean = false,
    var binary : Boolean = false) extends L4_Statement {

  override def prettyprint(out : PpStream) = {
    out << "print ( "
    out << filename << ", " << field
    if (condition.isDefined) out << ", " << condition.get
    out << " )"
  }

  override def progress = {
    val progField = field.progress
    ProgressLocation(
      IR_PrintField(
        filename.progress,
        progField.field,
        progField.slot,
        condition.getOrElse(L4_BooleanConstant(true)).progress,
        includeGhostLayers,
        onlyValues,
        binary))
  }
}

/// L4_ResolvePrintFieldFunctions

object L4_ResolvePrintFieldFunctions extends DefaultStrategy("Resolve print field function references") {
  val knownFunctionNames = {
    var ret = ListBuffer("printField")
    ret = ret ++ ret.map(_ + "Binary")
    ret = ret ++ ret.map(_ + "Values")
    ret = ret ++ ret.map(_ + "WithGhost")

    ret
  }

  this += new Transformation("Resolve", {
    case L4_ExpressionStatement(L4_FunctionCall(L4_UnresolvedFunctionReference(fctName, level, offset), args)) if knownFunctionNames.contains(fctName) =>
      var procFctNam = fctName

      def checkOptionAndRemove(option : String) : Boolean = {
        if (procFctNam.endsWith(option)) {
          procFctNam = procFctNam.dropRight(option.length)
          return true
        }
        false
      }

      val includeGhosts = checkOptionAndRemove("WithGhost")
      val onlyValues = checkOptionAndRemove("Values")
      val binary = checkOptionAndRemove("Binary")

      if (level.isDefined) Logger.warn(s"Found leveled print field function with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found print field function with offset; offset is ignored")

      args match {
        case ListBuffer(field : L4_FieldAccess)                      => // option 1: only field -> deduce name
          L4_PrintField(L4_StringConstant(field.target.name + ".txt"), field, None, includeGhosts, onlyValues, binary)
        case ListBuffer(fileName, field : L4_FieldAccess)            => // option 2: filename and field
          L4_PrintField(fileName, field, None, includeGhosts, onlyValues, binary)
        case ListBuffer(fileName, field : L4_FieldAccess, condition) => // option 3: filename, file and condition
          L4_PrintField(fileName, field, Some(condition), includeGhosts, onlyValues, binary)
        case _                                                       =>
          Logger.warn("Ignoring call to printField with unsupported arguments: " + args.mkString(", "))
          L4_NullStatement
      }
  })
}
