//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

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
    var basenameFile : L4_Expression,
    var field : L4_FieldAccess,
    var dataset : Option[L4_Expression] = None,
    var condition : Option[L4_Expression] = None,
    var includeGhostLayers : Boolean = false,
    var format : L4_Expression = L4_StringConstant("txt"),
    var outputSingleFile : Boolean = true,
    var useLocking : Boolean = true) extends L4_Statement {

  override def prettyprint(out : PpStream) = {
    out << "print ( "
    out << basenameFile << ", " << field
    if (condition.isDefined) out << ", " << condition.get
    out << " )"
  }

  override def progress = {
    val progField = field.progress
    ProgressLocation(IR_PrintField(
      basenameFile.progress,
      progField.field,
      progField.slot,
      dataset.getOrElse(L4_NullExpression).progress,
      condition.getOrElse(L4_BooleanConstant(true)).progress,
      includeGhostLayers,
      format.progress,
      outputSingleFile,
      useLocking))
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
      val binary = if (checkOptionAndRemove("Binary")) L4_StringConstant("bin") else L4_StringConstant("txt")

      // wrapper function to provide backwards compatibility.
      // determines if plain field values are written to file (WriteField) or field values including a visualization format (PrintField)
      def wrapStmtCtorOnlyValues(fn : L4_Expression, fieldAcc : L4_FieldAccess, cond : Option[L4_Expression], includeGhost : Boolean, useBin : L4_StringConstant) = {
        if(onlyValues)
          L4_WriteField(fn, fieldAcc, None, cond, includeGhost, useBin)
        else
          L4_PrintField(fn, fieldAcc, None, cond, includeGhost, useBin)
      }

      if (level.isDefined) Logger.warn(s"Found leveled print field function with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found print field function with offset; offset is ignored")

      args match {
        // deprecated function calls (backwards compatibility)
        case ListBuffer(field : L4_FieldAccess)                      => // option 1: only field -> deduce name
          wrapStmtCtorOnlyValues(L4_StringConstant(field.target.name), field, None, includeGhosts, binary)
        case ListBuffer(basename, field : L4_FieldAccess)            => // option 2: filename and field
          wrapStmtCtorOnlyValues(basename, field, None, includeGhosts, binary)
        case ListBuffer(basename, field : L4_FieldAccess, condition) => // option 3: filename, field and condition
          wrapStmtCtorOnlyValues(basename, field, Some(condition), includeGhosts, binary)
        // new function calls. "format" parameter has precedence over old parameters (e.g. binary)
        // TODO
        case ListBuffer(basename, field : L4_FieldAccess, fmt : L4_StringConstant) => // option 4: filename, field and format
          L4_PrintField(basename, field, includeGhostLayers = includeGhosts, format = fmt)
        case ListBuffer(basename, field : L4_FieldAccess, fmt : L4_StringConstant, singleFile : L4_BooleanConstant, useLock : L4_BooleanConstant) => // option 5: filename, field, format, single-shared-file and locking
          L4_PrintField(basename, field, includeGhostLayers = includeGhosts, format = fmt, outputSingleFile = singleFile.value, useLocking = useLock.value)
        case ListBuffer(fileName, field : L4_FieldAccess, dataset : L4_StringConstant, fmt : L4_StringConstant, singleFile : L4_BooleanConstant, useLock : L4_BooleanConstant) => // option 6: filename, field, dataset, format, single-shared-file and locking
          L4_PrintField(fileName, field, Some(dataset), includeGhostLayers = includeGhosts, format = fmt, outputSingleFile = singleFile.value, useLocking = useLock.value)
        // TODO: condition for locking/fpp
        case _                                                       =>
          Logger.warn("Ignoring call to printField with unsupported arguments: " + args.mkString(", "))
          L4_NullStatement
      }
  })
}
