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
import exastencils.field.ir.IR_WriteField
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_WriteField

case class L4_WriteField(
    var basenameFile : L4_Expression,
    var field : L4_FieldAccess,
    var condition: Option[L4_Expression] = None,
    var includeGhostLayers : Boolean = false,
    var format : L4_Expression = L4_StringConstant("ascii"),
    var outputSingleFile : Boolean = true,
    var useLocking : Boolean = true) extends L4_Statement {

  override def prettyprint(out : PpStream) = {
    out << "write ( "
    out << basenameFile << ", " << field
    if (condition.isDefined) out << ", " << condition.get
    out << " )"
  }

  override def progress = {
    val progField = field.progress
    ProgressLocation(IR_WriteField(
      basenameFile.progress,
      progField.field,
      progField.slot,
      condition.getOrElse(L4_BooleanConstant(true)).progress,
      includeGhostLayers,
      format.progress,
      outputSingleFile,
      useLocking))
  }
}

/// L4_ResolvePrintFieldFunctions

object L4_ResolveWriteFieldFunctions extends DefaultStrategy("Resolve write field function references") {
  val knownFunctionNames = {
    var ret = ListBuffer("writeField")
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

      if (level.isDefined) Logger.warn(s"Found leveled print field function with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found print field function with offset; offset is ignored")

      args match {
        case ListBuffer(field : L4_FieldAccess)                      => // option 1: only field -> deduce name
          L4_WriteField(L4_StringConstant(field.target.name), field, includeGhostLayers = includeGhosts)
        case ListBuffer(fileName, field : L4_FieldAccess)            => // option 2: filename and field
          L4_WriteField(fileName, field, includeGhostLayers = includeGhosts)
        case ListBuffer(fileName, field : L4_FieldAccess, condition) => // option 3: filename, file and condition
          L4_WriteField(fileName, field, Some(condition), includeGhostLayers = includeGhosts)
        case ListBuffer(fileName, field : L4_FieldAccess, fmt : L4_StringConstant) => // option 4: filename, file and format
          L4_WriteField(fileName, field, includeGhostLayers = includeGhosts, format = fmt)
        case ListBuffer(fileName, field : L4_FieldAccess, fmt : L4_StringConstant, singleFile : L4_BooleanConstant, useLock : L4_BooleanConstant) => // option 5: filename, file, format, single-shared-file and locking
          L4_WriteField(fileName, field, includeGhostLayers = includeGhosts, format = fmt, outputSingleFile = singleFile.value, useLocking = useLock.value)
        case _  =>
          Logger.warn("Ignoring call to writeField with unsupported arguments: " + args.mkString(", "))
          L4_NullStatement
      }
  })
}
