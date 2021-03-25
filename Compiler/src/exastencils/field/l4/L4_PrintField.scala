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
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.field.ir.IR_PrintField
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_PrintField

case class L4_PrintField(
    var filename : L4_Expression,
    var field : L4_FieldAccess,
    var ioInterface : L4_Expression,
    var includeGhostLayers : Boolean = false,
    var canonicalOrder : Boolean = false,
    var binaryOutput : Boolean = false,
    var separator : Option[L4_Expression] = None,
    var condition : Option[L4_Expression] = None,
    var dataset : Option[L4_Expression] = None,
    var mpiioRepresentation : Option[L4_StringConstant] = None) extends L4_Statement {

  override def prettyprint(out : PpStream) = {
    // TODO
    out << "print ( "
    out << filename << ", " << field
    if (condition.isDefined) out << ", " << condition.get
    out << " )"
  }

  override def progress = {
    val progField = field.progress
    ProgressLocation(IR_PrintField(
      filename.progress,
      progField.field,
      progField.slot,
      ioInterface.progress,
      includeGhostLayers,
      canonicalOrder,
      binaryOutput,
      separator.getOrElse(L4_StringConstant(" ")).progress,
      condition.getOrElse(L4_BooleanConstant(true)).progress,
      dataset.getOrElse(L4_NullExpression).progress,
      mpiioRepresentation.getOrElse(L4_StringConstant("native")).progress))
  }
}

/// L4_ResolvePrintFieldFunctions

object L4_ResolvePrintFieldFunctions extends DefaultStrategy("Resolve print field function references") {
  val basenameFunction = "printField"
  val optionsIO = ListBuffer("lock", "fpp", "mpiio", "hdf5", "nc", "sion")

  val knownFunctionNames = {
    var ret = ListBuffer(basenameFunction)
    ret = ret ++ optionsIO.map(opt => basenameFunction + "_" + opt)
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

      @deprecated
      val includeGhosts = checkOptionAndRemove("WithGhost") // new interface: passed as parameter
      @deprecated
      val onlyValues = checkOptionAndRemove("Values") // new interface: distinction between printField and writeField makes this flag obsolete
      @deprecated
      val binary = checkOptionAndRemove("Binary") // new interface: passed as parameter

      // new flags
      val ifaceSelection = L4_StringConstant(procFctNam.diff(basenameFunction + "_").replace("_", ""))

      // wrapper function for deprecated "onlyValues" flag (redundant since we have distinct Print/Write field functions)
      // determines if plain field values are written to file (WriteField) or field values including a visualization format (PrintField)
      @deprecated
      def wrapStmtCtorOnlyValues(fn : L4_Expression, fieldAcc : L4_FieldAccess, includeGhost : Boolean, useBin : Boolean, cond : Option[L4_Expression] = None) = {
        val sep = L4_StringConstant(if(Knowledge.experimental_generateParaviewFiles) "," else " ")
        if(onlyValues)
          L4_WriteField(fn, fieldAcc, ioInterface = L4_StringConstant("lock"), includeGhostLayers = includeGhosts, binaryOutput = useBin, condition = cond)
        else
          L4_PrintField(fn, fieldAcc, ioInterface = L4_StringConstant("lock"), includeGhostLayers = includeGhosts, binaryOutput = useBin, separator = Some(sep), condition = cond)
      }

      if (level.isDefined) Logger.warn(s"Found leveled print field function with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found print field function with offset; offset is ignored")

      ifaceSelection.value.toLowerCase match {
        // deprecated function calls (backwards compatibility)
        case s : String if s.isEmpty => args match {
          case ListBuffer(field : L4_FieldAccess)                      => // option 1: only field -> deduce name
            wrapStmtCtorOnlyValues(L4_StringConstant(field.target.name + ".txt"), field, includeGhosts, binary)
          case ListBuffer(filename, field : L4_FieldAccess)            => // option 2: filename and field
            wrapStmtCtorOnlyValues(filename, field, includeGhosts, binary)
          case ListBuffer(filename, field : L4_FieldAccess, condition) => // option 3: filename, field and condition
            wrapStmtCtorOnlyValues(filename, field, includeGhosts, binary, Some(condition))
          case _                                                       =>
            Logger.error("Ignoring call to " + fctName + " with unsupported arguments: " + args.mkString(", "))
        }
        // new function calls with explicit I/O interface selection in the function name.
        case "lock"  => args match {
          case ListBuffer(fn, field : L4_FieldAccess)                                                                         => // option 1: filename, field
            L4_PrintField(fn, field, ioInterface = ifaceSelection)
          case ListBuffer(fn, field : L4_FieldAccess, inclGhost : L4_BooleanConstant)                                         => // option 2: filename, field, inclGhost
            L4_PrintField(fn, field, ioInterface = ifaceSelection, includeGhostLayers = inclGhost.value)
          case ListBuffer(fn, field : L4_FieldAccess, inclGhost : L4_BooleanConstant, useBin : L4_BooleanConstant)            => // option 3: filename, field, inclGhost, useBin
            L4_PrintField(fn, field, ioInterface = ifaceSelection, includeGhostLayers = inclGhost.value, binaryOutput = useBin.value)
          case ListBuffer(fn, field : L4_FieldAccess, inclGhost : L4_BooleanConstant, useBin : L4_BooleanConstant, cond)      => // option 4: filename, field, inclGhost, useBin, cond
            L4_PrintField(fn, field, ioInterface = ifaceSelection, includeGhostLayers = inclGhost.value, binaryOutput = useBin.value, condition = Some(cond))
          case ListBuffer(fn, field : L4_FieldAccess, inclGhost : L4_BooleanConstant, useBin : L4_BooleanConstant, cond, sep) => // option 5: filename, field, inclGhost, useBin, cond, sep
            if (useBin.value) Logger.error("Invalid parameter combination in \"printField\": binaryMode = true and separator set.")
            L4_PrintField(fn, field, ioInterface = ifaceSelection, includeGhostLayers = inclGhost.value, binaryOutput = useBin.value, condition = Some(cond), separator = Some(sep))
          case _                                                                                                              =>
            Logger.error("Ignoring call to " + fctName + " with unsupported arguments: " + args.mkString(", "))
        }
        case "fpp"   => args match {
          case ListBuffer(fn, field : L4_FieldAccess)                              => // option 1: filename, field
            L4_PrintField(fn, field, ioInterface = ifaceSelection)
          case ListBuffer(fn, field : L4_FieldAccess, useBin : L4_BooleanConstant) => // option 2: filename, field, useBin
            L4_PrintField(fn, field, ioInterface = ifaceSelection, binaryOutput = useBin.value)
          case _                                                                   =>
            Logger.error("Ignoring call to " + fctName + " with unsupported arguments: " + args.mkString(", "))
        }
        case "mpiio" => args match {
          case ListBuffer(fn, field : L4_FieldAccess)                                      => // option 1: filename, field
            L4_PrintField(fn, field, ioInterface = ifaceSelection)
          case ListBuffer(fn, field : L4_FieldAccess, canonicalOrder : L4_BooleanConstant) => // option 2: filename, field, canonicalOrder
            L4_PrintField(fn, field, ioInterface = ifaceSelection, canonicalOrder = canonicalOrder.value)
          case _                                                                           =>
            Logger.error("Ignoring call to " + fctName + " with unsupported arguments: " + args.mkString(", "))
        }
        case "hdf5"  => args match {
          case ListBuffer(fn, field : L4_FieldAccess)                                      => // option 1: filename, field
            L4_PrintField(fn, field, ioInterface = ifaceSelection, dataset = Some(L4_StringConstant(field.name)))
          case ListBuffer(fn, field : L4_FieldAccess, canonicalOrder : L4_BooleanConstant) => // option 2: filename, field, canonicalOrder
            L4_PrintField(fn, field, ioInterface = ifaceSelection, canonicalOrder = canonicalOrder.value, dataset = Some(L4_StringConstant(field.name)))
          case _                                                                           =>
            Logger.error("Ignoring call to " + fctName + " with unsupported arguments: " + args.mkString(", "))
        }
        case "nc"    => args match {
          case ListBuffer(fn, field : L4_FieldAccess)                                      => // option 1: filename, field
            L4_PrintField(fn, field, ioInterface = ifaceSelection, dataset = Some(L4_StringConstant(field.name)))
          case ListBuffer(fn, field : L4_FieldAccess, canonicalOrder : L4_BooleanConstant) => // option 2: filename, field, canonicalOrder
            L4_PrintField(fn, field, ioInterface = ifaceSelection, canonicalOrder = canonicalOrder.value, dataset = Some(L4_StringConstant(field.name)))
          case _                                                                           =>
            Logger.error("Ignoring call to " + fctName + " with unsupported arguments: " + args.mkString(", "))
        }
        case "sion"  => args match {
          case ListBuffer(fn, field : L4_FieldAccess)                                       => // option 1: filename, field
            L4_WriteField(fn, field, ioInterface = ifaceSelection)
          case ListBuffer(fn, field : L4_FieldAccess, inclGhost : L4_BooleanConstant)       => // option 2: filename, field, inclGhost
            L4_WriteField(fn, field, ioInterface = ifaceSelection, includeGhostLayers = inclGhost.value)
          case ListBuffer(fn, field : L4_FieldAccess, inclGhost : L4_BooleanConstant, cond) => // option 3: filename, field, inclGhost, cond
            L4_WriteField(fn, field, ioInterface = ifaceSelection, includeGhostLayers = inclGhost.value, condition = Some(cond))
          case _                                                                            =>
            Logger.error("Ignoring call to " + fctName + " with unsupported arguments: " + args.mkString(", "))
        }
        case _       =>
          Logger.error("Ignoring call to " + fctName + " with unsupported I/O interface: " + ifaceSelection)
          L4_NullStatement
      }
  })
}
