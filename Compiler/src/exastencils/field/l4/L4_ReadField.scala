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
import exastencils.field.ir.IR_ReadField
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_ReadField

case class L4_ReadField(
    var filename : L4_Expression,
    var field : L4_FieldAccess,
    var ioInterface : L4_Expression,
    var includeGhostLayers : Boolean = false,
    var canonicalOrder : Boolean = false,
    var binaryInput : Boolean = false,
    var separator : Option[L4_Expression] = None,
    var condition : Option[L4_Expression] = None,
    var dataset : Option[L4_Expression] = None,
    var mpiioRepresentation : Option[L4_StringConstant] = None) extends L4_Statement {

  override def prettyprint(out : PpStream) = {
    ioInterface match {
      case _ @ L4_StringConstant(str) if str == "lock" || str == "fpp" =>
        out << s"readField_$str ( " << filename << ", " << field << ", " << includeGhostLayers << ", " << binaryInput
        if (condition.isDefined) out << ", " << condition.get
        if (separator.isDefined) out << ", " << separator.get
        out << " )"
      case _ @ L4_StringConstant("mpiio") =>
        out << "readField_mpiio ( " << filename << ", " << field << ", " << includeGhostLayers << ", " << canonicalOrder
        if (mpiioRepresentation.isDefined) out << ", " << mpiioRepresentation.get
        out << " )"
      case _ @ L4_StringConstant(str) if str == "hdf5" || str == "nc" =>
        out << s"readField_$str ( " << filename << ", " << dataset.get << ", " << field << ", " << includeGhostLayers << ", " << canonicalOrder << " )"
      case _ @ L4_StringConstant("sion") =>
        out << "readField_sion ( " << filename << ", " << field << ", " << includeGhostLayers
        if (condition.isDefined) out << ", " << condition.get
        out << " )"
    }
  }
  override def progress = {
    val progField = field.progress
    ProgressLocation(IR_ReadField(
      filename.progress,
      progField.field,
      progField.slot,
      ioInterface.progress,
      includeGhostLayers,
      canonicalOrder,
      binaryInput,
      separator.getOrElse(L4_StringConstant(" ")).progress,
      condition.getOrElse(L4_BooleanConstant(true)).progress,
      dataset.getOrElse(L4_NullExpression).progress,
      mpiioRepresentation.getOrElse(L4_StringConstant("native")).progress))
  }
}

/// L4_ResolveReadFieldFunctions

object L4_ResolveReadFieldFunctions extends DefaultStrategy("Resolve read field function references") {
  val basenameFunction = "readField"
  val optionsIO = ListBuffer("lock", "fpp", "mpiio", "hdf5", "nc", "sion")

  val knownFunctionNames = {
    var ret = ListBuffer(basenameFunction)
    ret = ret ++ optionsIO.map(opt => basenameFunction + "_" + opt)
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
      val includeGhosts = checkOptionAndRemove("WithGhost")

      // new flags
      val ifaceSelection = L4_StringConstant(procFctNam.diff(basenameFunction + "_").replace("_", ""))

      if (level.isDefined) Logger.warn(s"Found leveled read field function with level ${ level.get }; level is ignored")
      if (offset.isDefined) Logger.warn(s"Found read field function with offset; offset is ignored")

      ifaceSelection.value.toLowerCase match {
        // deprecated function calls (backwards compatibility)
        case s : String if s.isEmpty => args match {
          case ListBuffer(field : L4_FieldAccess)                      => // option 1: only field -> deduce name
            L4_ReadField(L4_StringConstant(field.target.name + ".txt"), field, ioInterface = L4_StringConstant("fpp"), includeGhostLayers = includeGhosts)
          case ListBuffer(filename, field : L4_FieldAccess)            => // option 2: filename and field
            L4_ReadField(filename, field, ioInterface = L4_StringConstant("fpp"), includeGhostLayers = includeGhosts)
          case ListBuffer(filename, field : L4_FieldAccess, condition) => // option 3: filename, field and condition
            L4_ReadField(filename, field, ioInterface = L4_StringConstant("fpp"), includeGhostLayers = includeGhosts, condition = Some(condition))
          case _ =>
            Logger.error("Ignoring call to " + fctName + " with unsupported arguments: " + args.mkString(", "))
        }
        // new function calls with explicit I/O interface selection in the function name.
        case "lock" | "fpp" => args match {
          case ListBuffer(fn, field : L4_FieldAccess)                                                                           => // option 1: filename, field
            L4_ReadField(fn, field, ioInterface = ifaceSelection)
          case ListBuffer(fn, field : L4_FieldAccess, inclGhost : L4_BooleanConstant)                                           => // option 2: filename, field, inclGhost
            L4_ReadField(fn, field, ioInterface = ifaceSelection, includeGhostLayers = inclGhost.value)
          case ListBuffer(fn, field : L4_FieldAccess, inclGhost : L4_BooleanConstant, useBin : L4_BooleanConstant)              => // option 3: filename, field, inclGhost, useBin
            L4_ReadField(fn, field, ioInterface = ifaceSelection, includeGhostLayers = inclGhost.value, binaryInput = useBin.value)
          case ListBuffer(fn, field : L4_FieldAccess, inclGhost : L4_BooleanConstant, useBin : L4_BooleanConstant, cond)        => // option 4: filename, field, inclGhost, useBin, cond
            L4_ReadField(fn, field, ioInterface = ifaceSelection, includeGhostLayers = inclGhost.value, binaryInput = useBin.value, condition = Some(cond))
          case ListBuffer(fn, field : L4_FieldAccess, inclGhost : L4_BooleanConstant, useBin : L4_BooleanConstant, cond, sep)   => // option 5: filename, field, inclGhost, useBin, cond, sep
            if(useBin.value) Logger.error("Invalid parameter combination in \"readField\": binaryMode = true and separator set.")
            L4_ReadField(fn, field, ioInterface = ifaceSelection, includeGhostLayers = inclGhost.value, binaryInput = useBin.value, condition = Some(cond), separator = Some(sep))
          case _ =>
            Logger.error("Ignoring call to " + fctName + " with unsupported arguments: " + args.mkString(", "))
        }
        case "mpiio" => args match {
          case ListBuffer(fn, field : L4_FieldAccess)                                                                                                => // option 1: filename, field
            L4_ReadField(fn, field, ioInterface = ifaceSelection)
          case ListBuffer(fn, field : L4_FieldAccess, inclGhost : L4_BooleanConstant)                                                                => // option 2: filename, field, inclGhost
            L4_ReadField(fn, field, ioInterface = ifaceSelection, includeGhostLayers = inclGhost.value)
          case ListBuffer(fn, field : L4_FieldAccess, inclGhost : L4_BooleanConstant, canonicalOrder : L4_BooleanConstant)                           => // option 3: filename, field, inclGhost, canonicalOrder
            L4_ReadField(fn, field, ioInterface = ifaceSelection, includeGhostLayers = inclGhost.value, canonicalOrder = canonicalOrder.value)
          case ListBuffer(fn, field : L4_FieldAccess, inclGhost : L4_BooleanConstant, canonicalOrder : L4_BooleanConstant, repr : L4_StringConstant) => // option 4: filename, field, inclGhost, canonicalOrder, repr
            if (!List("external32", "internal", "native").contains(repr.value)) Logger.error("Unknown representation type for MPI-I/O. Should be: \"external32\", \"internal\", \"native\".")
            L4_ReadField(fn, field, ioInterface = ifaceSelection, includeGhostLayers = inclGhost.value, canonicalOrder = canonicalOrder.value, mpiioRepresentation = Some(repr))
          case _                                                                                                                                     =>
            Logger.error("Ignoring call to " + fctName + " with unsupported arguments: " + args.mkString(", "))
        }
        case "hdf5" => args match {
          case ListBuffer(fn, datasetPath, field : L4_FieldAccess)                                                                      => // option 1: filename, /path/to/dataset, field
            L4_ReadField(fn, field, ioInterface = ifaceSelection, dataset = Some(datasetPath))
          case ListBuffer(fn, datasetPath, field : L4_FieldAccess, inclGhost : L4_BooleanConstant)                                      => // option 2: filename, /path/to/dataset, field, inclGhost
            L4_ReadField(fn, field, ioInterface = ifaceSelection, includeGhostLayers = inclGhost.value, dataset = Some(datasetPath))
          case ListBuffer(fn, datasetPath, field : L4_FieldAccess, inclGhost : L4_BooleanConstant, canonicalOrder : L4_BooleanConstant) => // option 3: filename, /path/to/dataset, field, inclGhost, canonicalOrder
            L4_ReadField(fn, field, ioInterface = ifaceSelection, includeGhostLayers = inclGhost.value, canonicalOrder = canonicalOrder.value, dataset = Some(datasetPath))
          case _                                                                                                                        =>
            Logger.error("Ignoring call to " + fctName + " with unsupported arguments: " + args.mkString(", "))
        }
        case "nc" => args match {
          case ListBuffer(fn, datasetName, field : L4_FieldAccess)                                                                      => // option 1: filename, datasetName, field
            L4_ReadField(fn, field, ioInterface = ifaceSelection, dataset = Some(datasetName))
          case ListBuffer(fn, datasetName, field : L4_FieldAccess, inclGhost : L4_BooleanConstant)                                      => // option 2: filename, datasetName, field, inclGhost
            L4_ReadField(fn, field, ioInterface = ifaceSelection, includeGhostLayers = inclGhost.value, dataset = Some(datasetName))
          case ListBuffer(fn, datasetName, field : L4_FieldAccess, inclGhost : L4_BooleanConstant, canonicalOrder : L4_BooleanConstant) => // option 3: filename, datasetName, field, inclGhost, canonicalOrder
            L4_ReadField(fn, field, ioInterface = ifaceSelection, includeGhostLayers = inclGhost.value, canonicalOrder = canonicalOrder.value, dataset = Some(datasetName))
          case _                                                                                                                        =>
            Logger.error("Ignoring call to " + fctName + " with unsupported arguments: " + args.mkString(", "))
        }
        case "sion" => args match {
          case ListBuffer(fn, field : L4_FieldAccess)                                       => // option 1: filename, field
            L4_ReadField(fn, field, ioInterface = ifaceSelection)
          case ListBuffer(fn, field : L4_FieldAccess, inclGhost : L4_BooleanConstant)       => // option 2: filename, field, inclGhost
            L4_ReadField(fn, field, ioInterface = ifaceSelection, includeGhostLayers = inclGhost.value)
          case ListBuffer(fn, field : L4_FieldAccess, inclGhost : L4_BooleanConstant, cond) => // option 3: filename, field, inclGhost, cond
            L4_ReadField(fn, field, ioInterface = ifaceSelection, includeGhostLayers = inclGhost.value, condition = Some(cond))
          case _ =>
            Logger.error("Ignoring call to " + fctName + " with unsupported arguments: " + args.mkString(", "))
        }
        case _ =>
          Logger.error("Ignoring call to " + fctName + " with unsupported I/O interface: " + ifaceSelection)
          L4_NullStatement
      }
  })
}
