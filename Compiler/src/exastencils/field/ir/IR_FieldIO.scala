package exastencils.field.ir

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_StringConstant
import exastencils.io.ir.IR_FileAccess
import exastencils.io.ir.IR_FileAccess_FPP
import exastencils.io.ir.IR_FileAccess_HDF5
import exastencils.io.ir.IR_FileAccess_Locking
import exastencils.io.ir.IR_FileAccess_MPIIO
import exastencils.io.ir.IR_FileAccess_None
import exastencils.io.ir.IR_FileAccess_PnetCDF
import exastencils.io.ir.IR_FileAccess_SionLib
import exastencils.logger.Logger

// IR_FieldIO

object IR_FieldIO {
  private var counter : Int = 0
  def getNewStreamName() : String = {
    counter += 1
    "fieldStream_%03d".format(counter)
  }

  private var fileNameCounter : Int = 0
  def getNewFileName() : String = {
    fileNameCounter += 1
    "fieldName_%03d".format(fileNameCounter)
  }
}

trait IR_FieldIO {
  val fmtOptionsHDF5 = List("hdf5", "h5")
  val fmtOptionsNetCDF = List("netCDF", "nc")
  val fmtOptionsSION = List("sionlib", "sion")
  val fmtOptionsSharedFile = fmtOptionsHDF5 ++ fmtOptionsNetCDF ++ fmtOptionsSION

  // get file extensions in case that the user has specified it (also for backwards-compatibility)
  val extension = "[.][^.]+$".r
  def getExtension(fn : IR_Expression) : String = extension findFirstIn fn.asInstanceOf[IR_StringConstant].value getOrElse ""

  // appends file extension depending on specified format
  def createFilename(basename : IR_Expression, fmt : IR_Expression) : IR_Expression = {
    val ext = getExtension(basename)
    val base = basename.asInstanceOf[IR_StringConstant].value
    val fn = if(ext.trim.isEmpty) {
      fmt.asInstanceOf[IR_StringConstant].value match {
        case "ascii" => base + ".txt"
        case "bin"   => base + ".bin"
        case s : String if fmtOptionsHDF5.contains(s) => base + ".hdf5"
        case s : String if fmtOptionsNetCDF.contains(s) => base + ".nc"
        case s : String if fmtOptionsSION.contains(s) => base + ".sion"
        case _ =>
          Logger.warn("Unsupported file format: " + _)
          ""
      }
    } else {
      base
    }
    IR_StringConstant(fn)
  }

  def validateInputParams(format : IR_Expression, outputSingleFile : Boolean, useLocking : Boolean) : Boolean = {
    val fmt = format.asInstanceOf[IR_StringConstant].value
    if(fmtOptionsSharedFile.contains(fmt) && !outputSingleFile) {
      Logger.warn("writeField: unsupported parameter combination: Shared file format \"" + fmt + "\" with outputSingleFile=\"false\". Parameter <outputSingleFile> is ignored.")
    }
    if(fmtOptionsSharedFile.contains(fmt) && useLocking) {
      Logger.warn("writeField: unsupported parameter combination: Shared file format \"" + fmt + "\" with useLocking=\"true\". Parameter <useLocking> is ignored.")
    }
    if(useLocking && !outputSingleFile) {
      Logger.warn("Ignoring call to writeField with unsupported parameter combination: useLocking=\"true\" and outputSingleFile=\"false\". Locking only applicable with a shared files.")
      return false
    }
    true
  }

  def selectAndAddStatements(
      basenameFile : IR_Expression,
      field : IR_Field,
      slot : IR_Expression,
      condition : Option[IR_Expression],
      includeGhostLayers : Boolean,
      format : IR_Expression,
      outputSingleFile : Boolean,
      useLock : Boolean,
      doWrite : Boolean,
      onlyVals : Boolean) : IR_FileAccess = {

    val fn = createFilename(basenameFile, format)
    val fmt = format.asInstanceOf[IR_StringConstant].value
    fmt match {
      case "ascii" | "bin"                            =>
        if (!outputSingleFile) {
          IR_FileAccess_FPP(fn, field, slot, includeGhostLayers, fmt == "ascii", writeAccess = doWrite, onlyValues = onlyVals, condition)
        } else if (useLock) {
          IR_FileAccess_Locking(fn, field, slot, includeGhostLayers, fmt == "ascii", writeAccess = doWrite, onlyValues = onlyVals, condition)
        } else {
          IR_FileAccess_MPIIO(fn, field, slot, includeGhostLayers, fmt == "ascii", writeAccess = true)
        }
      case s : String if fmtOptionsHDF5.contains(s)   =>
        IR_FileAccess_HDF5(fn, field, slot, includeGhostLayers, writeAccess = doWrite)
      case s : String if fmtOptionsNetCDF.contains(s) =>
        IR_FileAccess_PnetCDF(fn, field, slot, includeGhostLayers, writeAccess = doWrite)
      case s : String if fmtOptionsSION.contains(s)   =>
        IR_FileAccess_SionLib(fn, field, slot, includeGhostLayers, writeAccess = doWrite)
      case _                                          =>
        Logger.warn("Ignoring call to writeField with unsupported format: " + fmt)
        IR_FileAccess_None(field, slot)
    }
  }
}
