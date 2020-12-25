package exastencils.field.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Expandable
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_NullExpression
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.io.ir.IR_DataBuffer
import exastencils.io.ir.IR_FileAccess
import exastencils.io.ir.IR_FileAccess_FPP
import exastencils.io.ir.IR_FileAccess_HDF5
import exastencils.io.ir.IR_FileAccess_Locking
import exastencils.io.ir.IR_FileAccess_MPIIO
import exastencils.io.ir.IR_FileAccess_PnetCDF
import exastencils.io.ir.IR_FileAccess_SionLib
import exastencils.logger.Logger

/// IR_FieldIO

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

abstract class IR_FieldIO(
    // general parameters
    filename : IR_Expression,
    field : IR_Field,
    slot : IR_Expression,
    ioInterface : IR_Expression,
    doWrite : Boolean,
    onlyVals : Boolean,
    includeGhostLayers : Boolean,
    // locking/fpp specific parameters (essentially when using "iostreams")
    useBinary : Boolean = false,
    separator : IR_Expression = IR_StringConstant(" "),
    condition : IR_Expression = true,
    // dataset which can be specified for a netCDF/HDF5 file (for HDF5 this can be a path)
    dataset : IR_Expression = IR_NullExpression) extends IR_Statement with IR_Expandable {

  val fieldAsDataBuffer = IR_DataBuffer(field, slot, includeGhostLayers, None, dataset = Some(dataset), canonicalOrder = true)

  // wrapper function that generates statements for file access using the specified I/O interface
  def generateFileAccess(optPrintComponents : Option[ListBuffer[IR_Expression]] = None) : IR_FileAccess = {

    ioInterface.asInstanceOf[IR_StringConstant].value.toLowerCase match {
      case "lock"  =>
        IR_FileAccess_Locking(filename, ListBuffer(fieldAsDataBuffer), useBinary, doWrite, separator, condition, optPrintComponents)
      case "fpp"   =>
        IR_FileAccess_FPP(filename, ListBuffer(fieldAsDataBuffer), useBinary, doWrite, separator, condition, optPrintComponents)
      case "mpiio" =>
        IR_FileAccess_MPIIO(filename, ListBuffer(fieldAsDataBuffer), doWrite)
      case "hdf5"  =>
        IR_FileAccess_HDF5(filename, ListBuffer(fieldAsDataBuffer), doWrite)
      case "nc"    =>
        IR_FileAccess_PnetCDF(filename, ListBuffer(fieldAsDataBuffer), doWrite)
      case "sion"  =>
        IR_FileAccess_SionLib(filename, ListBuffer(fieldAsDataBuffer), doWrite, condition)
      case _       =>
        Logger.error("Ignoring call to " + (if (doWrite) {if (onlyVals) "writeField" else "printField"} else "readField") + " using unsupported I/O interface: " + ioInterface)
    }
  }
}
