package exastencils.field.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.io.ir._
import exastencils.logger.Logger
import exastencils.visualization.ir.IR_ResolveVisualizationPrinters

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

  private var resolveId : Int = Duplicate(IR_ResolveVisualizationPrinters.funcsResolved) // vis. funcs are resolved first
  def getNewResolveId() : Int = {
    val ret = Duplicate(resolveId)
    resolveId += 1
    ret
  }
}

abstract class IR_FieldIO extends IR_Statement with IR_Expandable {

  // general members
  def filename : IR_Expression
  def field : IR_Field
  def slot : IR_Expression
  def ioInterface : IR_Expression
  def doWrite : Boolean
  def onlyVals : Boolean
  def includeGhostLayers : Boolean
  def canonicalFileLayout : Boolean

  // locking/fpp specific members (essentially when using "iostreams")
  def useBinary : Boolean
  def separator : IR_Expression
  def condition : IR_Expression

  // dataset which can be specified for a netCDF/HDF5 file (for HDF5 this can be a path)
  def dataset : IR_Expression

  // binary representation for MPI-I/O
  def mpiioRepresentation : IR_StringConstant

  // wrapper function that generates statements for file access using the specified I/O interface
  def generateFileAccess(optSep : Option[IR_Expression] = None, optPrintComponents : Option[ListBuffer[IR_Expression]] = None) : IR_FileAccess = {
    val fieldAsDataBuffer = IR_DataBuffer(field, slot, includeGhostLayers, None, dataset = Some(dataset), canonicalFileLayout)

    ioInterface.asInstanceOf[IR_StringConstant].value.toLowerCase match {
      case "lock"  =>
        IR_FileAccess_Locking(filename, ListBuffer(fieldAsDataBuffer), useBinary, doWrite, optSep getOrElse separator, condition, optPrintComponents)
      case "fpp"   =>
        IR_FileAccess_FPP(filename, ListBuffer(fieldAsDataBuffer), useBinary, doWrite, optSep getOrElse separator, condition, optPrintComponents)
      case "mpiio" =>
        IR_FileAccess_MPIIO(filename, ListBuffer(fieldAsDataBuffer), doWrite, representation = mpiioRepresentation)
      case "hdf5"  =>
        IR_FileAccess_HDF5(filename, ListBuffer(fieldAsDataBuffer), doWrite, zlibCompressionLevel = if (onlyVals) Knowledge.hdf5_write_zlib_compression_level else 0)
      case "nc"    =>
        IR_FileAccess_PnetCDF(filename, ListBuffer(fieldAsDataBuffer), None, doWrite, timeIdx = IR_NullExpression, timeVal = IR_NullExpression, altFileMode = None)
      case "sion"  =>
        IR_FileAccess_SIONlib(filename, ListBuffer(fieldAsDataBuffer), doWrite, condition)
      case _       =>
        Logger.error("Ignoring call to " + (if (doWrite) { if (onlyVals) "writeField" else "printField" } else "readField") + " using unsupported I/O interface: " + ioInterface)
    }
  }
}
