package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_AddressOf
import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExternalFunctionReference
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_UnknownDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.config.Knowledge
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.util.ir.IR_Print
import exastencils.base.ir.IR_ImplicitConversion._

/// IR_Hdf5_API
// provides the most important datatypes and functions of the HDF5 API (for serial and parallel applications)

trait IR_Hdf5_API {
  val nullptr = IR_VariableAccess("NULL", IR_UnknownDatatype)

  /*
    DATATYPES
  */
  // from: https://support.hdfgroup.org/HDF5/doc1.6/UG/11_Datatypes.html
  def h5Datatype(datatype : IR_Datatype) : IR_VariableAccess = {
    val dt = datatype.resolveBaseDatatype.prettyprint match {
      case "char" => "H5T_NATIVE_CHAR"
      case "signed char" => "H5T_NATIVE_SCHAR"
      case "unsigned char" => "H5T_NATIVE_UCHAR"
      case "short" => "H5T_NATIVE_SHORT"
      case "unsigned short" => "H5T_NATIVE_USHORT"
      case "int" => "H5T_NATIVE_INT"
      case "unsigned" | "unsigned int" => "H5T_NATIVE_UINT"
      case "long" => "H5T_NATIVE_LONG"
      case "unsigned long" => "H5T_NATIVE_ULONG"
      case "long long" => "H5T_NATIVE_LLONG"
      case "unsigned long long" => "H5T_NATIVE_ULLONG"
      case "float" => "H5T_NATIVE_FLOAT"
      case "double" => "H5T_NATIVE_DOUBLE"
      case "long double" => "H5T_NATIVE_LDOUBLE"
      /*
      case "hsize_t" => "H5T_NATIVE_HSIZE"
      case "hssize_t" => "H5T_NATIVE_HSSIZE"
      case "herr_t" => "H5T_NATIVE_HERR"
      case "hbool_t" => "H5T_NATIVE_HBOOL"
      */
      case _ => Logger.error("Unsupported datatype when using HDF5: " + datatype.resolveBaseDatatype.prettyprint)
    }
    IR_VariableAccess(dt, IR_UnknownDatatype)
  }

  /*
    FUNCTIONS
  */
  // calls a function from the HDF5 library and checks if an error occured (i.e. toAssign was set to a value < 0) when debug statements are enabled
  def callH5Function(toAssign : IR_VariableAccess, funcName : String, args : IR_Expression*) : ListBuffer[IR_Statement] = {
    val stmts : ListBuffer[IR_Statement] = ListBuffer()
    stmts += IR_Assignment(toAssign, IR_FunctionCall(IR_ExternalFunctionReference(funcName), args : _*))
    if (Knowledge.parIO_generateDebugStatements)
      stmts += IR_IfCondition(toAssign < 0,
        IR_Print(IR_VariableAccess("std::cout", IR_UnknownDatatype),
          IR_StringConstant("Rank: "), MPI_IV_MpiRank, IR_StringConstant(". "),
          IR_VariableAccess("__FILE__", IR_UnknownDatatype), IR_StringConstant(": Error at line: "), IR_VariableAccess("__LINE__", IR_UnknownDatatype), IR_Print.endl))
    stmts
  }

  // groups and links
  def H5Lexists(ret : IR_VariableAccess, locId : IR_VariableAccess, groupName : String, propertyList : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callH5Function(ret, "H5Lexists", locId, IR_StringConstant(groupName), propertyList)

  def H5Gopen2(ret : IR_VariableAccess, locId : IR_VariableAccess, groupName : String, propertyListGroupAccess : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callH5Function(ret, "H5Gopen2", locId, IR_StringConstant(groupName), propertyListGroupAccess)

  def H5Gcreate2(ret : IR_VariableAccess, locId : IR_VariableAccess, groupName : String, propertyListLinkCreation : IR_VariableAccess, propertyListGroupCreation : IR_VariableAccess, propertyListGroupAccess : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callH5Function(ret, "H5Gcreate2", locId, IR_StringConstant(groupName), propertyListLinkCreation, propertyListGroupCreation, propertyListGroupAccess)

  def H5Gclose(err : IR_VariableAccess, group : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callH5Function(err, "H5Gclose", group)

  // property lists
  def H5Pcreate(propertyList : IR_VariableAccess, properyListClass : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callH5Function(propertyList, "H5Pcreate", properyListClass)

  def H5Pset_chunk(err : IR_VariableAccess, propertyList : IR_VariableAccess, rank : Int, chunkDimsPtr : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callH5Function(err, "H5Pset_chunk", propertyList, rank, chunkDimsPtr)

  def H5Pset_alignment(err : IR_VariableAccess, propertyList : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callH5Function(err, "H5Pset_alignment", propertyList, Knowledge.hdf5_object_alignment_threshold, Knowledge.hdf5_object_alignment_size)

  def H5Pset_istore_k(err : IR_VariableAccess, propertyList : IR_VariableAccess, value : IR_Expression) : ListBuffer[IR_Statement] =
    callH5Function(err, "H5Pset_istore_k", propertyList, value)

  def H5Pget_mdc_config(err : IR_VariableAccess, fileId : IR_VariableAccess, configPtr : IR_AddressOf) : ListBuffer[IR_Statement] =
    callH5Function(err, "H5Pget_mdc_config", fileId, configPtr)

  def H5Pset_mdc_config(err : IR_VariableAccess, fileId : IR_VariableAccess, configPtr : IR_AddressOf) : ListBuffer[IR_Statement] =
    callH5Function(err, "H5Pset_mdc_config", fileId, configPtr)

  def H5Pset_fapl_mpio(err : IR_VariableAccess, propertyList : IR_VariableAccess, comm : IR_VariableAccess, info : IR_Expression) : ListBuffer[IR_Statement] =
    callH5Function(err, "H5Pset_fapl_mpio", propertyList, comm, info)

  def H5Pset_dxpl_mpio(err : IR_VariableAccess, transferList : IR_VariableAccess, ioMode : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callH5Function(err, "H5Pset_dxpl_mpio", transferList, ioMode)

  def H5Pclose(err : IR_VariableAccess, propertyListToClose : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callH5Function(err, "H5Pclose", propertyListToClose)

  // files
  def H5Fcreate(fileId : IR_VariableAccess, filename : IR_Expression, mode : IR_VariableAccess, propertyListFileCreation : IR_VariableAccess, propertyListFileAccess : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callH5Function(fileId, "H5Fcreate", filename, mode, propertyListFileCreation, propertyListFileAccess)

  def H5Fopen(fileId : IR_VariableAccess, filename : IR_Expression, mode : IR_VariableAccess, propertyListFileAccess : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callH5Function(fileId, "H5Fopen", filename, mode, propertyListFileAccess)

  def H5Fclose(err : IR_VariableAccess, fileId : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callH5Function(err, "H5Fclose", fileId)

  // dataspaces
  def H5Screate(spaceId : IR_VariableAccess, spaceType : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callH5Function(spaceId, "H5Screate", spaceType)

  def H5Screate_simple(spaceId : IR_VariableAccess, rank : Int, dims : IR_VariableAccess, maxDims : IR_VariableAccess = nullptr) : ListBuffer[IR_Statement] =
    callH5Function(spaceId, "H5Screate_simple", rank, dims, maxDims)

  def H5Sselect_hyperslab(err : IR_VariableAccess, spaceId : IR_VariableAccess, selectOp : IR_VariableAccess, start : IR_VariableAccess, stride : IR_VariableAccess, count : IR_VariableAccess, block : IR_VariableAccess = nullptr) : ListBuffer[IR_Statement] =
    callH5Function(err, "H5Sselect_hyperslab", spaceId, IR_VariableAccess("H5S_SELECT_SET", IR_UnknownDatatype),
      start, stride, count, block)

  def H5Sget_simple_extent_dims(rank : IR_VariableAccess, spaceId : IR_VariableAccess, dims : IR_VariableAccess, maxDims : IR_VariableAccess = nullptr) : ListBuffer[IR_Statement] =
    callH5Function(rank, "H5Sget_simple_extent_dims", spaceId, dims, maxDims)

  def H5Sget_simple_extent_ndims(rank : IR_VariableAccess, spaceId : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callH5Function(rank, "H5Sget_simple_extent_ndims", spaceId)

  def H5Sclose(err : IR_VariableAccess, spaceId : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callH5Function(err, "H5Sclose", spaceId)

  // datasets
  def H5Dcreate2(setId : IR_VariableAccess, locId : IR_VariableAccess, name : IR_Expression, datatype : IR_Datatype, spaceId : IR_VariableAccess, propertyListLinkCreation : IR_VariableAccess, propertylistDatasetCreation : IR_VariableAccess, propertyListDatasetAccess : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callH5Function(setId, "H5Dcreate2", locId, name, h5Datatype(datatype), spaceId, propertyListLinkCreation, propertylistDatasetCreation, propertyListDatasetAccess)

  def H5Dopen2(setId : IR_VariableAccess, locId : IR_VariableAccess, name : IR_Expression, propertyListDatasetAccess : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callH5Function(setId, "H5Dopen2", locId, name, propertyListDatasetAccess)

  def H5Dread(err : IR_VariableAccess, setId : IR_VariableAccess, datatype: IR_Datatype, memspaceId : IR_VariableAccess, dataspaceId : IR_VariableAccess, transferList : IR_VariableAccess, bufPtr : IR_AddressOf) : ListBuffer[IR_Statement] =
    callH5Function(err, "H5Dread", setId, h5Datatype(datatype), memspaceId, dataspaceId, transferList, bufPtr)

  def H5Dwrite(err : IR_VariableAccess, setId : IR_VariableAccess, datatype: IR_Datatype, memspaceId : IR_VariableAccess, dataspaceId : IR_VariableAccess, transferList : IR_VariableAccess, bufPtr : IR_AddressOf) : ListBuffer[IR_Statement] =
    callH5Function(err, "H5Dwrite", setId, h5Datatype(datatype), memspaceId, dataspaceId, transferList, bufPtr)

  def H5Dget_space(spaceId : IR_VariableAccess, setId : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callH5Function(spaceId, "H5Dget_space", setId)

  def H5Dclose(err : IR_VariableAccess, setId : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callH5Function(err, "H5Dclose", setId)
}
