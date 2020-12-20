package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Access
import exastencils.base.ir.IR_AddressOf
import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExternalFunctionReference
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_UnknownDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.config.Knowledge
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.util.ir.IR_Print

/// IR_PnetCDF_API
// provides the most important datatypes and functions of the PnetCDF (parallel) and NetCDF (serial) API


trait IR_PnetCDF_API {
  val err_decl = IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("err"))
  val err = IR_VariableAccess(err_decl)

  /*
    DATATYPES
  */

  /*
  Determines the netCDF datatype of a field. Those are commonly used when defining variables/attributes.
  Table from: http://cucis.ece.northwestern.edu/projects/PnetCDF/doc/pnetcdf-c/Variable-Types.html#Variable-Types
  */
  def ncDatatype(datatype : IR_Datatype) : IR_VariableAccess = {
    val dt = datatype.resolveBaseDatatype.prettyprint match {
      case "char"                       => "NC_CHAR"
      case "byte"                       => "NC_BYTE"
      case "unsigned byte"              => "NC_UBYTE"
      case "short"                      => "NC_SHORT"
      case "unsigned short"             => "NC_USHORT"
      case "int"                        => "NC_INT"
      case "unsigned" | "unsigned int"  => "NC_UINT"
      case "float"                      => "NC_FLOAT"
      case "double"                     => "NC_DOUBLE"
      case "long long"                  => "NC_INT64"
      case "unsigned long long"         => "NC_UINT64"
      case _ => Logger.error("Unsupported datatype when using PnetCDF: " + datatype.resolveBaseDatatype.prettyprint)
    }
    IR_VariableAccess(dt, IR_UnknownDatatype)
  }

  /*
  Conversion table used to get the corresponding function of the API for a given datatype.
  When reading/writing data (vars, attrs, ...), it is possible to specify the datatype of the memory buffer per argument (flexible API) or by using a designated function (e.g. ncmpi_get_varm_double)
  but the flexible API only exists for parallel netcdf and makes the handling of parallel/serial programs cumbersome -> designated functions are used instead
  Table from: http://cucis.ece.northwestern.edu/projects/PnetCDF/doc/pnetcdf-c/ncmpi_005fget_005fvarm_005f_003ctype_003e.html#ncmpi_005fget_005fvarm_005f_003ctype_003e
  */
  def typeNameFunction(datatype: IR_Datatype) : String = {
    datatype.resolveBaseDatatype.prettyprint match {
      case "char"               => "text"
      case "signed char"        => "schar"
      case "short"              => "short"
      case "int"                => "int"
      case "float"              => "float"
      case "double"             => "double"
      case "unsigned char"      => "uchar"
      case "unsigned short"     => "ushort"
      case "unsigned int"       => "uint"
      case "long long"          => "longlong"
      case "unsigned longlong"  => "ulonglong"
      case _ => Logger.error("Unsupported datatype when using PnetCDF: " + datatype.resolveBaseDatatype.prettyprint)
    }
  }

  /*
    FUNCTIONS
  */
  // calls a function from the PnetCDF library and checks if an error occurred (i.e. err was set to a value < 0) when debug statements are enabled
  def callNcFunction(funcName : String, datatype : Option[IR_Datatype], args : IR_Expression*) : ListBuffer[IR_Statement] = {
    // handle serial/parallel API differences (exceptions: ncmpi_create ncmpi_create <-> and ncmpi_open <-> nc_open due to different signatures)
    val nonFlexibleFuncName = funcName.replaceFirst("<type>", if (datatype.isDefined) typeNameFunction(datatype.get) else "")
    val functionName = if (Knowledge.mpi_enabled) // parallel netcdf functions have a different prefix
      nonFlexibleFuncName
    else
      nonFlexibleFuncName.replaceFirst("ncmpi", "nc").replaceFirst("_all", "")

    // call function and print debugging statements
    val stmts : ListBuffer[IR_Statement] = ListBuffer()
    stmts += IR_Assignment(err, IR_FunctionCall(IR_ExternalFunctionReference(functionName), args : _*))
    if (Knowledge.parIO_generateDebugStatements) {
      val buildErrorStringFunction = if (Knowledge.mpi_enabled) "ncmpi_strerror" else "nc_strerror"
      stmts += IR_IfCondition(err Neq IR_VariableAccess("NC_NOERR", IR_UnknownDatatype),
        IR_Print(IR_VariableAccess("std::cout", IR_UnknownDatatype),
          IR_StringConstant("Rank: "), MPI_IV_MpiRank, IR_StringConstant(". "),
          IR_VariableAccess("__FILE__", IR_UnknownDatatype), IR_StringConstant(": Error at line: "), IR_VariableAccess("__LINE__", IR_UnknownDatatype),
          IR_StringConstant(". Message: "), IR_FunctionCall(IR_ExternalFunctionReference(buildErrorStringFunction), err), IR_Print.endl)
      )
    }
    stmts
  }

  /* serial functions (others handled by callNcFunction wrapper) */
  def nc_create(filename : IR_Expression, mode : IR_VariableAccess, fileId : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callNcFunction("nc_create", datatype = None, filename, mode, IR_AddressOf(fileId))

  def nc_open(filename : IR_Expression, mode : IR_VariableAccess, fileId : IR_VariableAccess) : ListBuffer[IR_Statement] =
  callNcFunction("nc_open", datatype = None, filename, mode, IR_AddressOf(fileId))

  /* parallel functions */
  def ncmpi_create(comm : IR_VariableAccess, filename : IR_Expression, mode : IR_VariableAccess, info : IR_VariableAccess, fileId : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callNcFunction("ncmpi_create", datatype = None, comm, filename, mode, info, IR_AddressOf(fileId))

  def ncmpi_open(comm : IR_VariableAccess, filename : IR_Expression, mode : IR_VariableAccess, info : IR_VariableAccess, fileId : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callNcFunction("ncmpi_open", datatype = None, comm, filename, mode, info, IR_AddressOf(fileId))

  def ncmpi_def_dim(fileId : IR_VariableAccess, name : IR_Expression, dimLength : IR_Expression, dimIdPtr : IR_Expression) : ListBuffer[IR_Statement] =
    callNcFunction("ncmpi_def_dim", datatype = None, fileId, name, dimLength, dimIdPtr)

  def ncmpi_def_var(fileId : IR_VariableAccess, name : IR_Expression, datatype: IR_Datatype, ndims : Int, dimIdPtr : IR_Expression, varIdPtr : IR_Expression) : ListBuffer[IR_Statement] =
    callNcFunction("ncmpi_def_var", datatype = None, fileId, name, ncDatatype(datatype), ndims, dimIdPtr, varIdPtr)

  def ncmpi_inq_varid(fileId : IR_VariableAccess, varName : IR_Expression, varIdPtr : IR_Expression) : ListBuffer[IR_Statement] =
    callNcFunction("ncmpi_inq_varid", datatype = None, fileId, varName, varIdPtr)

  def ncmpi_enddef(fileId : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callNcFunction("ncmpi_enddef", datatype = None, fileId)

  def ncmpi_begin_indep_data(fileId : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callNcFunction("ncmpi_begin_indep_data", datatype = None, fileId)

  def ncmpi_end_indep_data(fileId : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callNcFunction("ncmpi_end_indep_data", None, fileId)

  def ncmpi_close(fileId : IR_VariableAccess) : ListBuffer[IR_Statement] =
    callNcFunction("ncmpi_close", None, fileId)

  /* non-flexible API */

  // collective calls
  def ncmpi_put_var1_type_all(datatype: IR_Datatype, fileId : IR_VariableAccess, varId : IR_Access, indexPtr : IR_Expression, bufPtr : IR_AddressOf) : ListBuffer[IR_Statement] =
    callNcFunction("ncmpi_put_var1_<type>_all", datatype = Some(datatype), fileId, varId, indexPtr, bufPtr)

  def ncmpi_put_vara_type_all(datatype: IR_Datatype, fileId : IR_VariableAccess, varId : IR_Access, startPtr : IR_Expression, countPtr : IR_Expression, bufPtr : IR_AddressOf) : ListBuffer[IR_Statement] =
    callNcFunction("ncmpi_put_vara_<type>_all", Some(datatype), fileId, varId, startPtr, countPtr, bufPtr)

  def ncmpi_put_varm_type_all(datatype: IR_Datatype, fileId : IR_VariableAccess, varId : IR_Access, startPtr : IR_Expression, countPtr : IR_Expression, stridePtr : IR_Expression, imapPtr : IR_Expression, bufPtr : IR_AddressOf) : ListBuffer[IR_Statement] =
    callNcFunction("ncmpi_put_varm_<type>_all", Some(datatype), fileId, varId, startPtr, countPtr, stridePtr, imapPtr, bufPtr)

  def ncmpi_get_vara_type_all(datatype: IR_Datatype, fileId : IR_VariableAccess, varId : IR_Access, startPtr : IR_Expression, countPtr : IR_Expression, bufPtr : IR_AddressOf) : ListBuffer[IR_Statement] =
    callNcFunction("ncmpi_get_vara_<type>_all", Some(datatype), fileId, varId, startPtr, countPtr, bufPtr)

  def ncmpi_get_varm_type_all(datatype: IR_Datatype, fileId : IR_VariableAccess, varId : IR_Access, startPtr : IR_Expression, countPtr : IR_Expression, stridePtr : IR_Expression, imapPtr : IR_Expression, bufPtr : IR_AddressOf) : ListBuffer[IR_Statement] =
    callNcFunction("ncmpi_get_varm_<type>_all", Some(datatype), fileId, varId, startPtr, countPtr, stridePtr, imapPtr, bufPtr)

  // independent calls
  def ncmpi_put_var1_type(datatype: IR_Datatype, fileId : IR_VariableAccess, varId : IR_Access, indexPtr : IR_Expression, bufPtr : IR_AddressOf) : ListBuffer[IR_Statement] =
    callNcFunction("ncmpi_put_var1_<type>", datatype = Some(datatype), fileId, varId, indexPtr, bufPtr)

  def ncmpi_put_vara_type(datatype: IR_Datatype, fileId : IR_VariableAccess, varId : IR_Access, startPtr : IR_Expression, countPtr : IR_Expression, bufPtr : IR_AddressOf) : ListBuffer[IR_Statement] =
    callNcFunction("ncmpi_put_vara_<type>", Some(datatype), fileId, varId, startPtr, countPtr, bufPtr)

  def ncmpi_put_varm_type(datatype: IR_Datatype, fileId : IR_VariableAccess, varId : IR_Access, startPtr : IR_Expression, countPtr : IR_Expression, stridePtr : IR_Expression, imapPtr : IR_Expression, bufPtr : IR_AddressOf) : ListBuffer[IR_Statement] =
    callNcFunction("ncmpi_put_varm_<type>", Some(datatype), fileId, varId, startPtr, countPtr, stridePtr, imapPtr, bufPtr)

  def ncmpi_get_vara_type(datatype: IR_Datatype, fileId : IR_VariableAccess, varId : IR_Access, startPtr : IR_Expression, countPtr : IR_Expression, bufPtr : IR_AddressOf) : ListBuffer[IR_Statement] =
    callNcFunction("ncmpi_get_vara_<type>", Some(datatype), fileId, varId, startPtr, countPtr, bufPtr)

  def ncmpi_get_varm_type(datatype: IR_Datatype, fileId : IR_VariableAccess, varId : IR_Access, startPtr : IR_Expression, countPtr : IR_Expression, stridePtr : IR_Expression, imapPtr : IR_Expression, bufPtr : IR_AddressOf) : ListBuffer[IR_Statement] =
    callNcFunction("ncmpi_get_varm_<type>", Some(datatype), fileId, varId, startPtr, countPtr, stridePtr, imapPtr, bufPtr)

}
