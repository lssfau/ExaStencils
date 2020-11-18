package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.util.ir.IR_Print

case class IR_FileAccess_PnetCDF(
    var fileName : IR_Expression,
    var datasetName : IR_Expression,
    var field : IR_Field,
    var slot : IR_Expression,
    var includeGhostLayers : Boolean,
    var writeAccess : Boolean,
    var appendedMode : Boolean = false) extends IR_FileAccess(fileName, field, slot, includeGhostLayers, writeAccess, appendedMode) {

  // TODO: Handling collective/independent I/O
  // TODO: serial function calls

  /* hints:
    1. nc_header_align_size: Default size if 512 Byte. In case that an exisiting file is re-opened and more metadata is added, the file header expands and actual data is moved to higher file offsets
  */

  // general variables
  override def openMode : IR_VariableAccess = if(appendedMode) {
    // from: http://cucis.ece.northwestern.edu/projects/PnetCDF/doc/pnetcdf-c/ncmpi_005fopen.html
    if(writeAccess) IR_VariableAccess("NC_WRITE", IR_UnknownDatatype) else IR_VariableAccess("NC_NOWRITE", IR_UnknownDatatype)
  } else {
    // from: http://cucis.ece.northwestern.edu/projects/PnetCDF/doc/pnetcdf-c/ncmpi_005fcreate.html
    IR_VariableAccess("NC_64BIT_DATA | NC_CLOBBER", IR_UnknownDatatype) // create new CDF-5 file (clobber equals truncation)
  }

  // time variable handling
  val useTimeDim = true // TODO add as parameter: [true | false] to enable/disable writing record variables
  val numDimsDataAndTime : Int = numDimsData + (if (useTimeDim) 1 else 0) // record variables are bound to the "unlimited" dimension (often used as time)
  val indexTimeArray_decl = IR_VariableDeclaration(MPI_Offset, IR_FileAccess.declareVariable("printStep"), 0) // TODO parameter
  val timeStep_decl = IR_VariableDeclaration(IR_DoubleDatatype, IR_FileAccess.declareVariable("t"), 0.0) // TODO parameter

  // describes in-memory structure of array
  val linearizedOffsets : Array[IR_IntegerConstant] = (0 until numDimsData).map(d => {
    if(d == 0)
      IR_IntegerConstant(1) // stride in x direction is "1"
    else
      IR_IntegerConstant((0 until d).map(dd => field.layout.defTotal(dd)).product) // product of total points from "previous dimensions"
  }).toArray.reverse

  // decls
  val err_decl = IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("err"))
  val ncFile_decl = IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("ncFile"))
  val info_decl = IR_VariableDeclaration(IR_SpecialDatatype("MPI_Info"), IR_FileAccess.declareVariable("info"), IR_VariableAccess("MPI_INFO_NULL", IR_UnknownDatatype)) //TODO handle hints
  val dimId_decl = IR_VariableDeclaration(IR_ArrayDatatype(IR_IntegerDatatype, numDimsDataAndTime), IR_FileAccess.declareVariable("dimId")) // numDimsData + time dimension
  val varIdField_decl = IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("varId_"+field.name))
  val varIdTime_decl = IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("varIdTime"))
  val stride_decl = IR_VariableDeclaration(IR_ArrayDatatype(MPI_Offset, numDimsDataAndTime), IR_FileAccess.declareVariable("stride"),
    IR_InitializerList( (if (useTimeDim) IR_IntegerConstant(1) +: strideLocal else strideLocal) : _*) // add one more entry for unlimited "time" dimension
  )
  val count_decl = IR_VariableDeclaration(IR_ArrayDatatype(MPI_Offset, numDimsDataAndTime), IR_FileAccess.declareVariable("count"),
    IR_InitializerList( (if (useTimeDim) IR_IntegerConstant(1) +: innerPointsLocal else innerPointsLocal) : _*) // prepend "1" since "1*(innerPoints_local.product)" values are written per timestep
  )
  val imap_decl = IR_VariableDeclaration(IR_ArrayDatatype(MPI_Offset, numDimsDataAndTime), IR_FileAccess.declareVariable("imap"),
    IR_InitializerList( (if (useTimeDim) IR_IntegerConstant(0) +: linearizedOffsets else linearizedOffsets) : _*) // prepend "0" since the memory layout doesn't change with the additional time dimension
  )
  val globalDims_decl = IR_VariableDeclaration(IR_ArrayDatatype(MPI_Offset, numDimsDataAndTime), IR_FileAccess.declareVariable("globalDims"), IR_InitializerList(innerPointsGlobal : _*))
  val globalStart_decl = IR_VariableDeclaration(IR_ArrayDatatype(MPI_Offset, numDimsDataAndTime), IR_FileAccess.declareVariable("globalStart"), IR_InitializerList(IR_IntegerConstant(0)))
  val declarations : ListBuffer[IR_VariableDeclaration] = ListBuffer(
    err_decl, ncFile_decl, info_decl, dimId_decl, varIdField_decl,
    stride_decl, count_decl, imap_decl, globalDims_decl, globalStart_decl
  )
  if(useTimeDim) {
    declarations += timeStep_decl // TODO
    declarations += indexTimeArray_decl // TODO
    declarations += varIdTime_decl
  }

  // accesses
  val err = IR_VariableAccess(err_decl)
  val ncFile = IR_VariableAccess(ncFile_decl)
  val dimId = IR_VariableAccess(dimId_decl)
  val dimIdTime = IR_ArrayAccess(dimId, 0) // "time" is the slowest-varying dimension ("0" in KJI order)
  val varIdField = IR_VariableAccess(varIdField_decl)
  val varIdTime = IR_VariableAccess(varIdTime_decl)
  val info = IR_VariableAccess(info_decl)
  val stride = IR_VariableAccess(stride_decl)
  val count = IR_VariableAccess(count_decl)
  val imap = IR_VariableAccess(imap_decl) // describes local memory layout
  val globalDims = IR_VariableAccess(globalDims_decl)
  val globalStart = IR_VariableAccess(globalStart_decl)

  // from: http://cucis.ece.northwestern.edu/projects/PnetCDF/doc/pnetcdf-c/Variable-Types.html#Variable-Types
  val ncDatatype : IR_VariableAccess = {
    val dt = field.layout.datatype.prettyprint match {
      case "char" => "NC_CHAR"
      case "byte" => "NC_BYTE"
      case "unsigned byte" => "NC_UBYTE"
      case "short" => "NC_SHORT"
      case "unsigned short" => "NC_USHORT"
      case "int" => "NC_INT"
      case "unsigned" | "unsigned int" => "NC_UINT"
      case "float" => "NC_FLOAT"
      case "double" => "NC_DOUBLE"
      case "long long" => "NC_INT64"
      case "unsigned long long" => "NC_UINT64"
      case _ => Logger.error("Unsupported field datatype when using PnetCDF: " + field.layout.datatype.resolveBaseDatatype.prettyprint)
    }
    IR_VariableAccess(dt, IR_UnknownDatatype)
  }

  def createDimName(d : Int) = IR_StringConstant(IR_FileAccess.declareVariable(field.name + "_" + "dim" + (numDimsData - d)))

  // calls a function from the PnetCDF library and checks if an error occured (i.e. err was set to a value < 0) when debug statements are enabled
  def callNcFunction(funcName : String, args : IR_Expression*) : ListBuffer[IR_Statement] = {
    val stmts : ListBuffer[IR_Statement] = ListBuffer()
    stmts += IR_Assignment(err, IR_FunctionCall(IR_ExternalFunctionReference(funcName), args : _*))
    if(Knowledge.parIO_generateDebugStatements)
      stmts += IR_IfCondition(err < 0,
        IR_Print(IR_VariableAccess("std::cout", IR_UnknownDatatype), IR_VariableAccess("__FILE__", IR_UnknownDatatype), IR_StringConstant(": Error at line: "), IR_VariableAccess("__LINE__", IR_UnknownDatatype), IR_Print.endl)
      )
    stmts
  }

  def accessFileFragwise(accessStmts : ListBuffer[IR_Statement]) : IR_LoopOverFragments = {
    // set global starting index for fragment
    val setOffsetFrag : ListBuffer[IR_Assignment] = (0 until numDimsData).map(d => {
      IR_Assignment(IR_ArrayAccess(globalStart, d + (if (useTimeDim) 1 else 0)), startIdxGlobal(d))
    }).to[ListBuffer]

    IR_LoopOverFragments(
      IR_IfCondition(IR_IV_IsValidForDomain(field.domain.index),
          setOffsetFrag ++ accessStmts
      )
    )
  }

  override def createOrOpenFile() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // add declarations
    for (decl <- declarations) {
      statements += decl
    }

    // create/open file
    if (writeAccess && !appendedMode)
      statements ++= callNcFunction("ncmpi_create", mpiCommunicator, fileName, openMode, info, IR_AddressOf(ncFile))
    else
      statements ++= callNcFunction("ncmpi_open", mpiCommunicator, fileName, openMode, info, IR_AddressOf(ncFile))

    statements
  }

  override def setupAccess() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // define dimensions in case of write operations
    if(writeAccess) {
      if(Knowledge.domain_onlyRectangular) {
        // define spatial dimensions for rectangular domains
        for(d <- 0 until numDimsData) {
          statements ++= callNcFunction("ncmpi_def_dim", ncFile, createDimName(d), IR_ArrayAccess(globalDims, d), IR_AddressOf(IR_ArrayAccess(dimId, d + (if (useTimeDim) 1 else 0))))
        }
        // define unlimited time dimension
        if(useTimeDim)
          statements ++= callNcFunction("ncmpi_def_dim", ncFile, IR_StringConstant("time"), IR_VariableAccess("NC_UNLIMITED", IR_UnknownDatatype), IR_AddressOf(dimIdTime))
      } else {
        // define as 1d linearized block of nodes, cells, ...
        // TODO
        Logger.error("Unimplemented!")
      }
    }

    // define/inquire variables for the field and time
    if(writeAccess) {
      if(useTimeDim) {
        statements ++= callNcFunction(
          "ncmpi_def_var", ncFile, IR_StringConstant("time"), IR_VariableAccess("NC_DOUBLE", IR_UnknownDatatype), IR_IntegerConstant(1), IR_AddressOf(dimIdTime), IR_AddressOf(varIdTime))
      }
      statements ++= callNcFunction("ncmpi_def_var", ncFile, datasetName, ncDatatype, numDimsDataAndTime, dimId, IR_AddressOf(varIdField))
    } else {
      //statements ++= callNcFunction("ncmpi_inq_varid", ncFile, IR_StringConstant("time"), IR_AddressOf(varIdTime))
      statements ++= callNcFunction("ncmpi_inq_varid", ncFile, datasetName, IR_AddressOf(varIdField))
    }

    // end "define mode" when writing the file
    if(writeAccess)
      statements ++= callNcFunction("ncmpi_enddef", ncFile)

    // go into "data mode"
    if(!Knowledge.parIO_useCollectiveIO) {
      // start individual I/O
      statements ++= callNcFunction("ncmpi_begin_indep_data", ncFile)
    }

    statements
  }

  override def cleanupAccess() : ListBuffer[IR_Statement] = ListBuffer() // nothing to cleanup yet

  override def closeFile() : ListBuffer[IR_Statement] = callNcFunction("ncmpi_close", ncFile)

  // field pointer at offset of first non-ghost index
  val fieldPtrInner = IR_AddressOf(IR_FieldAccess(field, slot, IR_ExpressionIndex((0 until numDimsData).map(d => field.layout.defIdxGhostLeftBegin(d)).toArray)))

  override def writeField() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    if(useTimeDim) {
      statements ++= callNcFunction("ncmpi_put_var1_double_all", ncFile, varIdTime, IR_AddressOf(IR_VariableAccess(indexTimeArray_decl)), IR_AddressOf(IR_VariableAccess(timeStep_decl)))
    }

    val bufcount = innerPointsLocal.reduce(_ * _) // indicates how many derived datatype elements are written to file
    val write = if(startIdxLocal.map(expr => expr.asInstanceOf[IR_IntegerConstant].value).sum == 0) {
      // use simpler method if the whole array can be written without having to exclude ghost layers
      callNcFunction("ncmpi_put_vara_all", ncFile, varIdField, globalStart, count, fieldptr, bufcount, IR_VariableAccess(field.layout.datatype.prettyprint_mpi, IR_UnknownDatatype))
    } else {
      // more complex method that describes the memory layout (i.e. linearized offsets for each dimension in KJI order)
      callNcFunction("ncmpi_put_varm_all", ncFile, varIdField, globalStart, count, stride, imap, fieldPtrInner, bufcount, IR_VariableAccess(field.layout.datatype.prettyprint_mpi, IR_UnknownDatatype))
    }

    statements += accessFileFragwise(write)

    statements
  }

  override def readField() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    val bufcount = innerPointsLocal.reduce(_ * _) // indicates how many derived datatype elements are written to file
    val read = if(startIdxLocal.map(expr => expr.asInstanceOf[IR_IntegerConstant].value).sum == 0) {
      // use simpler method if the whole array can be written without having to exclude ghost layers
      callNcFunction("ncmpi_get_vara_all", ncFile, varIdField, globalStart, count, fieldptr, bufcount, IR_VariableAccess(field.layout.datatype.prettyprint_mpi, IR_UnknownDatatype))
    } else {
      // more complex method that describes the memory layout (i.e. linearized offsets for each dimension in KJI order)
      callNcFunction("ncmpi_get_varm_all", ncFile, varIdField, globalStart, count, stride, imap, fieldPtrInner, bufcount, IR_VariableAccess(field.layout.datatype.prettyprint_mpi, IR_UnknownDatatype))
    }

    statements += accessFileFragwise(read)

    statements
  }

  // headers, paths and libs
  override def includes : ListBuffer[String] = ListBuffer("pnetcdf.h")
  override def libraries : ListBuffer[String] = ListBuffer("pnetcdf")
  override def pathsInc : ListBuffer[String] = ListBuffer("$(PNETCDF_HOME)/include")
  override def pathsLib : ListBuffer[String] = ListBuffer("$(PNETCDF_HOME)/lib")

  override def validateParams() : Unit = {
    if (datasetName == IR_NullExpression) {
      Logger.error("Parameter \"dataset\" was not specified.")
    }
  }
}
