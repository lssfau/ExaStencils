package exastencils.io.ir

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.parallelization.api.mpi.MPI_IsRootProc
import exastencils.util.ir.IR_Print

case class IR_FileAccess_PnetCDF(
    var fileName : IR_Expression,
    var dataBuffers : ListBuffer[IR_DataBuffer],
    var writeAccess : Boolean,
    var appendedMode : Boolean = false) extends IR_FileAccess(fileName, dataBuffers, writeAccess, appendedMode) {

  // TODO: Test handling collective/independent I/O for "invalid" fragments
  // TODO: serial function calls

  /* hints:
    1. nc_header_align_size: Default size if 512 Byte. In case that an exisiting file is re-opened and more metadata is added, the file header expands and actual data is moved to higher file offsets
  */

  // general variables
  override def openMode : IR_VariableAccess = if (writeAccess) {
    if (appendedMode) {
      IR_VariableAccess("NC_WRITE", IR_UnknownDatatype) // open in read-write mode
    } else {
      if (Knowledge.mpi_enabled)
        IR_VariableAccess("NC_64BIT_DATA | NC_CLOBBER", IR_UnknownDatatype) // create new CDF-5 file (clobber equals truncation)
      else
        IR_VariableAccess("NC_64BIT_OFFSET | NC_CLOBBER", IR_UnknownDatatype) // create new CDF-2 file
    }
  } else {
    IR_VariableAccess("NC_NOWRITE", IR_UnknownDatatype) // open as read-only
  }

  // time variable handling
  val useTimeDim = true // TODO add as parameter: [true | false] to enable/disable writing record variables
  val numDimsDataAndTime = (numDimsData : Int) => numDimsData + (if (useTimeDim) 1 else 0) // record variables are bound to the "unlimited" dimension (often used as time)
  val indexTimeArray_decl = IR_VariableDeclaration(MPI_Offset, IR_FileAccess.declareVariable("printStep"), 0) // TODO make iv
  val timeStep_decl = IR_VariableDeclaration(IR_DoubleDatatype, IR_FileAccess.declareVariable("t"), 0.0) // TODO make iv

  // serial API uses "ptrdiff_t" for "imap" and "stride" parameters
  val ptrDatatype : IR_SpecialDatatype = if (Knowledge.mpi_enabled) MPI_Offset else IR_SpecialDatatype("ptrdiff_t")

  // decls
  val err_decl = IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("err"))
  val ncFile_decl = IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("ncFile"))
  val info_decl = IR_VariableDeclaration(IR_SpecialDatatype("MPI_Info"), IR_FileAccess.declareVariable("info"), IR_VariableAccess("MPI_INFO_NULL", IR_UnknownDatatype)) //TODO handle hints
  val varIdTime_decl = IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("varIdTime"))

  // decls for data extents
  val varIdField_decl = dataBuffers.map(buf =>
    IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("varId_"+buf.name)))
  val stride_decl = dataBuffers.map(buf => IR_FileAccess.declareDimensionality(
    IR_ArrayDatatype(ptrDatatype, numDimsDataAndTime(buf.numDimsData)), "stride", buf.localization,
    Some(if (useTimeDim) buf.stride :+ IR_IntegerConstant(1) else buf.stride))) // add one more entry for unlimited "time" dimension
  val count_decl = dataBuffers.map(buf => IR_FileAccess.declareDimensionality(
    IR_ArrayDatatype(MPI_Offset, numDimsDataAndTime(buf.numDimsData)), "count", buf.localization,
    Some(if (useTimeDim) buf.innerDimsLocal :+ IR_IntegerConstant(1) else buf.innerDimsLocal))) // prepend "1" since "1*(count.product)" values are written per timestep
  val emptyCount_decl = dataBuffers.map(buf => IR_FileAccess.declareDimensionality(
    IR_ArrayDatatype(MPI_Offset, numDimsDataAndTime(buf.numDimsData)), "emptyCount", buf.localization,
    Some(ListBuffer.fill(numDimsDataAndTime(buf.numDimsData))(IR_IntegerConstant(0)))))
  val imap_decl = dataBuffers.map(buf => IR_FileAccess.declareDimensionality( // describes in-memory access pattern
    IR_ArrayDatatype(ptrDatatype, numDimsDataAndTime(buf.numDimsData)),"imap", buf.localization,
    Some(if (useTimeDim) buf.imap :+ IR_IntegerConstant(0) else buf.imap))) // prepend "0" since the memory layout doesn't change with the additional time dimension
  val globalDims_decl = dataBuffers.map(buf => IR_FileAccess.declareDimensionality(
    IR_ArrayDatatype(MPI_Offset, buf.numDimsData), "globalDims", buf.localization,
    Some(buf.innerDimsGlobal)))
  val globalStart_decl = dataBuffers.map(buf => IR_FileAccess.declareDimensionality(
    IR_ArrayDatatype(MPI_Offset, numDimsDataAndTime(buf.numDimsData)), "globalStart", buf.localization,
    Some(ListBuffer.fill(numDimsDataAndTime(buf.numDimsData))(IR_IntegerConstant(0)))))

  var declarations : ListBuffer[IR_VariableDeclaration] = ListBuffer(err_decl, ncFile_decl)

  // declarations per databuffer
  declarations ++= varIdField_decl
  declarations ++= stride_decl.distinct
  declarations ++= count_decl.distinct
  declarations ++= imap_decl.distinct
  declarations ++= globalDims_decl.distinct
  declarations ++= globalStart_decl.distinct

  // add declarations which are only used in a parallel application
  if(Knowledge.mpi_enabled) {
    declarations += info_decl
  }

  // declarations when using record variables
  if(useTimeDim) {
    declarations += timeStep_decl // TODO
    declarations += indexTimeArray_decl // TODO
    declarations += varIdTime_decl
  }

  // declarations for collective I/O
  if(Knowledge.parIO_useCollectiveIO) {
    declarations ++= emptyCount_decl.distinct
  }

  // accesses
  val err = IR_VariableAccess(err_decl)
  val ncFile = IR_VariableAccess(ncFile_decl)
  val info = IR_VariableAccess(info_decl)
  val varIdTime = IR_VariableAccess(varIdTime_decl)
  val varIdField = (buf : IR_DataBuffer) => IR_VariableAccess(varIdField_decl(dataBuffers.indexOf(buf)))
  val stride = (buf : IR_DataBuffer) => IR_VariableAccess(stride_decl(dataBuffers.indexOf(buf)))
  val count = (buf : IR_DataBuffer) => IR_VariableAccess(count_decl(dataBuffers.indexOf(buf)))
  val emptyCount = (buf : IR_DataBuffer) => IR_VariableAccess(emptyCount_decl(dataBuffers.indexOf(buf)))
  val imap = (buf : IR_DataBuffer) => IR_VariableAccess(imap_decl(dataBuffers.indexOf(buf)))
  val globalDims = (buf : IR_DataBuffer) => IR_VariableAccess(globalDims_decl(dataBuffers.indexOf(buf)))
  val globalStart = (buf : IR_DataBuffer) => IR_VariableAccess(globalStart_decl(dataBuffers.indexOf(buf)))

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
  def apiTypename(datatype: IR_Datatype) : String = {
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

  def createDimName(buf : IR_DataBuffer, d : Int) = IR_StringConstant(IR_FileAccess.declareVariable(buf.name + "_" + "dim" + (buf.numDimsData - d))) // TODO

  // calls a function from the PnetCDF library and checks if an error occured (i.e. err was set to a value < 0) when debug statements are enabled
  def callNcFunction(funcName : String, datatype : Option[IR_Datatype], args : IR_Expression*) : ListBuffer[IR_Statement] = {
    // handle serial/parallel API differences
    val nonFlexibleFuncName = funcName.replaceFirst("<type>", if (datatype.isDefined) apiTypename(datatype.get) else "")
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

  def accessFileFragwise(buffer : IR_DataBuffer, accessStmts : ListBuffer[IR_Statement]) : IR_LoopOverFragments = {
    // set global starting index for fragment
    val setOffsetFrag : ListBuffer[IR_Statement] = buffer.numDimsDataRange.map(d => {
      IR_Assignment(IR_ArrayAccess(globalStart(buffer), d + (if (useTimeDim) 1 else 0)), buffer.startIndexGlobal.reverse(d)) : IR_Statement
    }).to[ListBuffer]

    if (Knowledge.parIO_useCollectiveIO) {
      val condSetOffset = IR_IfCondition(IR_IV_IsValidForDomain(buffer.domainIdx), setOffsetFrag)
      IR_LoopOverFragments(
        condSetOffset +: accessStmts
      )
    } else {
      IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(buffer.domainIdx),
            setOffsetFrag ++ accessStmts
        )
      )
    }
  }

  override def createOrOpenFile() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // add declarations
    for (decl <- declarations) {
      statements += decl
    }

    // create/open file
    if (writeAccess && !appendedMode) {
      if (Knowledge.mpi_enabled) {
        statements ++= callNcFunction("ncmpi_create", datatype = None, mpiCommunicator, fileName, openMode, info, IR_AddressOf(ncFile))
      } else {
        statements ++= callNcFunction("nc_create", datatype = None, fileName, openMode, IR_AddressOf(ncFile))
      }
    } else {
      if (Knowledge.mpi_enabled) {
        statements ++= callNcFunction("ncmpi_open", datatype = None, mpiCommunicator, fileName, openMode, info, IR_AddressOf(ncFile))
      } else {
        statements ++= callNcFunction("nc_open", datatype = None, fileName, openMode, IR_AddressOf(ncFile))
      }
    }

    statements
  }

  override def setupAccess() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    var distinctDimIds : mutable.HashMap[String, IR_VariableAccess] = mutable.HashMap()
    val dimIdTime = IR_VariableAccess("dimIdTime", IR_IntegerDatatype) // "time" is the slowest-varying dimension ("0" in KJI order)

    // define dimensions in case of write operations
    if (writeAccess) {
      // define unlimited time dimension
      if (useTimeDim) {
        statements += IR_VariableDeclaration(dimIdTime)
        statements ++= callNcFunction("ncmpi_def_dim", datatype = None,
          ncFile, IR_StringConstant("time"),
          IR_VariableAccess("NC_UNLIMITED", IR_UnknownDatatype), IR_AddressOf(dimIdTime))
      }

      // spatial dimensions
      for (buf <- dataBuffers) {
        val key = globalDims(buf).name

        // lookup cache to prevent the definition of dimensions with identical extents
        if (!distinctDimIds.contains(key)) {
          val dimId = IR_VariableAccess(IR_FileAccess.declareVariable("dimId"), IR_ArrayDatatype(IR_IntegerDatatype, buf.numDimsData))
          statements += IR_VariableDeclaration(dimId)

          for (d <- 0 until buf.numDimsData) {
            statements ++= callNcFunction("ncmpi_def_dim", datatype = None,
              ncFile, createDimName(buf, d),
              IR_ArrayAccess(globalDims(buf), d), IR_AddressOf(IR_ArrayAccess(dimId, d)))
          }

          distinctDimIds.put(key, dimId)
        }
      }
    }

    // define(write) or inquire(read) variables for each buffer and the time
    if (writeAccess) {
      if (useTimeDim) {
        statements ++= callNcFunction(
          "ncmpi_def_var", datatype = None,
          ncFile, IR_StringConstant("time"),
          IR_VariableAccess("NC_DOUBLE", IR_UnknownDatatype), IR_IntegerConstant(1), IR_AddressOf(dimIdTime), IR_AddressOf(varIdTime))
      }
      for (buf <- dataBuffers) {
        val dimIdSpatial = distinctDimIds(globalDims(buf).name)
        val dimIdRecord = IR_VariableAccess(IR_FileAccess.declareVariable("dimId" + buf.name), IR_ArrayDatatype(IR_IntegerDatatype, numDimsDataAndTime(buf.numDimsData)))

        // pass array with spatial and temporal dimension ids for record variables
        if (useTimeDim)
          statements += IR_VariableDeclaration(dimIdRecord, IR_InitializerList(dimIdTime +: buf.numDimsDataRange.map(d => IR_ArrayAccess(dimIdSpatial, d)) : _*))

        statements ++= callNcFunction("ncmpi_def_var", datatype = None,
          ncFile, buf.datasetName,
          ncDatatype(buf.datatype), numDimsDataAndTime(buf.numDimsData) : IR_Expression, if (useTimeDim) dimIdRecord else dimIdSpatial, IR_AddressOf(varIdField(buf)))
      }
    } else {
      //statements ++= callNcFunction("ncmpi_inq_varid", ncFile, IR_StringConstant("time"), IR_AddressOf(varIdTime))
      for (buf <- dataBuffers)
        statements ++= callNcFunction("ncmpi_inq_varid", datatype = None,
          ncFile, buf.datasetName, IR_AddressOf(varIdField(buf)))
    }

    // end "define mode" when writing the file
    if (writeAccess)
      statements ++= callNcFunction("ncmpi_enddef", datatype = None, ncFile)

    // go into "data mode"
    if (!Knowledge.parIO_useCollectiveIO && Knowledge.mpi_enabled) {
      // start individual I/O
      statements ++= callNcFunction("ncmpi_begin_indep_data", datatype = None, ncFile)
    }

    statements
  }

  val mpiDatatypeBuffer = (buf : IR_DataBuffer) => IR_VariableAccess(buf.datatype.resolveBaseDatatype.prettyprint_mpi, IR_UnknownDatatype)

  // set count to "0" for "invalid" frags in order to perform a NOP read/write
  val countSelection_decl = dataBuffers.map(buf => IR_VariableDeclaration(IR_PointerDatatype(MPI_Offset), IR_FileAccess.declareVariable("countSelection"), count(buf)))
  val countSelection = (buf : IR_DataBuffer) => IR_VariableAccess(countSelection_decl(dataBuffers.indexOf(buf)))
  def condAssignCount(buf : IR_DataBuffer) : ListBuffer[IR_Statement] = ListBuffer(
    countSelection_decl(dataBuffers.indexOf(buf)),
    IR_IfCondition(IR_Negation(IR_IV_IsValidForDomain(buf.domainIdx)),
      ListBuffer[IR_Statement](
        IR_Assignment(count(buf), emptyCount(buf))
      )
    )
  )

  override def write(buffer : IR_DataBuffer) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // write time values
    if (useTimeDim) {
      if (Knowledge.parIO_useCollectiveIO)
        statements ++= callNcFunction("ncmpi_put_var1_double_all", datatype = None,
          ncFile, varIdTime, IR_AddressOf(IR_VariableAccess(indexTimeArray_decl)), IR_AddressOf(IR_VariableAccess(timeStep_decl)))
      else
        statements += IR_IfCondition(MPI_IsRootProc.apply(), // only root needs to write the time value for independent I/O
          callNcFunction("ncmpi_put_var1_double", datatype = None,
            ncFile, varIdTime, IR_AddressOf(IR_VariableAccess(indexTimeArray_decl)), IR_AddressOf(IR_VariableAccess(timeStep_decl))))
    }

    // write dataset
    val write = IR_IfCondition(buffer.accessWithoutExclusion,
      /* truebody */
      // use simpler method if the whole array can be written without having to exclude ghost layers
      if (Knowledge.parIO_useCollectiveIO) {
        condAssignCount(buffer) ++
          callNcFunction("ncmpi_put_vara_<type>_all", Some(buffer.datatype),
            ncFile, varIdField(buffer), globalStart(buffer), countSelection(buffer), buffer.getBaseAddress)
      } else {
        callNcFunction("ncmpi_put_vara_<type>", Some(buffer.datatype),
          ncFile, varIdField(buffer), globalStart(buffer), count(buffer), buffer.getBaseAddress)
      },
      /* falsebody */
      // more complex method that describes the memory layout (imap parameter with linearized distance between two values (e.g. in x-direction "1") for each dimension in KJI order)
      // needs pointer at offset of first non-ghost index
      if (Knowledge.parIO_useCollectiveIO) {
        condAssignCount(buffer) ++
          callNcFunction("ncmpi_put_varm_<type>_all", Some(buffer.datatype),
            ncFile, varIdField(buffer), globalStart(buffer), countSelection(buffer), stride(buffer), imap(buffer), buffer.getAddressReferenceOffset)
      } else {
        callNcFunction("ncmpi_put_varm_<type>", Some(buffer.datatype),
          ncFile, varIdField(buffer), globalStart(buffer), count(buffer), stride(buffer), imap(buffer), buffer.getAddressReferenceOffset)
      }
    )

    statements += accessFileFragwise(buffer, ListBuffer(write))

    statements
  }

  override def read(buffer : IR_DataBuffer) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // read dataset
    val read = IR_IfCondition(buffer.accessWithoutExclusion,
      /* truebody */
      // use simpler method if the whole array can be written without having to exclude ghost layers
      if (Knowledge.parIO_useCollectiveIO) {
        condAssignCount(buffer) ++
          callNcFunction(
          "ncmpi_get_vara_<type>_all", Some(buffer.datatype),
            ncFile, varIdField(buffer), globalStart(buffer), countSelection(buffer), buffer.getBaseAddress)
      } else {
        callNcFunction("ncmpi_get_vara_<type>", Some(buffer.datatype),
          ncFile, varIdField(buffer), globalStart(buffer), count(buffer), buffer.getBaseAddress)
      },
      /* falsebody */
      // more complex method that describes the memory layout (i.e. linearized offsets for each dimension in KJI order)
      // needs pointer at offset of first non-ghost index
      if (Knowledge.parIO_useCollectiveIO) {
        condAssignCount(buffer) ++
          callNcFunction(
          "ncmpi_get_varm_<type>_all", Some(buffer.datatype),
            ncFile, varIdField(buffer), globalStart(buffer), countSelection(buffer), stride(buffer), imap(buffer), buffer.getAddressReferenceOffset)
      } else {
        callNcFunction("ncmpi_get_varm_<type>", Some(buffer.datatype),
          ncFile, varIdField(buffer), globalStart(buffer), count(buffer), stride(buffer), imap(buffer), buffer.getAddressReferenceOffset)
      }
    )

    statements += accessFileFragwise(buffer, ListBuffer(read))

    statements
  }

  override def cleanupAccess() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    if (!Knowledge.parIO_useCollectiveIO && Knowledge.mpi_enabled) {
      statements ++= callNcFunction("ncmpi_end_indep_data", None, ncFile)
    }
    statements
  }

  override def closeFile() : ListBuffer[IR_Statement] = callNcFunction("ncmpi_close", None, ncFile)

  // headers, paths and libs
  override def includes : ListBuffer[String] = ListBuffer(if(Knowledge.mpi_enabled) "pnetcdf.h" else "netcdf.h")
  override def libraries : ListBuffer[String] = ListBuffer(if(Knowledge.mpi_enabled) "pnetcdf" else "netcdf")
  override def pathsInc : ListBuffer[String] = ListBuffer("$(PNETCDF_HOME)/include")
  override def pathsLib : ListBuffer[String] = ListBuffer("$(PNETCDF_HOME)/lib")

  override def validateParams() : Unit = {
    for (buf <- dataBuffers)
      if (buf.datasetName == IR_NullExpression)
        Logger.error("Parameter \"dataset\" was not specified.")

    if(!Knowledge.mpi_enabled) {
      Knowledge.parIO_useCollectiveIO = false // no collective I/O handling for serial programs
    }
  }
}
