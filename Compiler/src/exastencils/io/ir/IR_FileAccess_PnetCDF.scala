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
import exastencils.parallelization.api.mpi.MPI_IsRootProc

case class IR_FileAccess_PnetCDF(
    var fileName : IR_Expression,
    var dataBuffers : ListBuffer[IR_DataBuffer],
    var writeAccess : Boolean,
    var appendedMode : Boolean = false) extends IR_FileAccess("nc", fileName, dataBuffers, writeAccess, appendedMode) with IR_PnetCDF_API {

  // TODO: Test handling collective/independent I/O for "invalid" fragments

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
  def numDimsDataAndTime(numDimsData : Int) : Int = numDimsData + (if (useTimeDim) 1 else 0) // record variables are bound to the "unlimited" dimension (often used as time)
  val indexTimeArray_decl = IR_VariableDeclaration(MPI_Offset, IR_FileAccess.declareVariable("printStep"), 0) // TODO make iv
  val timeStep_decl = IR_VariableDeclaration(IR_DoubleDatatype, IR_FileAccess.declareVariable("t"), 0.0) // TODO make iv

  // decls
  val ncFile_decl = IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("ncFile"))
  val info_decl = IR_VariableDeclaration(IR_SpecialDatatype("MPI_Info"), IR_FileAccess.declareVariable("info"), IR_VariableAccess("MPI_INFO_NULL", IR_UnknownDatatype)) //TODO handle hints
  val varIdTime_decl = IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("varIdTime"))
  val varIdField_decl : Array[IR_VariableDeclaration] = dataBuffers.map(buf =>
    IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("varId_"+buf.name))).toArray

  // decls for data extents
  val ptrDatatype : IR_SpecialDatatype = if (Knowledge.mpi_enabled) MPI_Offset else IR_SpecialDatatype("ptrdiff_t") // serial API uses "ptrdiff_t" for "imap" and "stride" parameters
  override def stride_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => {
    declareDimensionality("stride", buf.localization,
      handleFragmentDimension(buf,
        if (useTimeDim) IR_IntegerConstant(1) +: buf.strideKJI else buf.strideKJI,  // prepend one more entry for unlimited "time" dimension
        fragmentDim = 1),
      ptrDatatype)
  })
  override def count_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => {
    declareDimensionality("count", buf.localization,
      handleFragmentDimension(buf,
        if (useTimeDim) IR_IntegerConstant(1) +: buf.innerDimsLocalKJI else buf.innerDimsLocalKJI, // prepend "1" since "1*(count.product)" values are written per timestep
        fragmentDim = if (buf.accessBlockwise) IR_IV_NumValidFrags(buf.domainIdx) else 1))
  })
  override def globalStart_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => {
    declareDimensionality( "globalStart", buf.localization,
      ListBuffer.fill(numDimsDataAndTime(numDimsGlobal(buf)))(IR_IntegerConstant(0)))
  })
  lazy val emptyCount_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => {
    declareDimensionality("emptyCount", buf.localization,
      ListBuffer.fill(numDimsDataAndTime(numDimsGlobal(buf)))(IR_IntegerConstant(0)))
  })
  val imap_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => { // describes in-memory access pattern
    declareDimensionality("imap", buf.localization,
      if (useTimeDim) IR_IntegerConstant(0) +: buf.imapKJI else buf.imapKJI, // prepend "0" since the memory layout doesn't change with the additional time dimension,
      ptrDatatype)
  })

  var declarations : ListBuffer[IR_VariableDeclaration] = dimensionalityDeclarations :+ err_decl :+ ncFile_decl

  // declarations per databuffer
  declarations ++= varIdField_decl
  declarations ++= imap_decl.distinct

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
  val ncFile = IR_VariableAccess(ncFile_decl)
  val info = IR_VariableAccess(info_decl)
  val varIdTime = IR_VariableAccess(varIdTime_decl)
  lazy val varIdField : Array[IR_VariableAccess] = dataBuffers.indices.map(bufIdx => IR_VariableAccess(varIdField_decl(bufIdx))).toArray
  lazy val emptyCount : Array[IR_VariableAccess] = dataBuffers.indices.map(bufIdx => IR_VariableAccess(emptyCount_decl(bufIdx))).toArray
  lazy val imap : Array[IR_VariableAccess] = dataBuffers.indices.map(bufIdx => IR_VariableAccess(imap_decl(bufIdx))).toArray

  def createDimName(buf : IR_DataBuffer, d : Int) = IR_StringConstant(IR_FileAccess.declareVariable(buf.name + "_" + "dim" + (buf.numDimsData - d))) // TODO

  def accessFileFragwise(bufIdx : Int, accessStmts : ListBuffer[IR_Statement]) : IR_LoopOverFragments = {
    val buffer = dataBuffers(bufIdx)
    // set global starting index for fragment
    val setOffsetFrag : ListBuffer[IR_Statement] = buffer.startIndexGlobalKJI.indices.map(d => {
      IR_Assignment(IR_ArrayAccess(globalStart(bufIdx), d + (if (useTimeDim) 1 else 0)), buffer.startIndexGlobalKJI(d)) : IR_Statement
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

  override def accessFileBlockwise(bufIdx : Int, accessStatements : ListBuffer[IR_Statement]) : IR_Statement = {
    val buffer = dataBuffers(bufIdx)
    // set global starting index for block
    val setOffsetBlock : ListBuffer[IR_Statement] = buffer.startIndexGlobalKJI.indices.map(d => {
      IR_Assignment(IR_ArrayAccess(globalStart(bufIdx), d + (if (useTimeDim) 1 else 0)), buffer.startIndexGlobalKJI(d)) : IR_Statement
    }).to[ListBuffer]

    IR_LoopOverBlocks(IR_IfCondition(IR_IV_IsValidForDomain(buffer.domainIdx),
      setOffsetBlock ++ accessStatements
    ))
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
        statements ++= ncmpi_create(mpiCommunicator, filenameAsCString, openMode, info, ncFile)
      } else {
        statements ++= nc_create(filenameAsCString, openMode, ncFile)
      }
    } else {
      if (Knowledge.mpi_enabled) {
        statements ++= ncmpi_open(mpiCommunicator, filenameAsCString, openMode, info, ncFile)
      } else {
        statements ++= nc_open(filenameAsCString, openMode, ncFile)
      }
    }

    statements
  }

  override def setupAccess() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    val distinctDimIds : mutable.HashMap[String, IR_VariableAccess] = mutable.HashMap()
    val dimIdTime = IR_VariableAccess("dimIdTime", IR_IntegerDatatype) // "time" is the slowest-varying dimension ("0" in KJI order)

    // define dimensions in case of write operations
    if (writeAccess) {
      // define unlimited time dimension
      if (useTimeDim) {
        statements += IR_VariableDeclaration(dimIdTime)
        statements ++= ncmpi_def_dim(ncFile, IR_StringConstant("time"), IR_VariableAccess("NC_UNLIMITED", IR_UnknownDatatype), IR_AddressOf(dimIdTime))
      }

      // spatial dimensions
      for (bufIdx <- dataBuffers.indices) {
        val key = globalDims(bufIdx).name

        // lookup cache to prevent the definition of dimensions with identical extents
        if (!distinctDimIds.contains(key)) {
          val dimId = IR_VariableAccess(IR_FileAccess.declareVariable("dimId"), IR_ArrayDatatype(IR_IntegerDatatype, numDimsGlobal(bufIdx)))
          statements += IR_VariableDeclaration(dimId)

          for (d <- 0 until numDimsGlobal(bufIdx)) {
            statements ++= ncmpi_def_dim(ncFile, createDimName(dataBuffers(bufIdx), d), IR_ArrayAccess(globalDims(bufIdx), d), IR_AddressOf(IR_ArrayAccess(dimId, d)))
          }

          distinctDimIds.put(key, dimId)
        }
      }
    }

    // define(write) or inquire(read) variables for each buffer and the time
    if (writeAccess) {
      if (useTimeDim) {
        statements ++= ncmpi_def_var(ncFile, IR_StringConstant("time"), IR_DoubleDatatype, 1, IR_AddressOf(dimIdTime), IR_AddressOf(varIdTime))
      }
      for (bufIdx <- dataBuffers.indices) {
        val buf = dataBuffers(bufIdx)
        val dimIdSpatial = distinctDimIds(globalDims(bufIdx).name)
        val dimIdRecord = IR_VariableAccess(IR_FileAccess.declareVariable("dimId" + buf.name), IR_ArrayDatatype(IR_IntegerDatatype, numDimsDataAndTime(buf.numDimsData)))
        val numDims = numDimsGlobal(bufIdx)

        // pass array with spatial and temporal dimension ids for record variables
        if (useTimeDim)
          statements += IR_VariableDeclaration(dimIdRecord, IR_InitializerList(dimIdTime +: (0 until numDims).map(d => IR_ArrayAccess(dimIdSpatial, d)) : _*))

        statements ++= ncmpi_def_var(ncFile, buf.datasetName, buf.datatype, numDimsDataAndTime(numDims), if (useTimeDim) dimIdRecord else dimIdSpatial, IR_AddressOf(varIdField(bufIdx)))
      }
    } else {
      //statements ++= ncmpi_inq_varid(ncFile, IR_StringConstant("time"), varIdTime)
      for (bufIdx <- dataBuffers.indices)
        statements ++= ncmpi_inq_varid(ncFile, dataBuffers(bufIdx).datasetName, IR_AddressOf(varIdField(bufIdx)))
    }

    // end "define mode" when writing the file
    if (writeAccess)
      statements ++= ncmpi_enddef(ncFile)

    // go into "data mode"
    if (!Knowledge.parIO_useCollectiveIO && Knowledge.mpi_enabled) {
      // start individual I/O
      statements ++= ncmpi_begin_indep_data(ncFile)
    }

    statements
  }

  // set count to "0" for "invalid" frags in order to perform a NOP read/write
  val countSelection_decl : Array[IR_VariableDeclaration] = dataBuffers.indices.map(bufIdx =>
    IR_VariableDeclaration(IR_PointerDatatype(MPI_Offset), IR_FileAccess.declareVariable("countSelection"), count(bufIdx))).toArray
  val countSelection : Array[IR_VariableAccess] = dataBuffers.indices.map(bufIdx =>
    IR_VariableAccess(countSelection_decl(bufIdx))).toArray
  def condAssignCount(bufIdx : Int) : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()
    stmts += countSelection_decl(bufIdx)

    if (!dataBuffers(bufIdx).accessBlockwise) {
      stmts += IR_IfCondition(IR_Negation(IR_IV_IsValidForDomain(dataBuffers(bufIdx).domainIdx)),
        ListBuffer[IR_Statement](
          IR_Assignment(countSelection(bufIdx), emptyCount(bufIdx))))
    }

    stmts
  }

  override def write(bufIdx : Int) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    val buffer = dataBuffers(bufIdx)

    // write time values
    if (useTimeDim) {
      if (Knowledge.parIO_useCollectiveIO)
        statements ++= ncmpi_put_var1_type_all(IR_DoubleDatatype, ncFile, varIdTime, IR_AddressOf(IR_VariableAccess(indexTimeArray_decl)), IR_AddressOf(IR_VariableAccess(timeStep_decl)))
      else
        statements += IR_IfCondition(MPI_IsRootProc.apply(), // only root needs to write the time value for independent I/O
          ncmpi_put_var1_type(IR_DoubleDatatype, ncFile, varIdTime, IR_AddressOf(IR_VariableAccess(indexTimeArray_decl)), IR_AddressOf(IR_VariableAccess(timeStep_decl))))
    }

    // write dataset
    val write = IR_IfCondition(dataBuffers(bufIdx).accessWithoutExclusion,
      /* truebody */
      // use simpler method if the whole array can be written without having to exclude ghost layers
      if (Knowledge.parIO_useCollectiveIO) {
        condAssignCount(bufIdx) ++
          ncmpi_put_vara_type_all(buffer.datatype, ncFile, varIdField(bufIdx), globalStart(bufIdx), countSelection(bufIdx), buffer.getBaseAddress)
      } else {
        ncmpi_put_vara_type(buffer.datatype, ncFile, varIdField(bufIdx), globalStart(bufIdx), count(bufIdx), buffer.getBaseAddress)
      },
      /* falsebody */
      // more complex method that describes the memory layout (imap parameter with linearized distance between two values (e.g. in x-direction "1") for each dimension in KJI order)
      // needs pointer at offset of first non-ghost index
      if (Knowledge.parIO_useCollectiveIO) {
        condAssignCount(bufIdx) ++
          ncmpi_put_varm_type_all(buffer.datatype, ncFile, varIdField(bufIdx), globalStart(bufIdx), countSelection(bufIdx), stride(bufIdx), imap(bufIdx), buffer.getAddressReferenceOffset)
      } else {
        ncmpi_put_varm_type(buffer.datatype, ncFile, varIdField(bufIdx), globalStart(bufIdx), count(bufIdx), stride(bufIdx), imap(bufIdx), buffer.getAddressReferenceOffset)
      }
    )

    statements += accessFileWithGranularity(bufIdx, ListBuffer(write))

    statements
  }

  override def read(bufIdx : Int) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    val buffer = dataBuffers(bufIdx)

    // read dataset
    val read = IR_IfCondition(buffer.accessWithoutExclusion,
      /* truebody */
      // use simpler method if the whole array can be written without having to exclude ghost layers
      if (Knowledge.parIO_useCollectiveIO) {
        condAssignCount(bufIdx) ++
          ncmpi_get_vara_type_all(buffer.datatype, ncFile, varIdField(bufIdx), globalStart(bufIdx), countSelection(bufIdx), buffer.getBaseAddress)
      } else {
        ncmpi_get_vara_type(buffer.datatype, ncFile, varIdField(bufIdx), globalStart(bufIdx), count(bufIdx), buffer.getBaseAddress)
      },
      /* falsebody */
      // more complex method that describes the memory layout (i.e. linearized offsets for each dimension in KJI order)
      // needs pointer at offset of first non-ghost index
      if (Knowledge.parIO_useCollectiveIO) {
        condAssignCount(bufIdx) ++
          ncmpi_get_varm_type_all(buffer.datatype, ncFile, varIdField(bufIdx), globalStart(bufIdx), countSelection(bufIdx), stride(bufIdx), imap(bufIdx), buffer.getAddressReferenceOffset)
      } else {
        ncmpi_get_varm_type(buffer.datatype, ncFile, varIdField(bufIdx), globalStart(bufIdx), count(bufIdx), stride(bufIdx), imap(bufIdx), buffer.getAddressReferenceOffset)
      }
    )

    statements += accessFileWithGranularity(bufIdx, ListBuffer(read))

    statements
  }

  override def cleanupAccess() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    if (!Knowledge.parIO_useCollectiveIO && Knowledge.mpi_enabled) {
      statements ++= ncmpi_end_indep_data(ncFile)
    }
    statements
  }

  override def closeFile() : ListBuffer[IR_Statement] = ncmpi_close(ncFile)

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
