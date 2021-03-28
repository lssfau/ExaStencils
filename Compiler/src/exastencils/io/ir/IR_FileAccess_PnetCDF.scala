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
    var filename : IR_Expression,
    var dataBuffers : ListBuffer[IR_DataBuffer],
    var bufferIsRecordVariable : Option[mutable.HashMap[Int, Boolean]],
    var writeAccess : Boolean,
    var appendedMode : Boolean = false, // TODO: additional param "timeIndex" for appended mode?
    var initFragInfo : Boolean = true
) extends IR_FileAccess("nc") with IR_PnetCDF_API {

  /* hints:
    1. nc_header_align_size: Default size if 512 Byte. In case that an exisiting file is re-opened and more metadata is added, the file header expands and actual data is moved to higher file offsets
  */

  // general variables
  override def fileMode : IR_VariableAccess = if (writeAccess) {
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

  /* time variable handling */
  // datasets can be specified as "fixed-size" or "record" variables
  // fixed-size arrays only consist of spatial dimensions
  // record variables are additionally bound to a "unlimited" dimension (mostly for time)
  def truthTableRecordVariables : mutable.HashMap[Int, Boolean] = bufferIsRecordVariable getOrElse mutable.HashMap()
  def fileContainsRecordVariables : Boolean = truthTableRecordVariables.exists(_._2 == true)
  def useTimeDim(bufIdx : Int) : Boolean = truthTableRecordVariables.getOrElse(bufIdx, false)
  def numDimsDataAndTime(bufIdx : Int, numDimsData : Int) : Int = numDimsData + (if (useTimeDim(bufIdx)) 1 else 0)
  def writeTimeToFile : Boolean = writeAccess && fileContainsRecordVariables && Knowledge.parIO_vis_constantDataReduction
  def handleTimeDimension(timeValue : IR_Expression, bufIdx : Int, dims : ListBuffer[IR_Expression]) : ListBuffer[IR_Expression] = {
    if (useTimeDim(bufIdx)) timeValue +: dims else dims // prepend one more entry for unlimited "time" dimension
  }

  // decls
  val ncFile_decl = IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("ncFile"))
  val varIdTime_decl = IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("varIdTime"))
  val varIdBuffer_decl : Array[IR_VariableDeclaration] = dataBuffers.map(buf =>
    IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("varId_"+buf.name))).toArray

  // decls for data extents
  val ptrDatatype : IR_SpecialDatatype = if (Knowledge.mpi_enabled) MPI_Offset else IR_SpecialDatatype("ptrdiff_t") // serial API uses "ptrdiff_t" for "imap" and "stride" parameters
  override def stride_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.zipWithIndex.map { case (buf, bufIdx) =>
    buf.declareDimensionality("stride", ptrDatatype,
       handleTimeDimension(timeValue = 1, bufIdx, // prepend one more entry for unlimited "time" dimension
         dims = IR_DataBuffer.handleFragmentDimension(buf.canonicalOrder, buf.accessBlockwise, buf.strideKJI, fragmentDim = 1)))
  }
  override def count_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.zipWithIndex.map { case (buf, bufIdx) =>
    buf.declareDimensionality("count", datatypeDimArray,
      handleTimeDimension(timeValue = 1, bufIdx, // prepend "1" since "1*(count.product)" values are written per timestep
        dims = IR_DataBuffer.handleFragmentDimension(buf.canonicalOrder, buf.accessBlockwise, buf.innerDimsLocalKJI, fragmentDim = if (buf.accessBlockwise) IR_IV_NumValidFrags(buf.domainIdx) else 1)))
  }
  override def globalStart_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.zipWithIndex.map { case (buf, bufIdx) =>
    buf.declareDimensionality("globalStart", datatypeDimArray,
      handleTimeDimension(timeValue = IR_IV_TimeIndexRecordVariables(), bufIdx,
        dims = ListBuffer.fill(buf.datasetDimsGlobal)(IR_IntegerConstant(0))))
  }
  lazy val emptyCount_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.zipWithIndex.map { case (buf, bufIdx) =>
    buf.declareDimensionality("emptyCount", datatypeDimArray,
      ListBuffer.fill(numDimsDataAndTime(bufIdx, buf.datasetDimsGlobal))(IR_IntegerConstant(0)))
  }
  val imap_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.zipWithIndex.map { case (buf, bufIdx) => // describes in-memory access pattern
    buf.declareDimensionality("imap", ptrDatatype,
      handleTimeDimension(timeValue = 0, bufIdx, // prepend "0" since the memory layout doesn't change with the additional time dimension
        dims = IR_DataBuffer.handleFragmentDimension(buf.canonicalOrder, buf.accessBlockwise, buf.imapKJI, buf.imapKJI.head))) // for fragment-wise storage of fields: repeat imap entry for slowest-varying dim to match to the dimensionality of count, stride, etc.
  }

  var declarations : ListBuffer[IR_VariableDeclaration] = ListBuffer(err_decl, ncFile_decl) // file handle and error variable for debugging

  // declarations per databuffer
  declarations ++= varIdBuffer_decl
  declarations ++= imap_decl.distinct

  // declarations when using record variables
  if (fileContainsRecordVariables) {
    declarations += varIdTime_decl
  }

  // declarations for collective I/O
  if (Knowledge.parIO_useCollectiveIO) {
    declarations ++= emptyCount_decl.distinct
  }

  // accesses
  val ncFile = IR_VariableAccess(ncFile_decl)
  val info = PnetCDF_Info()
  val varIdTime = IR_VariableAccess(varIdTime_decl)
  lazy val varIdBuffer : Array[IR_VariableAccess] = dataBuffers.indices.map(bufIdx => IR_VariableAccess(varIdBuffer_decl(bufIdx))).toArray
  lazy val emptyCount : Array[IR_VariableAccess] = dataBuffers.indices.map(bufIdx => IR_VariableAccess(emptyCount_decl(bufIdx))).toArray
  lazy val localView : Array[MPI_View] = dataBuffers.indices.map(bufIdx => MPI_View.getView(bufIdx, global = false)).toArray
  lazy val imap : Array[IR_VariableAccess] = dataBuffers.indices.map(bufIdx => IR_VariableAccess(imap_decl(bufIdx))).toArray

  def createDimName(buf : IR_DataBuffer, d : Int) = IR_StringConstant(IR_FileAccess.declareVariable(buf.name + "_" + "dim" + (buf.numDimsData - d))) // TODO better name convention

  def accessFileFragwise(bufIdx : Int, accessStmts : ListBuffer[IR_Statement]) : IR_Statement = {
    val buffer = dataBuffers(bufIdx)
    // set global starting index for fragment
    val setOffsetFrag : ListBuffer[IR_Statement] = buffer.startIndexGlobalKJI.indices.map(d => {
      IR_Assignment(IR_ArrayAccess(globalStart(bufIdx), numDimsDataAndTime(bufIdx, d)), buffer.startIndexGlobalKJI(d)) : IR_Statement
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
      IR_Assignment(IR_ArrayAccess(globalStart(bufIdx), numDimsDataAndTime(bufIdx, d)), buffer.startIndexGlobalKJI(d)) : IR_Statement
    }).to[ListBuffer]

    IR_LoopOverBlocks(IR_IfCondition(IR_IV_IsValidForDomain(buffer.domainIdx),
      setOffsetBlock ++ accessStatements
    ))
  }

  override def createOrOpenFile() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // init frag info if it was not already (e.g. in visualization interface)
    if (initFragInfo)
      statements ++= IR_IV_FragmentInfo.init(dataBuffers.head.domainIdx, calculateFragOffset = dataBuffers.exists(!_.canonicalOrder))

    // add declarations
    (dimensionalityDeclarations ++ declarations).foreach(decl => statements += decl)

    // create/open file
    // distinction of serial/parallel interfaces only made for these functions since their signature (serial <-> parallel) differs greatly
    if (writeAccess && !appendedMode) {
      if (Knowledge.mpi_enabled) {
        statements ++= info.setHints()
        statements ++= ncmpi_create(mpiCommunicator, filenameAsCString, fileMode, info, ncFile)
      } else {
        statements ++= nc_create(filenameAsCString, fileMode, ncFile)
      }
    } else {
      if (Knowledge.mpi_enabled) {
        statements ++= info.setHints()
        statements ++= ncmpi_open(mpiCommunicator, filenameAsCString, fileMode, info, ncFile)
      } else {
        statements ++= nc_open(filenameAsCString, fileMode, ncFile)
      }
    }

    statements
  }

  override def setupAccess() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    val distinctDimIds : mutable.HashMap[String, IR_VariableAccess] = mutable.HashMap()
    val dimIdTime = IR_VariableAccess("dimIdTime", IR_IntegerDatatype) // "time" is the slowest-varying dimension ("0" in KJI order)

    // define dimensions in case of write operations
    if (writeAccess && !appendedMode) {
      // define unlimited time dimension
      if (fileContainsRecordVariables) {
        statements += IR_VariableDeclaration(dimIdTime)
        statements ++= ncmpi_def_dim(ncFile, IR_StringConstant("time_whole"), IR_VariableAccess("NC_UNLIMITED", IR_UnknownDatatype), IR_AddressOf(dimIdTime))
      }

      // spatial dimensions
      for (bufIdx <- dataBuffers.indices) {
        val key = globalDims(bufIdx).name

        // lookup cache to prevent the definition of dimensions with identical extents
        if (!distinctDimIds.contains(key)) {
          val buf = dataBuffers(bufIdx)
          val dimId = IR_VariableAccess(IR_FileAccess.declareVariable("dimId"), IR_ArrayDatatype(IR_IntegerDatatype, buf.datasetDimsGlobal))
          statements += IR_VariableDeclaration(dimId)

          for (d <- 0 until buf.datasetDimsGlobal) {
            statements ++= ncmpi_def_dim(ncFile, createDimName(dataBuffers(bufIdx), d), IR_ArrayAccess(globalDims(bufIdx), d), IR_AddressOf(IR_ArrayAccess(dimId, d)))
          }

          distinctDimIds.put(key, dimId)
        }
      }
    }

    // define or inquire variables for each buffer and the time
    if (writeAccess && !appendedMode) {
      // define
      if (fileContainsRecordVariables) {
        statements ++= ncmpi_def_var(ncFile, IR_StringConstant("time_whole"), IR_DoubleDatatype, 1, IR_AddressOf(dimIdTime), IR_AddressOf(varIdTime))
      }
      for (bufIdx <- dataBuffers.indices) {
        val buf = dataBuffers(bufIdx)
        val dimIdSpatial = distinctDimIds(globalDims(bufIdx).name)
        val numDims = buf.datasetDimsGlobal
        val dimIdRecord = IR_VariableAccess(IR_FileAccess.declareVariable("dimId" + buf.name), IR_ArrayDatatype(IR_IntegerDatatype, numDimsDataAndTime(bufIdx, numDims)))

        // pass array with spatial and temporal dimension ids for record variables
        if (useTimeDim(bufIdx))
          statements += IR_VariableDeclaration(dimIdRecord, IR_InitializerList(dimIdTime +: (0 until numDims).map(d => IR_ArrayAccess(dimIdSpatial, d)) : _*))

        statements ++= ncmpi_def_var(ncFile, buf.datasetName, buf.datatype,
          numDimsDataAndTime(bufIdx, numDims), if (useTimeDim(bufIdx)) dimIdRecord else dimIdSpatial, IR_AddressOf(varIdBuffer(bufIdx)))
      }
    } else {
      // inquire
      if (fileContainsRecordVariables) {
        statements ++= ncmpi_inq_varid(ncFile, IR_StringConstant("time_whole"), IR_AddressOf(varIdTime))
      }
      for (bufIdx <- dataBuffers.indices) {
        statements ++= ncmpi_inq_varid(ncFile, dataBuffers(bufIdx).datasetName, IR_AddressOf(varIdBuffer(bufIdx)))
      }
    }

    // create derived data types for memory access (omit ghost layers)
    if (Knowledge.mpi_enabled) {
      for (bufIdx <- dataBuffers.indices) {
        val buf = dataBuffers(bufIdx)
        val numDims = buf.totalDimsLocalKJI.length
        val total = buf.declareDimensionality("localDimsTotal", IR_IntegerDatatype, buf.totalDimsLocalKJI)
        val count = buf.declareDimensionality("localCount", IR_IntegerDatatype, buf.innerDimsLocalKJI)
        val start = buf.declareDimensionality("localStart", IR_IntegerDatatype, buf.startIndexLocalKJI)

        val localView = MPI_View(IR_VariableAccess(total), IR_VariableAccess(count), IR_VariableAccess(start),
          numDims, createViewPerFragment = false, isLocal = true, buf, "localSubarray")
        if (MPI_View.addView(localView)) {
          statements += IR_IfCondition(IR_Negation(buf.accessWithoutExclusion),
            ListBuffer[IR_Statement](
              total, count, start,
              localView.createDatatype))
        }
      }
    }

    // end "define mode" when writing the file and go into "data mode"
    if (writeAccess && !appendedMode)
      statements ++= ncmpi_enddef(ncFile)

    if (!Knowledge.parIO_useCollectiveIO && Knowledge.mpi_enabled) {
      // start individual I/O
      statements ++= ncmpi_begin_indep_data(ncFile)
    }

    // write time values
    if (writeTimeToFile) {
      if (Knowledge.parIO_useCollectiveIO) {
        statements ++= ncmpi_put_var1_type_all(IR_DoubleDatatype, ncFile, varIdTime, IR_AddressOf(IR_IV_TimeIndexRecordVariables()), IR_AddressOf(IR_IV_TimeValueRecordVariables()))
      } else {
        statements += IR_IfCondition(MPI_IsRootProc.apply(), // only root needs to write the time value for independent I/O
          ncmpi_put_var1_type(IR_DoubleDatatype, ncFile, varIdTime, IR_AddressOf(IR_IV_TimeIndexRecordVariables()), IR_AddressOf(IR_IV_TimeValueRecordVariables())))
      }
    }

    statements
  }

  // set count to "0" for "invalid" frags in order to perform a NOP read/write
  lazy val countSelection_decl : Array[IR_VariableDeclaration] = dataBuffers.indices.map(bufIdx =>
    IR_VariableDeclaration(IR_PointerDatatype(MPI_Offset), IR_FileAccess.declareVariable("countSelection"), count(bufIdx))).toArray
  lazy val countSelection : Array[IR_VariableAccess] = dataBuffers.indices.map(bufIdx =>
    IR_VariableAccess(countSelection_decl(bufIdx))).toArray
  val bufcount = IR_VariableAccess("bufcount", MPI_Offset)
  val bufcount_decl = IR_VariableDeclaration(bufcount, 1)
  def condAssignCount(bufIdx : Int,
      altCountSelection : Option[IR_VariableDeclaration] = None,
      altEmptyCount : Option[IR_VariableAccess] = None) : ListBuffer[IR_Statement] = {

    var stmts : ListBuffer[IR_Statement] = ListBuffer()
    val countSel_decl = altCountSelection getOrElse countSelection_decl(bufIdx)
    stmts += countSel_decl
    stmts += bufcount_decl

    if (!dataBuffers(bufIdx).accessBlockwise) {
      stmts += IR_IfCondition(IR_Negation(IR_IV_IsValidForDomain(dataBuffers(bufIdx).domainIdx)),
        ListBuffer[IR_Statement](
          IR_Assignment(bufcount, 0),
          IR_Assignment(IR_VariableAccess(countSel_decl), altEmptyCount getOrElse emptyCount(bufIdx))))
    }

    stmts
  }

  override def write(bufIdx : Int) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    val buffer = dataBuffers(bufIdx)

    // write dataset
    val write = IR_IfCondition(dataBuffers(bufIdx).accessWithoutExclusion,
      /* truebody */
      // use simpler method if the whole array can be written without having to exclude ghost layers
      if (Knowledge.parIO_useCollectiveIO) {
        condAssignCount(bufIdx) ++
          ncmpi_put_vara_type_all(buffer.datatype, ncFile, varIdBuffer(bufIdx), globalStart(bufIdx), countSelection(bufIdx), buffer.getBaseAddress)
      } else {
        ncmpi_put_vara_type(buffer.datatype, ncFile, varIdBuffer(bufIdx), globalStart(bufIdx), count(bufIdx), buffer.getBaseAddress)
      },
      /* falsebody */
      if (Knowledge.mpi_enabled) {
        // parallel application: use MPI derived datatype to skip (internal) intermediate buffer from "ncmpi_put_varm_type"
        if (Knowledge.parIO_useCollectiveIO) {
          condAssignCount(bufIdx) ++
            ncmpi_put_vara_all(ncFile, varIdBuffer(bufIdx), globalStart(bufIdx), countSelection(bufIdx), buffer.getBaseAddress, bufcount, localView(bufIdx).getAccess)
        } else {
          ncmpi_put_vara(ncFile, varIdBuffer(bufIdx), globalStart(bufIdx), count(bufIdx), buffer.getBaseAddress, 1, localView(bufIdx).getAccess)
        }
      } else {
        // serial: no MPI data types to describe in-memory layout -> use "nc_put_varm_<type>" instead
        ncmpi_put_varm_type(buffer.datatype, ncFile, varIdBuffer(bufIdx), globalStart(bufIdx), count(bufIdx), stride(bufIdx), imap(bufIdx), buffer.getAddressReferenceOffset)
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
          ncmpi_get_vara_type_all(buffer.datatype, ncFile, varIdBuffer(bufIdx), globalStart(bufIdx), countSelection(bufIdx), buffer.getBaseAddress)
      } else {
        ncmpi_get_vara_type(buffer.datatype, ncFile, varIdBuffer(bufIdx), globalStart(bufIdx), count(bufIdx), buffer.getBaseAddress)
      },
      /* falsebody */
      if (Knowledge.mpi_enabled) {
        // parallel application: use MPI derived datatype
        if (Knowledge.parIO_useCollectiveIO) {
          condAssignCount(bufIdx) ++
            ncmpi_get_vara_all(ncFile, varIdBuffer(bufIdx), globalStart(bufIdx), countSelection(bufIdx), buffer.getBaseAddress, bufcount, localView(bufIdx).getAccess)
        } else {
          ncmpi_get_vara(ncFile, varIdBuffer(bufIdx), globalStart(bufIdx), count(bufIdx), buffer.getBaseAddress, 1, localView(bufIdx).getAccess)
        }
      } else {
        // serial: no MPI data types to describe in-memory layout -> use "nc_get_varm_<type>" instead
        ncmpi_get_varm_type(buffer.datatype, ncFile, varIdBuffer(bufIdx), globalStart(bufIdx), count(bufIdx), stride(bufIdx), imap(bufIdx), buffer.getAddressReferenceOffset)
      }
    )

    statements += accessFileWithGranularity(bufIdx, ListBuffer(read))

    statements
  }

  override def cleanupAccess() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    if (Knowledge.mpi_enabled)
      MPI_View.resetViews()

    // increment time index/value once data was written
    if (writeTimeToFile) {
      statements += IR_Assignment(IR_IV_TimeIndexRecordVariables(), 1, "+=")
      statements += IR_Assignment(IR_IV_TimeValueRecordVariables(), 1.0, "+=")
    }

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
    for (buf <- dataBuffers) {
      if (buf.datasetName == IR_NullExpression)
        Logger.error("Parameter \"dataset\" was not specified.")

      if (Knowledge.mpi_enabled && !Knowledge.parIO_useCollectiveIO && buf.canonicalOrder)
        Logger.error("Parameter \"canonicalOrder\" should only be used with collective I/O.")
    }

    if(!Knowledge.mpi_enabled) {
      Knowledge.parIO_useCollectiveIO = false // no collective I/O handling for serial programs
    }
  }
}
