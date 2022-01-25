package exastencils.field.ir

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.config.Settings
import exastencils.datastructures.Transformation.OutputType
import exastencils.domain.ir.IR_IV_FragmentIndex
import exastencils.grid.ir._
import exastencils.io.ir._
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi.MPI_IsRootProc
import exastencils.visualization.ir.IR_PrintVisualization

/// IR_PrintNetCDF
// follows CF conventions to allow visualization with Paraview/VisIt
// only for rectilinear meshes and domains

case class IR_PrintNetCDF(
    var filename : IR_Expression,
    var field : IR_Field,
    var slot : IR_Expression,
    var ioMethod : IR_Expression,
    var includeGhostLayers : Boolean,
    var dataset : IR_Expression,
    var canonicalFileLayout : Boolean,
    var resolveId : Int) extends IR_Statement with IR_Expandable with IR_PrintVisualization with IR_PnetCDF_API {

  // validate params
  if (includeGhostLayers)
    Logger.error("Ghost layer visualization is currently unsupported for IR_PrintNetCDF!")
  if (numDimsGrid < 2)
    Logger.error("NetCDF visualization only supported for 2D/3D grids.")
  if (!canonicalFileLayout || !Knowledge.domain_onlyRectangular)
    Logger.error("NetCDF visualization only supports rectilinear domains.")
  if (!Knowledge.grid_isAxisAligned)
    Logger.error("Non-uniform && non-AA grids are unsupported.")
  if (field.localization != IR_AtNode)
    Logger.warn("NetCDF visualization expects data to be nodal: Cell- and face-centered variables are output as nodal at their corresponding discr. points.")

  override def ioInterface : String = "nc"

  override def numDimsGrid : Int = field.numDimsGrid
  override def numFields : Int = 1
  override def level : Int = field.level
  override def domainIndex : Int = field.domain.index

  def cellsPerDim(dim : Int) : Int = Knowledge.domain_fragmentLengthAsVec(dim) * (1 << level)
  def nodesPerDim(dim : Int) : Int = cellsPerDim(dim) + 1

  def IR_IV_TimeIdxRecords() = IR_IV_TimeIndexRecordVariables(resolveId)
  def IR_IV_TimeValRecords() = IR_IV_TimeValueRecordVariables(resolveId)

  override def numCellsPerFrag : IR_Expression = numCells_x * numCells_y * numCells_z
  override def numCells_x : Int = cellsPerDim(0)
  override def numCells_y : Int = cellsPerDim(1)
  override def numCells_z : Int = if (numDimsGrid > 2) cellsPerDim(2) else 1

  def dimsForLoc(dim : Int) : Int = field.localization match {
    case IR_AtNode              => nodesPerDim(dim)
    case IR_AtCellCenter        => cellsPerDim(dim)
    case IR_AtFaceCenter(`dim`) => nodesPerDim(dim)
    case IR_AtFaceCenter(_)     => cellsPerDim(dim)
  }

  // higher dim. datatypes are stored as separate scalar components for visualization
  // e.g. 3D vector as 3x scalar components each with KJI dims [z,y,x,1]
  // the CF conventions do not provide means to join datasets in order to interleave the component values (3 scalars -> 1 vector [z,y,x,3])
  // but can be re-constructed in visualization tools like Paraview (see "Calculator" tool) at the cost of user interaction
  def dataBuffersField : ListBuffer[IR_DataBuffer] = field.gridDatatype match {
    case _ : IR_ScalarDatatype   => ListBuffer(IR_DataBuffer(field, slot, includeGhostLayers, None, Some(IR_StringConstant(s"${field.name}")), canonicalFileLayout))
    case mat : IR_MatrixDatatype => (0 until mat.sizeM).to[ListBuffer].flatMap(r => (0 until mat.sizeN).map(c =>
      IR_DataBuffer(field, None, Some(IR_StringConstant(field.name + s"_${r}_$c")), r, c, canonicalFileLayout)))
  }

  def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] = dataBuffersField

  def recordVariables(constsIncluded : Boolean) : mutable.HashMap[Int, Boolean] = {
    val consts = dataBuffers(true).map(_.name).diff(dataBuffers(false).map(_.name)) // extract const bufs
    mutable.HashMap(dataBuffers(constsIncluded).zipWithIndex.map { case (buf, bufIdx) => (bufIdx, !consts.contains(buf.name)) } : _*) // non-constants are record variables
  }

  def fileMode(constsIncluded : Boolean) = IR_VariableAccess(if (!constsIncluded) "NC_WRITE" else "NC_64BIT_OFFSET | NC_CLOBBER", IR_UnknownDatatype) // VisIt does not recognize CDF-5 files
  def ioHandler(constsIncluded : Boolean) = IR_FileAccess_PnetCDF(filename, dataBuffers(constsIncluded), Some(recordVariables(constsIncluded)),
    writeAccess = true, appendedMode = !constsIncluded, initFragInfo = false, timeIdx = IR_IV_TimeIdxRecords(), timeVal = IR_IV_TimeValRecords(), altFileMode = Some(fileMode(constsIncluded)))

  override def expand() : OutputType = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    stmts += err_decl
    ioHandler(false).handleDependencies()
    ioHandler(false).validateParams()
    if (!Settings.additionalIncludes.contains("cstring"))
      Settings.additionalIncludes += "cstring"

    stmts += new IR_IfCondition(IR_ConstantsWrittenToFile().isEmpty,
      /* true: write constants and field data to file and mark that constants were already written */
      writeData(constsIncluded = true)
        :+ IR_ConstantsWrittenToFile().setFilename(filename),
      /* false: only write field data */
      writeData(constsIncluded = false))

    stmts
  }

  def writeData(constsIncluded : Boolean) : ListBuffer[IR_Statement] = {
    val appendedMode = !constsIncluded
    val handler = ioHandler(constsIncluded)
    val ncFile = handler.ncFile
    val buffers = dataBuffers(constsIncluded)

    // variable and dimension names
    def createDimName(d : Int) = IR_StringConstant(('X'+(numDimsGrid-1-d)).toChar.toString)
    def timeName = IR_StringConstant("time")
    def dimName(d : Int) = createDimName(d)
    def varNamePos(d : Int) = dimName(d) // must be identical with dimension name

    val varIdPosArray = (0 until numDimsGrid).map(d => IR_VariableAccess(IR_FileAccess.declareVariable(varNamePos(d).value), IR_IntegerDatatype))

    val varIdTime : IR_VariableAccess = handler.varIdTime
    lazy val varIdBuffer = handler.varIdBuffer

    var stmts : ListBuffer[IR_Statement] = ListBuffer()
    stmts ++= handler.createOrOpenFile()

    // setup
    if (!appendedMode) {
      /* define dims */
      // define time dim
      val dimIdTime = IR_VariableAccess(IR_FileAccess.declareVariable("dimIdTime"), IR_IntegerDatatype)
      stmts += IR_VariableDeclaration(dimIdTime)
      stmts ++= ncmpi_def_dim(ncFile, timeName, IR_VariableAccess("NC_UNLIMITED", IR_UnknownDatatype), IR_AddressOf(dimIdTime))

      // define spatial dims
      val dimId = IR_VariableAccess(IR_FileAccess.declareVariable("dimId"), IR_ArrayDatatype(IR_IntegerDatatype, numDimsGrid))
      stmts += IR_VariableDeclaration(dimId)
      for (d <- 0 until numDimsGrid) {
        val globalDims = handler.globalDims(0) // contains extents for each dim
        stmts ++= ncmpi_def_dim(ncFile, dimName(d), IR_ArrayAccess(globalDims, d), IR_AddressOf(IR_ArrayAccess(dimId, d)))
      }

      /* define vars */
      // define time dim
      stmts ++= ncmpi_def_var(ncFile, timeName, IR_DoubleDatatype, 1, IR_AddressOf(dimIdTime), IR_AddressOf(varIdTime))
      // for ParaView: // https://ferret.pmel.noaa.gov/Ferret/documentation/coards-netcdf-conventions
      val timeUnit = IR_StringConstant("seconds since 0000-00-00 00:00:00")
      stmts ++= ncmpi_put_att_text(ncFile, varIdTime, IR_StringConstant("units"), IR_FunctionCall(IR_ExternalFunctionReference("strlen"), timeUnit), timeUnit)

      // vars for positions, 1d arrays for uniform and AA
      for (id <- varIdPosArray) stmts += IR_VariableDeclaration(id)
      for (d <- 0 until numDimsGrid)
        stmts ++= ncmpi_def_var(ncFile, varNamePos(d), IR_RealDatatype, 1, IR_AddressOf(IR_ArrayAccess(dimId, d)), IR_AddressOf(varIdPosArray(d)))

      // vars for buffers
      val numDimsDataAndTime = numDimsGrid + 1
      val dimIdRecord = IR_VariableAccess(IR_FileAccess.declareVariable("dimId"), IR_ArrayDatatype(IR_IntegerDatatype, numDimsDataAndTime))
      stmts += IR_VariableDeclaration(dimIdRecord, IR_InitializerList(dimIdTime +: (0 until numDimsGrid).map(d => IR_ArrayAccess(dimId, d)) : _*))
      for ((buf, bufIdx) <- buffers.zipWithIndex) {
        stmts ++= ncmpi_def_var(ncFile, buf.datasetName, buf.datatype,
          numDimsDataAndTime, dimIdRecord, IR_AddressOf(varIdBuffer(bufIdx)))
      }
    } else {
      // inquire vars for time and buffers
      stmts ++= ncmpi_inq_varid(ncFile, timeName, IR_AddressOf(varIdTime))
      for ((buf, bufIdx) <- buffers.zipWithIndex) {
        stmts ++= ncmpi_inq_varid(ncFile, buf.datasetName, IR_AddressOf(varIdBuffer(bufIdx)))
      }
    }

    // end "define mode" when writing the file and go into "data mode"
    if (!appendedMode)
      stmts ++= ncmpi_enddef(ncFile)

    // write time values
    if (Knowledge.parIO_useCollectiveIO) {
      stmts ++= ncmpi_put_var1_type_all(IR_DoubleDatatype, ncFile, varIdTime, IR_AddressOf(IR_IV_TimeIdxRecords()), IR_AddressOf(IR_IV_TimeValRecords()))
    } else {
      stmts += IR_IfCondition(MPI_IsRootProc.apply(), // only root needs to write the time value for independent I/O
        ncmpi_put_var1_type(IR_DoubleDatatype, ncFile, varIdTime, IR_AddressOf(IR_IV_TimeIdxRecords()), IR_AddressOf(IR_IV_TimeValRecords())))
    }

    // write positions manually (1D arrays, not treated as databuffers)
    if (!appendedMode) {
      if (Knowledge.grid_isUniform || field.localization != IR_AtNode) {
        // uniform: need to copy into 1D buffers
        // non-nodal: no vf -> copy into 1D buffers
        for (d <- 0 until numDimsGrid) {
          val np = dimsForLoc(d)
          val dtBuf = IR_RealDatatype
          val dtDims = if (Knowledge.mpi_enabled) handler.MPI_Offset else IR_SpecialDatatype("size_t")
          val tmpBuf = IR_VariableAccess(IR_FileAccess.declareVariable("tmp" + varNamePos(d).value), IR_PointerDatatype(dtBuf))
          val count = IR_VariableAccess(IR_FileAccess.declareVariable("count"), dtDims)
          val globalStart = IR_VariableAccess("globalStart", dtDims)
          val it = IR_LoopOverDimensions.defItForDim(d)
          stmts += IR_LoopOverFragments(
            IR_VariableDeclaration(tmpBuf) +:
              IR_ArrayAllocation(tmpBuf, dtBuf, np) +:
              IR_VariableDeclaration(count, np) +:
              IR_ForLoop(IR_VariableDeclaration(it, 0), IR_Lower(it, np), IR_PreIncrement(it),
                IR_Assignment(IR_ArrayAccess(tmpBuf, it), getPos(field.localization, level, d))) +:
              IR_VariableDeclaration(globalStart, IR_IV_FragmentIndex(d) * np) +:
              ncmpi_put_vara_type_all(dtBuf, ncFile, varIdPosArray(numDimsGrid-1 - d), IR_AddressOf(globalStart), IR_AddressOf(count), IR_AddressOf(IR_ArrayAccess(tmpBuf, 0))) :+
              IR_ArrayFree(tmpBuf)
          )
        }
      } else if (Knowledge.grid_isAxisAligned && field.localization == IR_AtNode) {
        // AA and nodal: use vf directly
        for (d <- 0 until numDimsGrid) {
          val np = dimsForLoc(d)
          val dt = IR_RealDatatype
          val vf = IR_VF_NodePositionPerDim(level, field.domain, d).associatedField
          val dummyBuf = IR_DataBuffer(vf, slot, includeGhostLayers, None, None, canonicalFileLayout)
          val count = IR_VariableAccess(IR_FileAccess.declareVariable("count"), handler.ptrDatatype)
          val globalStart = IR_VariableAccess("globalStart", handler.ptrDatatype)

          stmts += IR_VariableDeclaration(count, vf.layout.defIdxById("DRE", d) - vf.layout.defIdxById("DLB", d))
          if (Knowledge.mpi_enabled) {
            // create mpi dt
            val countLoc = IR_VariableAccess(IR_FileAccess.declareVariable("localCount"), IR_ArrayDatatype(IR_IntegerDatatype, 1))
            val totalLoc = IR_VariableAccess(IR_FileAccess.declareVariable("localDimsTotal"), IR_ArrayDatatype(IR_IntegerDatatype, 1))
            val startLoc = IR_VariableAccess(IR_FileAccess.declareVariable("localStart"), IR_ArrayDatatype(IR_IntegerDatatype, 1))
            val localView = MPI_View(totalLoc, countLoc, startLoc, 1, createViewPerFragment = false, isLocal = true, dummyBuf, "localSubarray")

            MPI_View.addView(localView)
            stmts += IR_VariableDeclaration(countLoc, IR_InitializerList(vf.layout.defIdxById("DRE", d) - vf.layout.defIdxById("DLB", d)))
            stmts += IR_VariableDeclaration(totalLoc, IR_InitializerList(vf.layout.defTotal(d)))
            stmts += IR_VariableDeclaration(startLoc, IR_InitializerList(vf.layout.defIdxById("DLB", d)))
            stmts += localView.createDatatype

            // use flexible API
            val addr = IR_AddressOf(IR_FieldAccess(vf, slot, new IR_ExpressionIndex(vf.referenceOffset.map(i => IR_Negative(i)).toArray)))
            stmts += IR_LoopOverFragments(
              IR_VariableDeclaration(globalStart, IR_IV_FragmentIndex(d) * np) +:
                ncmpi_put_vara_all(ncFile, varIdPosArray(numDimsGrid-1 - d), IR_AddressOf(globalStart), IR_AddressOf(count), addr, 1, localView.getAccess))
          } else {
            // use plain vara, start at ref offset in memory
            val addr = IR_AddressOf(IR_FieldAccess(vf, slot, IR_ExpressionIndex(0)))
            stmts += IR_LoopOverFragments(
              IR_VariableDeclaration(globalStart, IR_IV_FragmentIndex(d) * np) +:
                ncmpi_put_vara_type_all(dt, ncFile, varIdPosArray(d), IR_AddressOf(globalStart), IR_AddressOf(count), addr))
          }
        }
      } else {
        Logger.error("Error in PrintNetCDF: Unsupported mesh type.")
      }
    }

    // start individual I/O if enabled
    if (!Knowledge.parIO_useCollectiveIO && Knowledge.mpi_enabled)
      stmts ++= ncmpi_begin_indep_data(ncFile)

    // write field data
    for ((buf, bufIdx) <- buffers.zipWithIndex) {
      val numDims = buf.totalDimsLocalKJI.length
      val lTotal = buf.declareDimensionality("lDimsTotal", IR_IntegerDatatype, buf.totalDimsLocalKJI)
      val lCount = buf.declareDimensionality("lCount", IR_IntegerDatatype, buf.innerDimsLocalKJI)
      val lStart = buf.declareDimensionality("lStart", IR_IntegerDatatype, buf.startIndexLocalKJI)

      // MPI: prepare derived data types for memory access (omit ghost layers)
      val localView = MPI_View(IR_VariableAccess(lTotal), IR_VariableAccess(lCount), IR_VariableAccess(lStart),
        numDims, createViewPerFragment = false, isLocal = true, buf, "localSubarray")
      if (Knowledge.mpi_enabled && MPI_View.addView(localView))
          stmts ++= ListBuffer(lTotal, lCount, lStart, localView.createDatatype) // decl extents and create dt

      // use dims already provided by I/O handler
      val globalStart = handler.globalStart(bufIdx)
      val count = handler.count(bufIdx)
      val countSel = handler.countSelection(bufIdx)
      val bufcount = 1
      val stride = handler.stride(bufIdx)
      val imap = handler.imap(bufIdx)

      // write databuffer
      val write = if (Knowledge.mpi_enabled) {
        // parallel application: use MPI derived datatype to skip (internal) intermediate buffer from "ncmpi_put_varm_type"
        val view = MPI_View.localViews.find(_ == localView).get
        if (Knowledge.parIO_useCollectiveIO) {
          handler.condAssignCount(bufIdx) ++
            ncmpi_put_vara_all(ncFile, varIdBuffer(bufIdx), globalStart, countSel, buf.getBaseAddress, bufcount, view.getAccess)
        } else {
          ncmpi_put_vara(ncFile, varIdBuffer(bufIdx), globalStart, count, buf.getBaseAddress, 1, view.getAccess)
        }
      } else {
        // serial: no MPI data types to describe in-memory layout -> use "nc_put_varm_<type>" instead
        ncmpi_put_varm_type(buf.datatype, ncFile, varIdBuffer(bufIdx), globalStart, count, stride, imap, buf.getAddressReferenceOffset)
      }
      stmts += handler.accessFileWithGranularity(bufIdx, write)
    }

    // cleanup and close file
    stmts ++= handler.cleanupAccess()
    stmts ++= handler.closeFile()

    stmts
  }

  // unused in this implementation
  override def connectivityForCell(global : Boolean) : ListBuffer[IR_Expression] = ???
  override def someCellField : IR_Field = ???
  override def nodeOffsets : ListBuffer[IR_ConstIndex] = ???
}
