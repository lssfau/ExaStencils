package exastencils.io.ir

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.config.Platform
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.logger.Logger
import exastencils.util.ir.IR_Print

case class IR_FileAccess_HDF5(
    var fileName : IR_Expression,
    var dataBuffers : ListBuffer[IR_DataBuffer],
    var writeAccess : Boolean,
    var appendedMode : Boolean = false) extends IR_FileAccess("hdf5", fileName, dataBuffers, writeAccess, appendedMode) with IR_Hdf5_API {

  override def openMode : IR_VariableAccess = if (writeAccess) {
    if (appendedMode) IR_VariableAccess("H5F_ACC_RDWR", IR_UnknownDatatype) else IR_VariableAccess("H5F_ACC_TRUNC", IR_UnknownDatatype)
  } else {
    IR_VariableAccess("H5F_ACC_RDONLY", IR_UnknownDatatype)
  }
  val ioMode : IR_VariableAccess = IR_VariableAccess(if (Knowledge.parIO_useCollectiveIO) "H5FD_MPIO_COLLECTIVE" else "H5FD_MPIO_INDEPENDENT", IR_UnknownDatatype)

  // hdf5 specific datatypes
  val hid_t = IR_SpecialDatatype("hid_t")
  val hsize_t = IR_SpecialDatatype("hsize_t")
  val herr_t = IR_SpecialDatatype("herr_t")
  val htri_t = IR_SpecialDatatype("htri_t")

  // decls
  val err_decl = IR_VariableDeclaration(herr_t, IR_FileAccess.declareVariable("err"))
  val fileId_decl = IR_VariableDeclaration(hid_t, IR_FileAccess.declareVariable("fileId"))
  val propertyList_decl = IR_VariableDeclaration(hid_t, IR_FileAccess.declareVariable("propertyList"))
  val transferList_decl = IR_VariableDeclaration(hid_t, IR_FileAccess.declareVariable("transferList"))
  val emptyDataspace_decl = IR_VariableDeclaration(hid_t, IR_FileAccess.declareVariable("emptyDataspace"))
  val dataset_decl : Array[IR_VariableDeclaration] = dataBuffers.map(buf => IR_VariableDeclaration(hid_t, IR_FileAccess.declareVariable("dataset_" + buf.name))).toArray
  val info_decl = IR_VariableDeclaration(IR_SpecialDatatype("MPI_Info"), IR_FileAccess.declareVariable("info"), IR_VariableAccess("MPI_INFO_NULL", IR_UnknownDatatype)) //TODO handle hints
  var declarations : ListBuffer[IR_VariableDeclaration] = dimensionalityDeclarations :+ err_decl :+ fileId_decl :+ propertyList_decl :+ transferList_decl

  // declarations per databuffer
  declarations ++= dataset_decl

  // add declarations which are only used in a parallel application
  if (Knowledge.mpi_enabled) {
    declarations += info_decl
  }

  // declarations for collective I/O
  if (Knowledge.parIO_useCollectiveIO) {
    declarations += emptyDataspace_decl
  }

  // variable accesses
  val err = IR_VariableAccess(err_decl)
  val fileId = IR_VariableAccess(fileId_decl)
  val propertyList = IR_VariableAccess(propertyList_decl)
  val transferList = IR_VariableAccess(transferList_decl)
  val emptyDataspace = IR_VariableAccess(emptyDataspace_decl)
  def dataspace(bufIdx : Int) : IR_VariableAccess = getDataspace(bufIdx).get
  def memspace(bufIdx : Int) : IR_VariableAccess = getMemspace(bufIdx).get
  def dataset(bufIdx : Int) = IR_VariableAccess(dataset_decl(bufIdx))

  // dataspaces and memspaces per buffer
  var memspaces : mutable.HashMap[String, IR_VariableAccess] = mutable.HashMap()
  def addMemspace(bufIdx : Int, acc : IR_VariableAccess) : Option[IR_VariableAccess] = memspaces.put(localDims(bufIdx).name, acc)
  def getMemspace(bufIdx : Int) : Option[IR_VariableAccess] = memspaces.get(localDims(bufIdx).name)

  var dataspaces : mutable.HashMap[String, IR_VariableAccess] = mutable.HashMap()
  def dataspaceKey : Array[String] = dataBuffers.indices.map(bufIdx => if (writeAccess) globalDims(bufIdx).name else globalDims(bufIdx).name + bufIdx).toArray
  def addDataspace(bufIdx : Int, acc : IR_VariableAccess) : Option[IR_VariableAccess] = {
    Logger.dbg(dataspaceKey(bufIdx))
    dataspaces.put(dataspaceKey(bufIdx), acc)
  }
  def getDataspace(bufIdx : Int) : Option[IR_VariableAccess] = {
    Logger.dbg(dataspaceKey(bufIdx))
    dataspaces.get(dataspaceKey(bufIdx))
  }

  val info = IR_VariableAccess(info_decl)
  val defaultPropertyList = IR_VariableAccess("H5P_DEFAULT", IR_UnknownDatatype)

  // get group names from absolute dataset path (w/o dataset name) beginning from root ("/"). E.g. /path/to/datasetName contains groups: /path, /path/to
  val locationId : IR_VariableAccess = fileId
  val groups : ListBuffer[String] = dataBuffers.flatMap(buf => {
    val absPath = buf.datasetName.asInstanceOf[IR_StringConstant].value
    (if (absPath.startsWith("/")) absPath else "/" + absPath).tail.split("/").scanLeft(""){_ + "/" + _}.tail.dropRight(1)
  }).distinct

  // handling of groups within the file
  var groupDecls : ListBuffer[IR_VariableDeclaration] = ListBuffer()
  def createGroupHierarchy : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    val groupExists_decl = IR_VariableDeclaration(htri_t, IR_FileAccess.declareVariable("linkExists"))
    val groupExists = IR_VariableAccess(groupExists_decl)
    stmts += groupExists_decl

    for (gName <- groups) {
      val groupId_decl = IR_VariableDeclaration(hid_t, IR_FileAccess.declareVariable("groupId"))
      val groupId = IR_VariableAccess(groupId_decl)
      stmts += groupId_decl
      groupDecls += groupId_decl

      // check if group already exists and open, otherwise create
      stmts ++= H5Lexists(groupExists, locationId, gName, defaultPropertyList)
      stmts += IR_IfCondition(groupExists > 0,
        H5Gopen2(groupId, locationId, gName, defaultPropertyList),
        H5Gcreate2(groupId, locationId, gName, defaultPropertyList, defaultPropertyList, defaultPropertyList))
    }

    stmts
  }

  def closeGroupHierarchy : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    for (g <- groupDecls)
      stmts ++= H5Gclose(err, IR_VariableAccess(g))

    stmts
  }

  override def createOrOpenFile() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // add decls
    declarations.foreach(decl => statements += decl)

    // setup property list for file creation/opening
    statements ++= H5Pcreate(propertyList, IR_VariableAccess("H5P_FILE_ACCESS", IR_UnknownDatatype))
    if (Knowledge.mpi_enabled)
      statements ++= H5Pset_fapl_mpio(err, propertyList, mpiCommunicator, info)

    // create/open file
    if (writeAccess && !appendedMode)
      statements ++= H5Fcreate(fileId, filenameAsCString, openMode, defaultPropertyList, propertyList)
    else
      statements ++= H5Fopen(fileId, filenameAsCString, openMode, propertyList)

    statements
  }

  override def setupAccess() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements ++= createGroupHierarchy

    // create memspace. select hyperslab to only use the inner points for file accesses.
    for (bufIdx <- dataBuffers.indices) {
      if (getMemspace(bufIdx).isEmpty) {
        val buf = dataBuffers(bufIdx)
        val memspace = IR_VariableAccess(IR_FileAccess.declareVariable("memspace_" + dataBuffers(bufIdx).localization.name), hid_t)
        statements += IR_VariableDeclaration(memspace)
        statements ++= H5Screate_simple(memspace, buf.datasetDimsLocal, localDims(bufIdx))
        statements ++= H5Sselect_hyperslab(err, memspace, IR_VariableAccess("H5S_SELECT_SET", IR_UnknownDatatype), localStart(bufIdx), stride(bufIdx), count(bufIdx))
        addMemspace(bufIdx, memspace)
      }
    }

    // empty dataspace in case of collective I/O with invalid frags
    if (Knowledge.parIO_useCollectiveIO) {
      statements ++= H5Screate(emptyDataspace, IR_VariableAccess("H5S_NULL", IR_UnknownDatatype))
    }

    // request I/O mode (independent/collective) via transfer list
    if (Knowledge.mpi_enabled) {
      statements ++= H5Pcreate(transferList, IR_VariableAccess("H5P_DATASET_XFER", IR_UnknownDatatype))
      statements ++= H5Pset_dxpl_mpio(err, transferList, ioMode)
    }

    statements
  }

  override def cleanupAccess() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements ++= closeGroupHierarchy

    // close property lists
    if (Knowledge.mpi_enabled) {
      statements ++= H5Pclose(err, propertyList)
      statements ++= H5Pclose(err, transferList)
    }

    // close data- and memspaces
    if (Knowledge.parIO_useCollectiveIO)
     statements ++= H5Sclose(err, emptyDataspace)
    for (space <- dataspaces.values ++ memspaces.values)
      statements ++= H5Sclose(err, space)

    // close dataset
    statements ++= dataBuffers.indices.flatMap(bufIdx => H5Dclose(err, dataset(bufIdx)))

    statements
  }

  override def closeFile() : ListBuffer[IR_Statement] = H5Fclose(err, fileId)

  override def accessFileFragwise(bufIdx : Int, accessStmts : ListBuffer[IR_Statement]) : IR_LoopOverFragments = {
    val buffer = dataBuffers(bufIdx)

    // set global starting index for fragment and select hyperslab in global domain
    val setOffsetFrag : ListBuffer[IR_Statement] = buffer.startIndexGlobalKJI.indices.map(d =>
      IR_Assignment(IR_ArrayAccess(globalStart(bufIdx), d), buffer.startIndexGlobalKJI(d)) : IR_Statement).to[ListBuffer]
    val selectHyperslab = H5Sselect_hyperslab(err, dataspace(bufIdx), IR_VariableAccess("H5S_SELECT_SET", IR_UnknownDatatype), globalStart(bufIdx), stride(bufIdx), count(bufIdx))

    if (Knowledge.parIO_useCollectiveIO && !dataBuffers(bufIdx).accessBlockwise) {
      val condAssignOffset = IR_IfCondition(IR_IV_IsValidForDomain(buffer.domainIdx), setOffsetFrag)
      IR_LoopOverFragments(
        (condAssignOffset +: selectHyperslab) ++ accessStmts)
    }
    else {
      IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(buffer.domainIdx),
          setOffsetFrag ++ selectHyperslab ++ accessStmts))
    }
  }

  override def accessFileBlockwise(bufIdx : Int, accessStatements : ListBuffer[IR_Statement]) : IR_Statement = {
    val buffer = dataBuffers(bufIdx)

    // set global starting index for block and select hyperslab in global domain
    val setOffsetBlock = IR_LoopOverBlocks(buffer.startIndexGlobalKJI.indices.map(d =>
        IR_Assignment(IR_ArrayAccess(globalStart(bufIdx), d), buffer.startIndexGlobalKJI(d)) : IR_Statement).to[ListBuffer])
    val selectHyperslab = H5Sselect_hyperslab(err, dataspace(bufIdx), IR_VariableAccess("H5S_SELECT_SET", IR_UnknownDatatype), globalStart(bufIdx), stride(bufIdx), count(bufIdx))

    IR_LoopOverBlocks(IR_IfCondition(IR_IV_IsValidForDomain(buffer.domainIdx),
      setOffsetBlock +: (selectHyperslab ++ accessStatements)))
  }

  // set data- and memspace to an empty space for "invalid" frags in order to perform a NOP read/write
  val dataspaceSelection = IR_VariableAccess("selectDataspace", hid_t)
  val memspaceSelection = IR_VariableAccess("selectMemspace", hid_t)
  def condAssignSpace(bufIdx : Int) : ListBuffer[IR_Statement] = {
    ListBuffer(
      IR_VariableDeclaration(dataspaceSelection, dataspace(bufIdx)),
      IR_VariableDeclaration(memspaceSelection, memspace(bufIdx)),
      IR_IfCondition(IR_Negation(IR_IV_IsValidForDomain(dataBuffers(bufIdx).domainIdx)),
        ListBuffer[IR_Statement](
          IR_Assignment(dataspaceSelection, emptyDataspace),
          IR_Assignment(memspaceSelection, emptyDataspace))))
  }

  override def read(bufIdx : Int) : ListBuffer[IR_Statement] = {
    val buffer = dataBuffers(bufIdx)
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // open dataset
    statements ++= H5Dopen2(dataset(bufIdx), locationId, buffer.datasetName, defaultPropertyList)

    // get dataspace (one space per buffer since we don't know the dataspaces in a file beforehand)
    val dataspace = IR_VariableAccess(IR_FileAccess.declareVariable("dataspace_" + buffer.localization.name), hid_t)
    statements += IR_VariableDeclaration(dataspace)
    statements ++= H5Dget_space(dataspace, dataset(bufIdx))
    addDataspace(bufIdx, dataspace)

    // get properties from dataspace and compare with provided values
    def checkDims(readBuffer : IR_Statement) : ListBuffer[IR_Statement] = {
      if (Knowledge.parIO_generateDebugStatements) {
        val rank_decl = IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("rank"))
        val dimsDataset_decl = IR_VariableDeclaration(IR_ArrayDatatype(hsize_t, buffer.datasetDimsGlobal), IR_FileAccess.declareVariable("dimsDataset"))
        val rank = IR_VariableAccess(rank_decl)
        val dimsDataset = IR_VariableAccess(dimsDataset_decl)

        var dbgStmts : ListBuffer[IR_Statement] = ListBuffer()
        dbgStmts += rank_decl

        var falseBdy = ListBuffer[IR_Statement]()
        falseBdy += dimsDataset_decl
        falseBdy ++= H5Sget_simple_extent_dims(rank, dataspace, dimsDataset)
        falseBdy += IR_IfCondition(
          buffer.globalDimsKJI.indices.map(d => IR_ArrayAccess(dimsDataset, d) Neq IR_ArrayAccess(globalDims(bufIdx), d)).fold(IR_BooleanConstant(true))((a, b) => a AndAnd b), // compare dimensionality
          IR_Print(IR_VariableAccess("std::cout", IR_UnknownDatatype), IR_StringConstant("Dimensionality mismatch! No data is read from the file.")),
          readBuffer)

        dbgStmts ++= H5Sget_simple_extent_ndims(rank, dataspace)
        dbgStmts += IR_IfCondition(rank Neq buffer.datasetDimsGlobal,
          ListBuffer[IR_Statement](IR_Print(IR_VariableAccess("std::cout", IR_UnknownDatatype), IR_StringConstant("Rank mismatch! No data is read from the file."))),
          falseBdy)

        dbgStmts
      } else {
        ListBuffer(readBuffer)
      }
    }

    if (Knowledge.parIO_useCollectiveIO && !dataBuffers(bufIdx).accessBlockwise) {
      statements ++= checkDims(
        accessFileWithGranularity(bufIdx,
          condAssignSpace(bufIdx) ++
            H5Dread(err, dataset(bufIdx), buffer.datatype, memspaceSelection, dataspaceSelection, transferList, buffer.getBaseAddress)))
    } else {
      statements ++= checkDims(
        accessFileWithGranularity(bufIdx,
          H5Dread(err, dataset(bufIdx), buffer.datatype, memspace(bufIdx), dataspace, transferList, buffer.getBaseAddress)))
    }

    statements
  }

  override def write(bufIdx : Int) : ListBuffer[IR_Statement] = {
    val buffer = dataBuffers(bufIdx)
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // create dataspace (one dataspace for multiple buffers with identical dimensionality)
    val dataspace = getDataspace(bufIdx).getOrElse({
      val space = IR_VariableAccess(IR_FileAccess.declareVariable("dataspace_" + buffer.localization.name), hid_t)
      statements += IR_VariableDeclaration(space)
      statements ++= H5Screate_simple(space, buffer.datasetDimsGlobal, globalDims(bufIdx))
      addDataspace(bufIdx, space)

      space
    })

    // create dataset
    statements ++= H5Dcreate2(dataset(bufIdx), locationId, buffer.datasetName, buffer.datatype, dataspace, defaultPropertyList, defaultPropertyList, defaultPropertyList)

    if (Knowledge.parIO_useCollectiveIO && !dataBuffers(bufIdx).accessBlockwise) {
      statements += accessFileWithGranularity(bufIdx,
        condAssignSpace(bufIdx) ++
          H5Dwrite(err, dataset(bufIdx), buffer.datatype, memspaceSelection, dataspaceSelection, transferList, buffer.getBaseAddress))
    } else {
      statements += accessFileWithGranularity(bufIdx,
        H5Dwrite(err, dataset(bufIdx), buffer.datatype, memspace(bufIdx), dataspace, transferList, buffer.getBaseAddress))
    }

    statements
  }

  override def validateParams() : Unit = {
    for (buf <- dataBuffers)
      if (buf.datasetName == IR_NullExpression)
        Logger.error("Parameter \"dataset\" was not specified.")

    if (!Knowledge.mpi_enabled)
      Knowledge.parIO_useCollectiveIO = false // no collective I/O handling for serial programs
  }

  // when hdf5 is installed with --enable-parallel flag, the "hdf5.h" header requires "mpi.h" -> serial programs unfortunately require MPI headers and libs, too
  // alternative solution: install an additional, serial version of hdf5
  if (!Knowledge.mpi_enabled) {
    Platform.targetCompilerBinary = "mpicxx" // TODO
  }

  override def includes : ListBuffer[String] = ListBuffer("hdf5.h")
  override def libraries : ListBuffer[String] = ListBuffer("hdf5")
  override def pathsInc : ListBuffer[String] = ListBuffer("$(HDF5_HOME)/include")
  override def pathsLib : ListBuffer[String] = ListBuffer("$(HDF5_HOME)/lib")
}
