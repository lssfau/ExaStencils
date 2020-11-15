package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.config.Settings
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir._
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.util.ir.IR_Print

case class IR_FileAccess_HDF5(
    var fileName : IR_Expression,
    var datasetName : IR_Expression,
    var field : IR_Field,
    var slot : IR_Expression,
    var includeGhostLayers : Boolean, // TODO: handling
    var writeAccess : Boolean,
    var appendedMode : Boolean = false) extends IR_FileAccess(fileName, field, slot, includeGhostLayers, writeAccess, appendedMode) {

  val openMode = if(writeAccess)
    if(appendedMode) IR_VariableAccess("H5F_ACC_RDWR", IR_UnknownDatatype) else IR_VariableAccess("H5F_ACC_TRUNC", IR_UnknownDatatype)
  else
    IR_VariableAccess("H5F_ACC_RDONLY", IR_UnknownDatatype)
  val ioMode = IR_VariableAccess("H5FD_MPIO_COLLECTIVE", IR_UnknownDatatype) // TODO: knowledge parameter
  val rank = numDimsData

  // TODO: Handling collective/independent I/O

  // hdf5 specific datatypes
  val hid_t = IR_SpecialDatatype("hid_t")
  val hsize_t = IR_SpecialDatatype("hsize_t")
  val herr_t = IR_SpecialDatatype("herr_t")
  val htri_t = IR_SpecialDatatype("htri_t")
  // decls
  // TODO move most to IR_FileAccess ...
  val err_decl = IR_VariableDeclaration(herr_t, IR_FileAccess.declareVariable("err"))
  val fileId_decl = IR_VariableDeclaration(hid_t, IR_FileAccess.declareVariable("fileId"))
  val propertyList_decl = IR_VariableDeclaration(hid_t, IR_FileAccess.declareVariable("propertyList"))
  val transferList_decl = IR_VariableDeclaration(hid_t, IR_FileAccess.declareVariable("transferList"))
  val dataspace_decl = IR_VariableDeclaration(hid_t, IR_FileAccess.declareVariable("dataspace_"+field.name))
  val memspace_decl = IR_VariableDeclaration(hid_t, IR_FileAccess.declareVariable("memspace_"+field.name))
  val dataset_decl = IR_VariableDeclaration(hid_t, IR_FileAccess.declareVariable("dataset_"+field.name))
  val stride_decl = IR_VariableDeclaration(IR_ArrayDatatype(hsize_t, numDimsData), IR_FileAccess.declareVariable("stride"), IR_InitializerList(stride_local : _*))
  val count_decl = IR_VariableDeclaration(IR_ArrayDatatype(hsize_t, numDimsData), IR_FileAccess.declareVariable("count"), IR_InitializerList(innerPoints_local : _*))
  val localDims_decl = IR_VariableDeclaration(IR_ArrayDatatype(hsize_t, numDimsData), IR_FileAccess.declareVariable("localDims"), IR_InitializerList(totalPoints_local : _*))
  val localStart_decl = IR_VariableDeclaration(IR_ArrayDatatype(hsize_t, numDimsData), IR_FileAccess.declareVariable("localStart"), IR_InitializerList(startIdx_local : _*))
  val globalDims_decl = IR_VariableDeclaration(IR_ArrayDatatype(hsize_t, numDimsData), IR_FileAccess.declareVariable("globalDims"), IR_InitializerList(innerPoints_global : _*))
  val globalStart_decl = IR_VariableDeclaration(IR_ArrayDatatype(hsize_t, numDimsData), IR_FileAccess.declareVariable("globalStart"))
  val info_decl = IR_VariableDeclaration(IR_SpecialDatatype("MPI_Info"), IR_FileAccess.declareVariable("info"), IR_VariableAccess("MPI_INFO_NULL", IR_UnknownDatatype)) //TODO handle hints
  val declCollection : ListBuffer[IR_VariableDeclaration] = ListBuffer(
    err_decl, fileId_decl, propertyList_decl, transferList_decl, dataspace_decl, memspace_decl, dataset_decl, info_decl,
    stride_decl, count_decl, localDims_decl, localStart_decl, globalDims_decl, globalStart_decl)
  // variable accesses
  val err = IR_VariableAccess(err_decl)
  val fileId = IR_VariableAccess(fileId_decl)
  val propertyList = IR_VariableAccess(propertyList_decl)
  val transferList = IR_VariableAccess(transferList_decl)
  val dataspace = IR_VariableAccess(dataspace_decl)
  val memspace = IR_VariableAccess(memspace_decl)
  val dataset = IR_VariableAccess(dataset_decl)
  val stride = IR_VariableAccess(stride_decl)
  val count = IR_VariableAccess(count_decl)
  val localDims = IR_VariableAccess(localDims_decl)
  val localStart = IR_VariableAccess(localStart_decl)
  val globalDims = IR_VariableAccess(globalDims_decl)
  val globalStart = IR_VariableAccess(globalStart_decl)
  val info = IR_VariableAccess(info_decl)
  val defaultPropertyList = IR_VariableAccess("H5P_DEFAULT", IR_UnknownDatatype)

  // from: https://support.hdfgroup.org/HDF5/doc1.6/UG/11_Datatypes.html
  val h5Datatype = {
    val dt = field.layout.datatype.prettyprint match {
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
      case _ => Logger.error("Unsupported field datatype when using HDF5: " + field.layout.datatype.resolveBaseDatatype.prettyprint)
    }
    IR_VariableAccess(dt, IR_UnknownDatatype)
  }

  // NOTE: loc_id is fixed to filename in this implementation
  // -> name of dataset must contain an absolute path beginning from root ("/") of the group hierarchy
  // https://support.hdfgroup.org/HDF5/doc1.8/_topic/loc_id+name_obj.htm
  val locationId = fileId
  val groups : ListBuffer[String] = { // get group names from absolute dataset path (w/o dataset name)
    val absPath = datasetName.asInstanceOf[IR_StringConstant].value
    absPath.tail.split("/").scanLeft(""){_ + "/" + _}.tail.dropRight(1).to[ListBuffer]
  }

  // calls a function from the HDF5 library and checks if an error occured (i.e. toAssign was set to a value < 0) when debug statements are enabled
  def callH5Function(toAssign : IR_VariableAccess, funcName : String, args : IR_Expression*) : ListBuffer[IR_Statement] = {
    val stmts : ListBuffer[IR_Statement] = ListBuffer()
    stmts += IR_Assignment(toAssign, IR_FunctionCall(IR_ExternalFunctionReference(funcName), args : _*))
    if(Knowledge.parIO_generateDebugStatements)
      stmts += IR_IfCondition(toAssign < 0,
        IR_Print(IR_VariableAccess("std::cout", IR_UnknownDatatype), IR_VariableAccess("__FILE__", IR_UnknownDatatype), IR_StringConstant(": Error at line: "), IR_VariableAccess("__LINE__", IR_UnknownDatatype), IR_Print.endl))
    stmts
  }

  // handling of groups within the file
  var groupDecls : ListBuffer[IR_VariableDeclaration] = ListBuffer()
  def createGroupHierarchy : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    val groupExists_decl = IR_VariableDeclaration(htri_t, IR_FileAccess.declareVariable("linkExists"))
    val groupExists = IR_VariableAccess(groupExists_decl)
    stmts += groupExists_decl

    for(gName <- groups) {
      val groupId_decl = IR_VariableDeclaration(hid_t, IR_FileAccess.declareVariable("groupId"))
      val groupId = IR_VariableAccess(groupId_decl)
      stmts += groupId_decl
      groupDecls += groupId_decl

      // check if group already exists and open, otherwise create
      stmts ++= callH5Function(groupExists, "H5Lexists", locationId, IR_StringConstant(gName), defaultPropertyList)
      stmts += IR_IfCondition(groupExists > 0,
        callH5Function(groupId, "H5Gopen2", locationId, IR_StringConstant(gName), defaultPropertyList),
        callH5Function(groupId, "H5Gcreate2", locationId, IR_StringConstant(gName), defaultPropertyList, defaultPropertyList, defaultPropertyList)
      )
    }

    stmts
  }
  def closeGroupHierarchy : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    for(g <- groupDecls)
      stmts ++= callH5Function(err, "H5Gclose", IR_VariableAccess(g))

    stmts
  }

  override def prologue() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // add decls
    declCollection.foreach(decl => statements += decl)

    // setup property list for file access
    statements ++= callH5Function(propertyList, "H5Pcreate", IR_VariableAccess("H5P_FILE_ACCESS", IR_UnknownDatatype))
    if(Knowledge.mpi_enabled)
      statements ++= callH5Function(err, "H5Pset_fapl_mpio", propertyList, mpiCommunicator, info)

    // create/open file
    if(writeAccess && !appendedMode)
      statements ++= callH5Function(fileId, "H5Fcreate", fileName, openMode, defaultPropertyList, propertyList)
    else
      statements ++= callH5Function(fileId, "H5Fopen", fileName, openMode, propertyList)

    statements ++= createGroupHierarchy

    // create memspace. select hyperslab to only use the inner points for file accesses.
    if(Knowledge.domain_onlyRectangular) {
      statements ++= callH5Function(memspace, "H5Screate_simple", rank, localDims, nullptr)
      statements ++= callH5Function(err, "H5Sselect_hyperslab", memspace, IR_VariableAccess("H5S_SELECT_SET", IR_UnknownDatatype), localStart, stride, count, nullptr)
    } else {
      // TODO
      Logger.error("Unimplemented!")
    }

    // request I/O mode (independent/collective) via transfer list
    statements ++= callH5Function(transferList, "H5Pcreate", IR_VariableAccess("H5P_DATASET_XFER", IR_UnknownDatatype))
    statements ++= callH5Function(err, "H5Pset_dxpl_mpio", transferList, ioMode)

    statements
  }

  override def epilogue() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // cleanup
    statements ++= closeGroupHierarchy
    statements ++= callH5Function(err, "H5Pclose", propertyList)
    statements ++= callH5Function(err, "H5Pclose", transferList)
    statements ++= callH5Function(err, "H5Sclose", dataspace)
    statements ++= callH5Function(err, "H5Sclose", memspace)
    statements ++= callH5Function(err, "H5Dclose", dataset)
    statements ++= callH5Function(err, "H5Fclose", fileId)

    statements
  }

  def accessFileFragwise(accessStmts : ListBuffer[IR_Statement]) : IR_LoopOverFragments = {
    // set global starting index for fragment and select hyperslab in global domain
    val setOffsetFrag : ListBuffer[IR_Assignment] = numDimsDataRange.map(d => IR_Assignment(IR_ArrayAccess(globalStart, d), startIdx_global(d))).to[ListBuffer]
    val selectHyperslab : ListBuffer[IR_Statement] = callH5Function(err, "H5Sselect_hyperslab", dataspace, IR_VariableAccess("H5S_SELECT_SET", IR_UnknownDatatype), globalStart, stride, count, nullptr)
    IR_LoopOverFragments(
      IR_IfCondition(IR_IV_IsValidForDomain(field.domain.index),
        setOffsetFrag ++ selectHyperslab ++ accessStmts
      )
    )
  }

  override def readField() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // open dataset
    statements ++= callH5Function(dataset, "H5Dopen2", locationId, datasetName, defaultPropertyList)

    // get dataspace
    statements ++= callH5Function(dataspace, "H5Dget_space", dataset)

    // get properties from dataspace and compare with provided values
    def checkDims(readField : IR_LoopOverFragments) : ListBuffer[IR_Statement] = if(Knowledge.parIO_generateDebugStatements) {
      val rank_decl = IR_VariableDeclaration(IR_IntegerDatatype, "rank")
      val dimsDataset_decl = IR_VariableDeclaration(IR_ArrayDatatype(hsize_t, numDimsData), IR_FileAccess.declareVariable("dimsDataset"))
      val rank = IR_VariableAccess(rank_decl)
      val dimsDataset = IR_VariableAccess(dimsDataset_decl)

      var dbgStmts : ListBuffer[IR_Statement] = ListBuffer()
      dbgStmts += rank_decl

      var falseBdy = ListBuffer[IR_Statement]()
      falseBdy += dimsDataset_decl
      falseBdy ++= callH5Function(rank, "H5Sget_simple_extent_dims", dataspace, dimsDataset, nullptr)
      falseBdy += IR_IfCondition(
        numDimsDataRange.map(d => IR_ArrayAccess(dimsDataset, d) Neq IR_ArrayAccess(globalDims, d)).fold(IR_BooleanConstant(true))((a, b) => a AndAnd(b)), // compare dimensionality
        IR_Print(IR_VariableAccess("std::cout", IR_UnknownDatatype), IR_StringConstant("Dimensionality mismatch! No data is read from the file.")),
        readField
      )

      dbgStmts ++= callH5Function(rank, "H5Sget_simple_extent_ndims", dataspace)
      dbgStmts += IR_IfCondition(rank Neq numDimsData,
        ListBuffer[IR_Statement](
          IR_Print(IR_VariableAccess("std::cout", IR_UnknownDatatype), IR_StringConstant("Rank mismatch! No data is read from the file."))
        ),
        falseBdy
      )

      dbgStmts
    } else {
      ListBuffer(readField)
    }

    statements ++= checkDims(
      accessFileFragwise(
        callH5Function(err, "H5Dread", dataset, h5Datatype, memspace, dataspace, transferList, fieldptr)
      )
    )

    statements
  }

  override def writeField() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // create dataspace
    statements ++= callH5Function(dataspace, "H5Screate_simple", rank, globalDims, nullptr)

    // create dataset
    statements ++= callH5Function(dataset, "H5Dcreate2", locationId, datasetName, h5Datatype, dataspace, defaultPropertyList, defaultPropertyList, defaultPropertyList)

    statements += accessFileFragwise(
      callH5Function(err, "H5Dwrite", dataset, h5Datatype, memspace, dataspace, transferList, fieldptr)
    )

    statements
  }

  override def expand() : Output[StatementList]  = {
    // headers, paths and libs
    if (!Settings.additionalIncludes.contains("hdf5.h"))
      Settings.additionalIncludes += "hdf5.h"
    if(!Settings.additionalLibs.contains("hdf5"))
      Settings.additionalLibs += "hdf5"
    if(!Settings.pathsInc.contains("$(HDF5_HOME)/include"))
      Settings.pathsInc += "$(HDF5_HOME)/include"
    if(!Settings.pathsLib.contains("$(HDF5_HOME)/lib"))
      Settings.pathsLib += "$(HDF5_HOME)/lib"

    if(datasetName == IR_NullExpression) {
      Logger.error("Parameter \"dataset\" was not specified.")
    }

    var stmts : ListBuffer[IR_Statement] = ListBuffer()
    stmts ++= prologue()
    stmts ++= kernel()
    stmts ++= epilogue()
    stmts
  }
}
