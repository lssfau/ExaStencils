package exastencils.io.ir

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.config.Settings
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir.StatementList
import exastencils.grid.ir.IR_Localization
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression

// IR_FileAccess
// Used to read/write field data from/to files

object IR_FileAccess {
  // prohibit redeclaration of variables for sequences of I/O statements in the same scope
  private val declMap : mutable.HashMap[String, Int] = mutable.HashMap()
  def declareVariable(s: String) : String = {
    val counter = declMap.getOrElseUpdate(s, 0)
    declMap.update(s, counter+1)
    s + "_%02d".format(counter)
  }

  // reduce the number of duplicate declarations in the target code for identical dimensionalities
  private val dimensionalityMap : mutable.HashMap[String, IR_VariableDeclaration] = mutable.HashMap()
  def declareDimensionality(dt : IR_Datatype, name : String, localization : IR_Localization, dims : Option[ListBuffer[IR_Expression]] = None) : IR_VariableDeclaration = {
    val declName = name + localization.name
    val lookup = declName + IR_SimplifyExpression.simplifyIntegralExpr(
      if (dims.isDefined) dims.get.reduce(_ * _) else IR_IntegerConstant(0)).prettyprint // TODO maybe find a more robust key
    dimensionalityMap.getOrElseUpdate(lookup, IR_VariableDeclaration(dt, declareVariable(declName), if(dims.isDefined) Some(IR_InitializerList(dims.get.reverse : _*)) else None))
  }
  def resetDimensionalityMap() : Unit = dimensionalityMap.clear()
}

abstract class IR_FileAccess(
    interfaceName : String,
    filename : IR_Expression,
    dataBuffers : ListBuffer[IR_DataBuffer],
    writeAccess : Boolean,
    appendedMode : Boolean = false) extends IR_Statement with IR_Expandable {

  // commonly used datatypes
  val MPI_Offset : IR_SpecialDatatype = if (Knowledge.mpi_enabled) IR_SpecialDatatype("MPI_Offset") else IR_SpecialDatatype("size_t")
  val MPI_Comm = IR_SpecialDatatype("MPI_Comm")
  lazy val datatypeDimArray : IR_Datatype = interfaceName match {
    case "mpiio" => IR_IntegerDatatype
    case "nc"    => MPI_Offset
    case "hdf5"  => IR_SpecialDatatype("hsize_t")
    case _ =>
      Logger.warn("IR_FileAccess: Unknown I/O interface used for \"datatypeDimArray\"")
      IR_UnknownDatatype
  }
  def mpiDatatypeBuffer(buf : IR_DataBuffer) = IR_VariableAccess(buf.datatype.resolveBaseDatatype.prettyprint_mpi, IR_UnknownDatatype)

  // commonly used declarations
  def stride_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => IR_FileAccess.declareDimensionality(
    IR_ArrayDatatype(datatypeDimArray, buf.numDimsData), "stride", buf.localization, Some(buf.stride)))
  def count_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => IR_FileAccess.declareDimensionality(
    IR_ArrayDatatype(datatypeDimArray, buf.numDimsData), "count", buf.localization, Some(buf.innerDimsLocal)))
  def localDims_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => IR_FileAccess.declareDimensionality(
    IR_ArrayDatatype(datatypeDimArray, buf.numDimsData), "localDims", buf.localization, Some(buf.totalDimsLocal)))
  def localStart_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => IR_FileAccess.declareDimensionality(
    IR_ArrayDatatype(datatypeDimArray, buf.numDimsData), "localStart", buf.localization, Some(buf.startIndexLocal)))
  def globalDims_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => IR_FileAccess.declareDimensionality(
    IR_ArrayDatatype(datatypeDimArray, buf.numDimsData), "globalDims", buf.localization, Some(buf.innerDimsGlobal)))
  def globalStart_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => IR_FileAccess.declareDimensionality(
    IR_ArrayDatatype(datatypeDimArray, buf.numDimsData), "globalStart", buf.localization, Some(ListBuffer.fill(buf.numDimsData)(IR_IntegerConstant(0)))))
  def dimensionalityDeclarations : ListBuffer[IR_VariableDeclaration] = stride_decl.distinct ++ count_decl.distinct ++
    localDims_decl.distinct ++ localStart_decl.distinct ++ globalDims_decl.distinct ++ globalStart_decl.distinct

  // commonly used variable accesses
  val mpiCommunicator = IR_VariableAccess("mpiCommunicator", IR_UnknownDatatype)
  def stride(bufIdx : Int) = IR_VariableAccess(stride_decl(bufIdx))
  def count(bufIdx : Int) = IR_VariableAccess(count_decl(bufIdx))
  def localDims(bufIdx : Int) = IR_VariableAccess(localDims_decl(bufIdx))
  def localStart(bufIdx : Int) = IR_VariableAccess(localStart_decl(bufIdx))
  def globalDims(bufIdx : Int) = IR_VariableAccess(globalDims_decl(bufIdx))
  def globalStart(bufIdx : Int) = IR_VariableAccess(globalStart_decl(bufIdx))

  // structure of file access classes
  def createOrOpenFile() : ListBuffer[IR_Statement]
  def setupAccess() : ListBuffer[IR_Statement]
  def accessFile(bufIdx : Int) : ListBuffer[IR_Statement] = {
    if(writeAccess) {
      write(bufIdx)
    } else {
      read(bufIdx)
    }
  }
  def accessFile(buffer : IR_DataBuffer) : ListBuffer[IR_Statement] = accessFile(dataBuffers.indexOf(buffer))
  def cleanupAccess() : ListBuffer[IR_Statement]
  def closeFile() : ListBuffer[IR_Statement]

  // headers, libs and paths of each I/O interface
  def libraries : ListBuffer[String] = ListBuffer()
  def pathsLib : ListBuffer[String] = ListBuffer()
  def includes : ListBuffer[String] = ListBuffer()
  def pathsInc : ListBuffer[String] = ListBuffer()

  // determines with which mode the file is opened/created
  def openMode : IR_VariableAccess

  // checks input parameters that were passed
  def validateParams() : Unit = {}

  def accessFileWithGranularity(bufIdx : Int, accessStatements : ListBuffer[IR_Statement]) : IR_Statement = if (dataBuffers(bufIdx).accessBlockwise) {
    accessFileBlockwise(bufIdx, accessStatements)
  } else {
    accessFileFragwise(bufIdx, accessStatements)
  }

  // method to access the file in a fragment-wise fashion
  /* IMPORTANT:
    For collective I/O, each process must participate in a read/write call. Therefore, the I/O library matches the function calls of each process (synchronization).
    In case that a process's number of valid fragments differs from the other processes, some adaptions need to be made:
      1. Instead of checking if the fragment is valid before writing, we write without any conditions. Otherwise we would deadlock (this is quite similar to a conditional MPI_Barrier).
      2. In case that we have an "invalid" fragment, we participate in the collective function call but actually write nothing.
  */
  def accessFileFragwise(bufIdx : Int, accessStatements : ListBuffer[IR_Statement]) : IR_LoopOverFragments

  // method to access the file in a block-wise fashion
  def accessFileBlockwise(bufIdx : Int, accessStatements : ListBuffer[IR_Statement]) : IR_Statement = IR_NullStatement // TODO implement and replace calls with accessFileWithGranularity

  // core methods for file access
  def read(bufIdx : Int) : ListBuffer[IR_Statement]
  def write(bufIdx : Int) : ListBuffer[IR_Statement]

  override def expand() : Output[StatementList] = {
    // add new headers, paths and libs
    for (inc <- includes) {
      if (!Settings.additionalIncludes.contains(inc))
        Settings.additionalIncludes += inc
    }
    for (lib <- libraries) {
      if (!Settings.additionalLibs.contains(lib))
        Settings.additionalLibs += lib
    }
    for (pathInc <- pathsInc) {
      if (!Settings.pathsInc.contains(pathInc))
        Settings.pathsInc += pathInc
    }
    for (pathLib <- pathsLib) {
      if (!Settings.pathsLib.contains(pathLib))
        Settings.pathsLib += pathLib
    }

    validateParams()

    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    stmts ++= createOrOpenFile()
    stmts ++= setupAccess()
    for (bufIdx <- dataBuffers.indices) {
      stmts ++= accessFile(bufIdx)
    }
    stmts ++= cleanupAccess()
    stmts ++= closeFile()

    // reset lookup tables
    IR_FileAccess.resetDimensionalityMap()

    stmts
  }
}
