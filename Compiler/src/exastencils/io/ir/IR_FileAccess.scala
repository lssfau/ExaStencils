package exastencils.io.ir

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.config.Settings
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir.StatementList
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression

// IR_FileAccess
// Used to read/write field data from/to files

object IR_FileAccess {
  def filenameAsCString(filename : IR_Expression) : IR_Expression = filename match {
    case sc : IR_StringConstant                                         => sc
    case vAcc : IR_VariableAccess if vAcc.datatype == IR_StringDatatype => IR_MemberFunctionCall(vAcc, "c_str")
    case _                                                              => Logger.error("Wrong datatype passed for parameter \"filename\" to IR_FileAccess. Should be a \"String\" instead of:" + _)
  }

  // prohibit redeclaration of variables for sequences of I/O statements in the same scope
  private val declMap : mutable.HashMap[String, Int] = mutable.HashMap()
  def declareVariable(s: String) : String = {
    val counter = declMap.getOrElseUpdate(s, 0)
    declMap.update(s, counter+1)
    s + "_%02d".format(counter)
  }
}

abstract class IR_FileAccess(interfaceName : String) extends IR_Statement with IR_Expandable {

  /* commonly used datatypes */
  val MPI_Offset : IR_SpecialDatatype = if (Knowledge.mpi_enabled) IR_SpecialDatatype("MPI_Offset") else IR_SpecialDatatype("size_t")
  val MPI_Comm = IR_SpecialDatatype("MPI_Comm")
  def datatypeDimArray : IR_Datatype = interfaceName match {
    case "mpiio" => IR_IntegerDatatype
    case "nc"    => MPI_Offset
    case "hdf5"  => IR_SpecialDatatype("hsize_t")
    case _ =>
      Logger.warn("IR_FileAccess: Unknown I/O interface used for \"datatypeDimArray\"")
      IR_UnknownDatatype
  }
  def mpiDatatypeBuffer(buf : IR_DataBuffer) = IR_VariableAccess(buf.datatype.resolveBaseDatatype.prettyprint_mpi, IR_UnknownDatatype)

  // to be implemented by all subclasses
  def dataBuffers : ListBuffer[IR_DataBuffer]
  def filename : IR_Expression
  def writeAccess : Boolean
  def appendedMode : Boolean

  /* commonly used declarations. mainly for dimensionality extents */
  def stride_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => {
    buf.declareDimensionality("stride", datatypeDimArray,
      IR_DataBuffer.handleFragmentDimension(buf, buf.strideKJI,
        fragmentDim = 1))
  })
  def count_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => {
    buf.declareDimensionality("count", datatypeDimArray,
      IR_DataBuffer.handleFragmentDimension(buf, buf.innerDimsLocalKJI,
        fragmentDim = if (buf.accessBlockwise) IR_IV_NumValidFrags(buf.domainIdx) else 1))
  })
  def localDims_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => {
    buf.declareDimensionality("localDims", datatypeDimArray,
      IR_DataBuffer.handleFragmentDimension(buf, buf.totalDimsLocalKJI,
        fragmentDim = if (buf.accessBlockwise) IR_IV_NumValidFrags(buf.domainIdx) else 1))
  })
  def localStart_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => {
    buf.declareDimensionality("localStart", datatypeDimArray,
      IR_DataBuffer.handleFragmentDimension(buf, buf.startIndexLocalKJI,
        fragmentDim = 0))
  })
  def globalDims_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => {
    buf.declareDimensionality("globalDims", datatypeDimArray, buf.globalDimsKJI)
  })
  def globalStart_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => {
    buf.declareDimensionality("globalStart", datatypeDimArray, ListBuffer.fill(buf.datasetDimsGlobal)(IR_IntegerConstant(0)))
  })

  /* collection with declarations of dim. extents */
  def dimensionalityDeclarations : ListBuffer[IR_VariableDeclaration] = dataBuffers.indices.flatMap(idx => {
    stride_decl(idx) :: count_decl(idx) :: localDims_decl(idx) :: localStart_decl(idx) :: globalDims_decl(idx) :: globalStart_decl(idx) :: Nil
  }).distinct.to[ListBuffer]

  /* commonly used variable accesses */
  val mpiCommunicator = IR_VariableAccess("mpiCommunicator", IR_UnknownDatatype)
  def stride(bufIdx : Int) = IR_VariableAccess(stride_decl(bufIdx))
  def count(bufIdx : Int) = IR_VariableAccess(count_decl(bufIdx))
  def localDims(bufIdx : Int) = IR_VariableAccess(localDims_decl(bufIdx))
  def localStart(bufIdx : Int) = IR_VariableAccess(localStart_decl(bufIdx))
  def globalDims(bufIdx : Int) = IR_VariableAccess(globalDims_decl(bufIdx))
  def globalStart(bufIdx : Int) = IR_VariableAccess(globalStart_decl(bufIdx))

  /* file offsets for each databuffer. can be used to get the seekp in a file for a certain buffer */
  def fileDisplacement(bufIdx : Int) : IR_Expression = {
    IR_SimplifyExpression.simplifyIntegralExpr(dataBuffers.map(_.typicalByteSizeGlobal).take(bufIdx).reduceOption(_ + _).getOrElse(0))
  }

  // casts string variables to "const char*", string constants are unaffected
  def filenameAsCString : IR_Expression = IR_FileAccess.filenameAsCString(filename)

  /* structure of file access classes */
  def createOrOpenFile() : ListBuffer[IR_Statement]
  def setupAccess() : ListBuffer[IR_Statement]
  def fileAccess(bufIdx : Int) : ListBuffer[IR_Statement] = {
    if(writeAccess) {
      write(bufIdx)
    } else {
      read(bufIdx)
    }
  }
  def fileAccess(buffer : IR_DataBuffer) : ListBuffer[IR_Statement] = fileAccess(dataBuffers.indexOf(buffer))
  def cleanupAccess() : ListBuffer[IR_Statement]
  def closeFile() : ListBuffer[IR_Statement]

  // headers, libs and paths of each I/O interface
  def libraries : ListBuffer[String] = ListBuffer()
  def pathsLib : ListBuffer[String] = ListBuffer()
  def includes : ListBuffer[String] = ListBuffer()
  def pathsInc : ListBuffer[String] = ListBuffer()

  // determines with which mode the file is opened/created
  def fileMode : IR_VariableAccess

  // checks input parameters that were passed
  def validateParams() : Unit = {}

  // depending on the granularity of a data buffer (fragment-/blockwise), the corresponding function is chosen
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
  def accessFileBlockwise(bufIdx : Int, accessStatements : ListBuffer[IR_Statement]) : IR_Statement

  // HACK: artificial fragment loop (of length 1) to set start indices to "startIndexGlobalKJI" (which uses fragmentIdx)
  def IR_LoopOverBlocks(stmts : ListBuffer[IR_Statement]) : IR_ForLoop = IR_LoopOverBlocks(stmts : _*)
  def IR_LoopOverBlocks(stmts : IR_Statement*) = IR_ForLoop(
    IR_VariableDeclaration(IR_LoopOverFragments.defIt, 0),
    IR_Lower(IR_LoopOverFragments.defIt, 1),
    IR_PreIncrement(IR_LoopOverFragments.defIt),
    stmts.to[ListBuffer])

  // core methods for file access
  def read(bufIdx : Int) : ListBuffer[IR_Statement]
  def write(bufIdx : Int) : ListBuffer[IR_Statement]

  // add new headers, paths and libs
  def handleDependencies() : Unit = {
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
  }

  override def expand() : Output[StatementList] = {
    handleDependencies()
    validateParams()

    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    stmts ++= createOrOpenFile()
    stmts ++= setupAccess()
    for (bufIdx <- dataBuffers.indices) {
      stmts ++= fileAccess(bufIdx)
    }
    stmts ++= cleanupAccess()
    stmts ++= closeFile()

    // reset lookup tables
    IR_DataBuffer.resetDimensionalityMap()

    stmts
  }
}
