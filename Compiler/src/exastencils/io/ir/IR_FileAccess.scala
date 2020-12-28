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
    val lookup = declName + (if (dims.isDefined) dims.get.hashCode() else 0).toString
    dimensionalityMap.getOrElseUpdate(
      lookup,
      IR_VariableDeclaration(dt, declareVariable(declName), if(dims.isDefined) Some(IR_InitializerList(dims.get : _*)) else None)) // dims already specified in KJI order
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
  def datatypeDimArray : IR_Datatype = interfaceName match {
    case "mpiio" => IR_IntegerDatatype
    case "nc"    => MPI_Offset
    case "hdf5"  => IR_SpecialDatatype("hsize_t")
    case _ =>
      Logger.warn("IR_FileAccess: Unknown I/O interface used for \"datatypeDimArray\"")
      IR_UnknownDatatype
  }
  def mpiDatatypeBuffer(buf : IR_DataBuffer) = IR_VariableAccess(buf.datatype.resolveBaseDatatype.prettyprint_mpi, IR_UnknownDatatype)

  // helpers to declare dimensionalities
  protected def declareDimensionality(name : String, localization: IR_Localization, dims : ListBuffer[IR_Expression], datatype : IR_Datatype = datatypeDimArray) : IR_VariableDeclaration = {
    IR_FileAccess.declareDimensionality(IR_ArrayDatatype(datatype, dims.length), name, localization, Some(dims))
  }
  // output data fragment-after-fragment -> additional dimensionality in this case
  protected def handleFragmentDimension(buf : IR_DataBuffer, dims : ListBuffer[IR_Expression], fragmentDim : IR_Expression) : ListBuffer[IR_Expression] = {
    if (!buf.canonicalOrder) {
      fragmentDim +: (if (buf.accessBlockwise) dims.drop(1) else dims)
    } else {
      dims
    }
  }

  // helper function to handle accesses for hodt
  def handleAccessesHodt(buf : IR_DataBuffer) : ListBuffer[IR_Access] = {
    val indices = if(buf.numDimsData > buf.numDimsGrid) {
      buf.datatype match {
        case mat : IR_MatrixDatatype =>
          Array.range(0, mat.sizeM).flatMap(rows =>
            Array.range(0, mat.sizeN).map(cols =>
              IR_ExpressionIndex(IR_LoopOverDimensions.defIt(buf.numDimsGrid).indices :+ IR_IntegerConstant(rows) :+ IR_IntegerConstant(cols))))
        case _ : IR_ScalarDatatype   =>
          Array(IR_LoopOverDimensions.defIt(buf.numDimsData))
        case _                       =>
          Logger.error("Unsupported higher dimensional datatype used for I/O interface.")
      }
    } else {
      Array(IR_LoopOverDimensions.defIt(buf.numDimsData))
    }

    // return access list with last separator removed
    indices.map(idx => buf.getAccess(idx)).to[ListBuffer]
  }

  // commonly used declarations
  def stride_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => {
    declareDimensionality("stride", buf.localization,
      handleFragmentDimension(buf, buf.strideKJI,
        fragmentDim = 1))
  })
  def count_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => {
    declareDimensionality("count", buf.localization,
      handleFragmentDimension(buf, buf.innerDimsLocalKJI,
        fragmentDim = if (buf.accessBlockwise) IR_IV_NumValidFrags(buf.domainIdx) else 1))
  })
  def localDims_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => {
    declareDimensionality("localDims", buf.localization,
      handleFragmentDimension(buf, buf.totalDimsLocalKJI,
        fragmentDim = if (buf.accessBlockwise) IR_IV_NumValidFrags(buf.domainIdx) else 1))
  })
  def localStart_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => {
    declareDimensionality("localStart", buf.localization,
      handleFragmentDimension(buf, buf.startIndexLocalKJI,
        fragmentDim = 0))
  })
  def globalDims_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => {
    declareDimensionality("globalDims", buf.localization, buf.globalDimsKJI)
  })
  def globalStart_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => {
    declareDimensionality("globalStart", buf.localization, ListBuffer.fill(numDimsGlobal(buf))(IR_IntegerConstant(0)))
  })

  // collection with declarations of dim. extents
  def dimensionalityDeclarations : ListBuffer[IR_VariableDeclaration] = dataBuffers.indices.flatMap(idx => {
    stride_decl(idx) :: count_decl(idx) :: localDims_decl(idx) :: localStart_decl(idx) :: globalDims_decl(idx) :: globalStart_decl(idx) :: Nil
  }).distinct.to[ListBuffer]

  // commonly used variable accesses
  val mpiCommunicator = IR_VariableAccess("mpiCommunicator", IR_UnknownDatatype)
  def stride(bufIdx : Int) = IR_VariableAccess(stride_decl(bufIdx))
  def count(bufIdx : Int) = IR_VariableAccess(count_decl(bufIdx))
  def localDims(bufIdx : Int) = IR_VariableAccess(localDims_decl(bufIdx))
  def localStart(bufIdx : Int) = IR_VariableAccess(localStart_decl(bufIdx))
  def globalDims(bufIdx : Int) = IR_VariableAccess(globalDims_decl(bufIdx))
  def globalStart(bufIdx : Int) = IR_VariableAccess(globalStart_decl(bufIdx))

  // file offsets for each databuffer
  def fileDisplacement(bufIdx : Int) : IR_Expression = {
    IR_SimplifyExpression.simplifyIntegralExpr(dataBuffers.map(_.typicalByteSize(global = true)).take(bufIdx).reduceOption(_ + _).getOrElse(0))
  }

  def filenameAsCString : IR_Expression = filename match {
    case sc : IR_StringConstant                                         => sc
    case vAcc : IR_VariableAccess if vAcc.datatype == IR_StringDatatype => IR_MemberFunctionCall(vAcc, "c_str")
    case _                                                              => Logger.error("Wrong datatype passed for parameter \"filename\" to IR_FileAccess. Should be a \"String\" instead of:" + _)
  }

  // determine number of dims (numDimsData does not work since the data can be laid out canonically or fragment-wise -> differences in dimensionality)
  def numDimsLocal(bufIdx : Int) : Int = numDimsLocal(dataBuffers(bufIdx))
  def numDimsGlobal(bufIdx : Int) : Int = numDimsGlobal(dataBuffers(bufIdx))
  def numDimsLocal(buf : IR_DataBuffer) : Int = if (!buf.canonicalOrder) numDimsGlobal(buf) else buf.startIndexLocalKJI.length
  def numDimsGlobal(buf : IR_DataBuffer) : Int = buf.globalDimsKJI.length

  // structure of file access classes
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
  def accessFileBlockwise(bufIdx : Int, accessStatements : ListBuffer[IR_Statement]) : IR_Statement

  // HACK: artificial fragment loop (of length 1) to set start indices to "startIndexGlobalKJI" (which uses fragmentIdx)
  def IR_LoopOverBlocks(stmts : ListBuffer[IR_Statement]) = IR_ForLoop(
    IR_VariableDeclaration(IR_LoopOverFragments.defIt, 0),
    IR_Lower(IR_LoopOverFragments.defIt, 1),
    IR_PreIncrement(IR_LoopOverFragments.defIt),
    stmts)

  // core methods for file access
  def read(bufIdx : Int) : ListBuffer[IR_Statement]
  def write(bufIdx : Int) : ListBuffer[IR_Statement]

  lazy val statementList : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    stmts ++= createOrOpenFile()
    stmts ++= setupAccess()
    for (bufIdx <- dataBuffers.indices) {
      stmts ++= fileAccess(bufIdx)
    }
    stmts ++= cleanupAccess()
    stmts ++= closeFile()

    stmts
  }

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

    val ret = statementList

    // reset lookup tables
    IR_FileAccess.resetDimensionalityMap()

    ret
  }
}
