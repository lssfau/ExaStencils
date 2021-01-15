package exastencils.io.ir

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.config.Settings
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir.StatementList
import exastencils.grid.ir.IR_Localization
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.util.ir.IR_Print
import exastencils.util.ir.IR_PrintBinary
import exastencils.util.ir.IR_PrintBlockBinary
import exastencils.util.ir.IR_Read
import exastencils.util.ir.IR_ReadBinary
import exastencils.util.ir.IR_ReadBlockBinary

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

  // reduce the number of duplicate declarations in the target code for identical dimensionalities
  private val dimensionalityMap : mutable.HashMap[String, IR_VariableDeclaration] = mutable.HashMap()
  def declareDimensionality(dt : IR_Datatype, name : String, localization : IR_Localization, dims : Option[ListBuffer[IR_Expression]] = None) : IR_VariableDeclaration = {
    val declName = name + localization.name
    val lookup = dt.resolveBaseDatatype.prettyprint + declName + (if (dims.isDefined) dims.get.hashCode() else 0).toString
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

  /* helpers to declare dimensionalities */
  def declareDimensionality(name : String, localization: IR_Localization, dims : ListBuffer[IR_Expression], datatype : IR_Datatype = datatypeDimArray) : IR_VariableDeclaration = {
    // cast dims when used in initializer list, otherwise "Wnarrowing" warning
    val castDims = dims.map {
      case vAcc : IR_VariableAccess if vAcc.datatype != datatype => IR_Cast(datatype, vAcc)
      case iv : IR_InternalVariable if iv.resolveDatatype() != datatype => IR_Cast(datatype, iv)
      case expr : IR_Expression => expr
    }
    IR_FileAccess.declareDimensionality(IR_ArrayDatatype(datatype, dims.length), name, localization, Some(castDims))
  }

  /* helper function to get access indices for multidim. datatypes */
  def getIndicesMultiDimDatatypes(buf : IR_DataBuffer) : Array[IR_Index] = {
    if(buf.numDimsData > buf.numDimsGrid) {
      buf.datatype match {
        case mat : IR_MatrixDatatype =>
          Array.range(0, mat.sizeM).flatMap(rows =>
            Array.range(0, mat.sizeN).map(cols =>
              IR_ExpressionIndex(IR_LoopOverDimensions.defIt(buf.numDimsGrid).indices :+ IR_IntegerConstant(rows) :+ IR_IntegerConstant(cols))))
        case _ : IR_ScalarDatatype   =>
          Array(IR_LoopOverDimensions.defIt(buf.numDimsGrid))
        case _                       =>
          Logger.error("Unsupported higher dimensional datatype used for I/O interface.")
      }
    } else {
      Array(IR_LoopOverDimensions.defIt(buf.numDimsGrid))
    }
  }

  // helper function to handle accesses for multidim. datatypes
  def handleAccessesMultiDimDatatypes(buf : IR_DataBuffer) : ListBuffer[IR_Access] = {
    // return access list with last separator removed
    getIndicesMultiDimDatatypes(buf).map(idx => buf.getAccess(idx)).to[ListBuffer]
  }

  /* helper functions for I/O operations using C++ STL I/O streams */
  def loopOverDims(bufIdx : Int, condition : IR_Expression, accessStatements : IR_Statement*) : IR_LoopOverDimensions = {
    val buffer = dataBuffers(bufIdx)
    IR_LoopOverDimensions(buffer.numDimsGrid, IR_ExpressionIndexRange(
      IR_ExpressionIndex(buffer.numDimsGridRange.map(dim => buffer.beginIndices(dim) - Duplicate(buffer.referenceOffset(dim)) : IR_Expression).toArray),
      IR_ExpressionIndex(buffer.numDimsGridRange.map(dim => buffer.endIndices(dim) - Duplicate(buffer.referenceOffset(dim)) : IR_Expression).toArray)),
      IR_IfCondition(condition, accessStatements.to[ListBuffer]))
  }

  def printBufferAscii(
      bufIdx : Int,
      stream : IR_VariableAccess,
      condition : IR_Expression,
      separator : IR_Expression,
      optPrintComponents : Option[ListBuffer[IR_Expression]] = None,
      indent : Option[IR_Expression] = None) : IR_ScopedStatement = {

    val buf = dataBuffers(bufIdx)

    val printComponents = optPrintComponents getOrElse ListBuffer[IR_Expression]()
    printComponents += "std::scientific"
    printComponents ++= indent
    printComponents ++= handleAccessesMultiDimDatatypes(buf).flatMap(acc => List(acc, separator)).dropRight(1)
    printComponents += IR_Print.newline
    loopOverDims(bufIdx, condition, IR_Print(stream, printComponents))
  }

  def printBufferBinary(bufIdx : Int, stream : IR_VariableAccess, condition : IR_Expression) : IR_ScopedStatement = {
    val buf = dataBuffers(bufIdx)
    val bytesAccessedKnownApriori = condition == IR_BooleanConstant(true) // if there is no condition -> required number of accessed bytes are known

    IR_IfCondition(bytesAccessedKnownApriori AndAnd buf.accessWithoutExclusion,
      /* true: write whole buffer */
      IR_PrintBlockBinary(stream, buf.getBaseAddress, buf.typicalByteSizeLocal),
      /* false: write component by component in a loop */
      loopOverDims(bufIdx, condition, IR_PrintBinary(stream, handleAccessesMultiDimDatatypes(buf))))
  }

  def readBufferAscii(
      bufIdx : Int,
      stream : IR_VariableAccess,
      condition : IR_Expression,
      skipSep : Option[IR_VariableAccess] = None) : IR_ScopedStatement = {

    val buf = dataBuffers(bufIdx)

    // handle accesses of high dim datatypes
    val acc = handleAccessesMultiDimDatatypes(buf).flatMap(acc => List(acc) ++ skipSep)

    loopOverDims(bufIdx, condition, IR_Read(stream, (if (skipSep.isDefined) acc.dropRight(1) else acc) : _*)) // cond. remove sep at end
  }

  def readBufferBinary(bufIdx : Int, stream : IR_VariableAccess, condition : IR_Expression) : IR_ScopedStatement = {
    val buf = dataBuffers(bufIdx)
    val bytesAccessedKnownApriori = condition == IR_BooleanConstant(true) // if there is no condition -> required number of accessed bytes are known

    IR_IfCondition(bytesAccessedKnownApriori AndAnd buf.accessWithoutExclusion,
      /* true: read whole buffer */
      IR_ReadBlockBinary(stream, buf.getBaseAddress, buf.typicalByteSizeLocal),
      /* false: read component by component in a loop */
      loopOverDims(bufIdx, condition, IR_ReadBinary(stream, handleAccessesMultiDimDatatypes(buf))))
  }

  /* commonly used declarations. mainly for dimensionality extents */
  def stride_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => {
    declareDimensionality("stride", buf.localization,
      IR_DataBuffer.handleFragmentDimension(buf, buf.strideKJI,
        fragmentDim = 1))
  })
  def count_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => {
    declareDimensionality("count", buf.localization,
      IR_DataBuffer.handleFragmentDimension(buf, buf.innerDimsLocalKJI,
        fragmentDim = if (buf.accessBlockwise) IR_IV_NumValidFrags(buf.domainIdx) else 1))
  })
  def localDims_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => {
    declareDimensionality("localDims", buf.localization,
      IR_DataBuffer.handleFragmentDimension(buf, buf.totalDimsLocalKJI,
        fragmentDim = if (buf.accessBlockwise) IR_IV_NumValidFrags(buf.domainIdx) else 1))
  })
  def localStart_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => {
    declareDimensionality("localStart", buf.localization,
      IR_DataBuffer.handleFragmentDimension(buf, buf.startIndexLocalKJI,
        fragmentDim = 0))
  })
  def globalDims_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => {
    declareDimensionality("globalDims", buf.localization, buf.globalDimsKJI)
  })
  def globalStart_decl : ListBuffer[IR_VariableDeclaration] = dataBuffers.map(buf => {
    declareDimensionality("globalStart", buf.localization, ListBuffer.fill(numDimsGlobal(buf))(IR_IntegerConstant(0)))
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

  /* determine number of dims (numDimsData does not work since the data can be laid out canonically or fragment-wise -> differences in dimensionality) */
  def numDimsLocal(bufIdx : Int) : Int = numDimsLocal(dataBuffers(bufIdx))
  def numDimsGlobal(bufIdx : Int) : Int = numDimsGlobal(dataBuffers(bufIdx))
  def numDimsLocal(buf : IR_DataBuffer) : Int = if (!buf.canonicalOrder) numDimsGlobal(buf) else buf.startIndexLocalKJI.length
  def numDimsGlobal(buf : IR_DataBuffer) : Int = buf.globalDimsKJI.length

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
  def openMode : IR_VariableAccess

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
