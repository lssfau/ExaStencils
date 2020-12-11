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
import exastencils.domain.ir._
import exastencils.field.ir._
import exastencils.grid.ir._
import exastencils.logger._

// IR_FileAccess
// Used to read/write field data from/to files

object IR_FileAccess {
  // prohibit redeclaration of variables for sequences of I/O statements in the same scope
  private var declMap : mutable.HashMap[String, Int] = mutable.HashMap()
  def declareVariable(s: String) : String = {
    val counter = declMap.getOrElseUpdate(s, 0)
    declMap.update(s, counter+1)
    s + "_%02d".format(counter)
  }
}

abstract class IR_FileAccess(
    filename : IR_Expression,
    field : IR_Field,
    slot : IR_Expression,
    includeGhostLayers : Boolean,
    writeAccess : Boolean,
    appendedMode : Boolean = false) extends IR_Statement with IR_Expandable {

  def numDimsGrid : Int = field.layout.numDimsGrid
  def numDimsData : Int = field.layout.numDimsData
  def numDimsDataRange : Range = (0 until numDimsData).reverse // KJI order

  def beginId : String = if (includeGhostLayers) "GLB" else "DLB"
  def endId : String = if (includeGhostLayers) "GRE" else "DRE"

  def arrayIndexRange : Range = 0 until field.gridDatatype.resolveFlattendSize

  // local/global dimensions and offsets
  // TODO: handle other domains
  // TODO: handle data reduction
  // TODO: global variable for totalNumberOfFrags and validFragsPerBlock
  def strideLocal : Array[IR_Expression] = numDimsDataRange.map (_ => IR_IntegerConstant(1)).toArray

  def innerPointsLocal : Array[IR_Expression] = if(includeGhostLayers) {
    numDimsDataRange.map(d => IR_IntegerConstant(field.layout.defIdxGhostRightEnd(d) - field.layout.defIdxGhostLeftBegin(d))).toArray
  } else {
    numDimsDataRange.map(d => IR_IntegerConstant(field.layout.defIdxDupRightEnd(d) - field.layout.defIdxDupLeftBegin(d))).toArray
  }

  def totalPointsLocal : Array[IR_Expression] = numDimsDataRange.map(d => IR_IntegerConstant(field.layout.defTotal(d))).toArray

  def startIdxLocal : Array[IR_Expression] = numDimsDataRange.map(d => IR_IntegerConstant(
    if(includeGhostLayers)
      field.layout.defIdxGhostLeftBegin(d)
    else
      field.layout.defIdxDupLeftBegin(d))
  ).toArray

  def innerPointsGlobal : Array[IR_Expression] = if(Knowledge.domain_onlyRectangular) {
    numDimsDataRange.map(d => Knowledge.domain_rect_numFragsTotalAsVec(d) * innerPointsLocal(d)).toArray
  } else { // TODO handling for other domain types
    Logger.error("Unimplemented!")
    numDimsDataRange.map(_ => IR_NullExpression).toArray
  }

  def startIdxGlobal : Array[IR_Expression] = if(Knowledge.domain_onlyRectangular) {
    numDimsDataRange.map(d => innerPointsLocal(d) * (IR_IV_FragmentIndex(d) Mod Knowledge.domain_rect_numFragsTotalAsVec(d))).toArray
  } else { // TODO handling for other domain types
    Logger.error("Unimplemented!")
    numDimsDataRange.map(_ => IR_NullExpression).toArray
  }

  // commonly used declarations
  // ...

  // commonly used variable accesses
  def mpiCommunicator = IR_VariableAccess("mpiCommunicator", IR_UnknownDatatype)
  def nullptr = IR_VariableAccess("NULL", IR_UnknownDatatype)
  def fieldptr = IR_AddressOf(IR_LinearizedFieldAccess(field, slot, IR_LoopOverFragments.defIt, 0))

  // commonly used datatypes
  val MPI_Offset : IR_SpecialDatatype = if(Knowledge.mpi_enabled) IR_SpecialDatatype("MPI_Offset") else IR_SpecialDatatype("size_t")
  val MPI_Comm = IR_SpecialDatatype("MPI_Comm")

  // TODO move to printField and accept positions as optional parameter
  def getPos(field : IR_Field, dim : Int) : IR_Expression = {
    // TODO: add function to field (layout) to decide node/cell for given dim
    field.localization match {
      case IR_AtNode              => IR_VF_NodePositionPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(numDimsGrid))
      case IR_AtCellCenter        => IR_VF_CellCenterPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(numDimsGrid))
      case IR_AtFaceCenter(`dim`) => IR_VF_NodePositionPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(numDimsGrid))
      case IR_AtFaceCenter(_)     => IR_VF_CellCenterPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(numDimsGrid))
    }
  }

  // determines whether ghost layers shall be excluded for I/O operations or not
  def accessWholeBuffer : Boolean = startIdxLocal.map(expr => expr.asInstanceOf[IR_IntegerConstant].value).sum == 0

  // structure of file accesses
  def createOrOpenFile() : ListBuffer[IR_Statement]
  def setupAccess() : ListBuffer[IR_Statement]
  def accessFile() : ListBuffer[IR_Statement] = {
    if(writeAccess) {
      writeField()
    } else {
      readField()
    }
  }
  def cleanupAccess() : ListBuffer[IR_Statement]
  def closeFile() : ListBuffer[IR_Statement]

  // headers, libs and paths of each I/O interface
  def libraries : ListBuffer[String] = ListBuffer()
  def pathsLib : ListBuffer[String] = ListBuffer()
  def includes : ListBuffer[String] = ListBuffer()
  def pathsInc : ListBuffer[String] = ListBuffer()

  override def expand() : Output[StatementList] = {
    // add new headers, paths and libs
    for(inc <- includes) {
      if(!Settings.additionalIncludes.contains(inc))
        Settings.additionalIncludes += inc
    }
    for(lib <- libraries) {
      if(!Settings.additionalLibs.contains(lib))
        Settings.additionalLibs += lib
    }
    for(pathInc <- pathsInc) {
      if(!Settings.pathsInc.contains(pathInc))
        Settings.pathsInc += pathInc
    }
    for(pathLib <- pathsLib) {
      if(!Settings.pathsLib.contains(pathLib))
        Settings.pathsLib += pathLib
    }

    validateParams()

    var stmts : ListBuffer[IR_Statement] = ListBuffer()
    stmts ++= createOrOpenFile()
    stmts ++= setupAccess()
    stmts ++= accessFile()
    stmts ++= cleanupAccess()
    stmts ++= closeFile()
    stmts
  }

  // determines with which mode the file is opened/created
  def openMode : IR_VariableAccess

  // checks input parameters that were passed
  def validateParams() : Unit = {}

  def accessFileWithGranularity(blockwiseGranularity : Boolean, accessStatements : ListBuffer[IR_Statement]) = if(blockwiseGranularity) { // TODO access Databuffer member
    accessFileBlockwise(accessStatements)
  } else {
    accessFileFragwise(accessStatements)
  }

  // method to access the file in a fragment-wise fashion
  /* IMPORTANT:
    For collective I/O, each process must participate in a read/write call. Therefore, the I/O library matches the function calls of each process (synchronization).
    In case that a process's number of valid fragments differs from the other processes, some adaptions need to be made:
      1. Instead of checking if the fragment is valid before writing, we write without any conditions. Otherwise we would deadlock (this is quite similar to a conditional MPI_Barrier).
      2. In case that we have an "invalid" fragment, we participate in the collective function call but actually write nothing.
  */
  def accessFileFragwise(accessStatements : ListBuffer[IR_Statement]) : IR_LoopOverFragments

  // method to access the file in a block-wise fashion
  def accessFileBlockwise(accessStatements : ListBuffer[IR_Statement]) : IR_Statement = IR_NullStatement // TODO

  // core methods for file access
  def readField() : ListBuffer[IR_Statement]
  def writeField() : ListBuffer[IR_Statement]
}
