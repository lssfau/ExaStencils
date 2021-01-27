package exastencils.visualization.ir

import scala.collection.mutable.ListBuffer
import scala.reflect.io.File

import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_MemberFunctionCall
import exastencils.base.ir.IR_Multiplication
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir.IR_ScalarDatatype
import exastencils.base.ir.IR_SpecialDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_StringDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.baseExt.ir.IR_UnduplicatedVariable
import exastencils.baseExt.ir.IR_VectorDatatype
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldIO
import exastencils.grid.ir.IR_AtCellCenter
import exastencils.grid.ir.IR_AtFaceCenter
import exastencils.grid.ir.IR_AtNode
import exastencils.grid.ir.IR_Localization
import exastencils.grid.ir.IR_VF_CellCenterPerDim
import exastencils.grid.ir.IR_VF_NodePositionPerDim
import exastencils.io.ir.IR_IV_FragmentOffset
import exastencils.io.ir.IR_IV_NumValidFrags
import exastencils.io.ir.IR_IV_TemporaryBuffer
import exastencils.io.ir.IR_IV_TotalNumFrags
import exastencils.logger.Logger

// determines whether constants were already written to file or not
// name of the file containing the constant data can be queried here
case class IR_IV_ConstantsWrittenToFile(id : Int) extends IR_UnduplicatedVariable {
  override def resolveName() : String = "constantsWrittenToFile" + id
  override def resolveDatatype() : IR_Datatype = IR_StringDatatype
  override def resolveDefValue() : Option[IR_Expression] = Some("\"\"")

  def setFilename(basename : IR_Expression, extension : Option[IR_Expression] = None) : IR_Assignment = {
    val rhs = if (extension.isEmpty) {
      basename
    } else {
      (basename, extension.get) match {
        case (sc : IR_StringConstant, sc2 : IR_StringConstant) => IR_StringConstant(sc.value + sc2.value)
        case (base : IR_Expression, ext : IR_Expression)       => base + ext
      }
    }
    IR_Assignment(this, rhs)
  }
  def isEmpty : IR_Expression = !Knowledge.parIO_constantDataReduction OrOr IR_MemberFunctionCall(IR_VariableAccess(resolveName(), resolveDatatype()), "empty")
}

/// IR_PrintVisualization
// provides general functions and structure for visualization interfaces
// resolves function calls for visualization in the DSL

trait IR_PrintVisualization {
  // remove file extension
  def extRegex = "[.][^.]+$"
  def lastIdxSubst(vAcc : IR_VariableAccess, pattern : String) = IR_MemberFunctionCall(vAcc, "find_last_of", pattern)

  def ext : IR_Expression = filename match {
    case sc : IR_StringConstant                                         => IR_StringConstant(extRegex.r findFirstIn sc.value getOrElse "")
    case vAcc : IR_VariableAccess if vAcc.datatype == IR_StringDatatype => IR_MemberFunctionCall(vAcc, "substr", lastIdxSubst(vAcc,"\".\""))
    case _                                                              => Logger.error("Parameter \"filename\" is not a string.")
  }

  // specifies if data is stored fragment-by-fragment or canonically
  // canonical layout: data is stored as if the domain was never decomposed
  def canonicalFileLayout : Boolean

  // generates the basename either as StringConstant or as expressions which extract the basename in the target code (via "substr")
  def basename(noPath : Boolean, appStr : Option[IR_StringConstant] = None) : IR_Expression = filename match {
    case sc : IR_StringConstant                                          =>
      val bnConst = if(noPath) {
        sc.value.substring(sc.value.lastIndexOf(File.separator) + 1).replaceFirst(extRegex, "")
      } else {
        sc.value.replaceFirst(extRegex, "")
      }
      if(appStr.isDefined) IR_StringConstant(bnConst + appStr.get.value) else IR_StringConstant(bnConst)
    case vAcc : IR_VariableAccess if vAcc.datatype == IR_StringDatatype =>
      val bnExpr = if(noPath) {
        IR_MemberFunctionCall(
          IR_MemberFunctionCall(vAcc, "substr", 0, lastIdxSubst(vAcc, "\".\"")), // remove extension
          "substr", lastIdxSubst(vAcc, "\"\\\\/\"") + 1) // remove path
      } else {
        IR_MemberFunctionCall(vAcc, "substr", 0, lastIdxSubst(vAcc, "\".\""))
      }
      if(appStr.isDefined) bnExpr + appStr.get else bnExpr
    case _ =>
      Logger.error("Parameter \"filename\" is not a string.")
  }

  /* attributes to be implemented in application.ir or field.ir */

  def filename : IR_Expression
  def ioInterface : String

  def numDimsGrid : Int
  def numFields : Int

  def newStream = IR_VariableAccess(IR_FieldIO.getNewStreamName(), IR_SpecialDatatype("std::ofstream"))

  def level : Int

  // create a new instance of a the IV for each visualization function that is resolved
  def resolveId : Int
  def IR_ConstantsWrittenToFile() = IR_IV_ConstantsWrittenToFile(resolveId)

  def connectivityStartIndex : Int = 0 // start index when initializing the connectivity buffer. for exodus: "1"
  def connectivityForCell(global : Boolean = true) : ListBuffer[IR_Expression] // contains expressions to describe a mesh's connectivity list (e.g. 4 expressions for a quad)

  def dimsConnectivityFrag : ListBuffer[IR_IntegerConstant] = ListBuffer(connectivityForCell().length, numCells_x, numCells_y, numCells_z)
  def dimsPositionsFrag    : ListBuffer[IR_Expression] = (loopOverNodes().indices.end - loopOverNodes().indices.begin).indices.to[ListBuffer] // number of node positions stored per fragment, e.g. for non-reduced SWE: 6 * numCells_x * numCells_y

  def numCellsPerFrag : IR_Expression
  def numPointsPerFrag : IR_Expression = dimsPositionsFrag.reduce(_ * _)

  def numFrags : IR_Expression = IR_IV_TotalNumFrags(domainIndex)
  def numFragsPerBlock : IR_Expression = IR_IV_NumValidFrags(domainIndex)
  def fragmentOffset : IR_Expression = IR_IV_FragmentOffset(domainIndex)

  def numCells_x : Int
  def numCells_y : Int
  def numCells_z : Int

  def numCells : IR_Expression = numCellsPerFrag * numFrags
  def numNodes : IR_Multiplication = numPointsPerFrag * numFrags

  def someCellField : IR_Field // required as base for setting up iteration spaces later
  def domainIndex : Int = someCellField.domain.index

  def nodeOffsets : ListBuffer[IR_ConstIndex] // essentially only for SWE, but used here to reduce duplicate code

  // access a single component of a higher dim. datatype
  def accessComponent(componentIndex : Int, gridDatatype : IR_Datatype, startIdx : ListBuffer[IR_Expression]) : ListBuffer[IR_Expression] = gridDatatype match {
    case mat : IR_MatrixDatatype =>
      val r = componentIndex / mat.sizeN
      val c = componentIndex % mat.sizeN
      startIdx :+ IR_IntegerConstant(r) :+ IR_IntegerConstant(c)
    case _ : IR_VectorDatatype =>
      startIdx :+ IR_IntegerConstant(componentIndex)
    case _ : IR_ScalarDatatype   =>
      startIdx
    case _                       =>
      Logger.error("Unknown grid datatype used: " + gridDatatype.prettyprint)
  }


  /* temp. buffer (for node pos./connectivity/...) helpers */

  // get the coordinates for a certain grid localization
  def getPos(localization : IR_Localization, level: Int, dim : Int, index : IR_ExpressionIndex = IR_LoopOverDimensions.defIt(numDimsGrid)) : IR_Expression = {
    localization match {
      case IR_AtNode              => IR_VF_NodePositionPerDim.access(level, dim, index)
      case IR_AtCellCenter        => IR_VF_CellCenterPerDim.access(level, dim, index)
      case IR_AtFaceCenter(`dim`) => IR_VF_NodePositionPerDim.access(level, dim, index)
      case IR_AtFaceCenter(_)     => IR_VF_CellCenterPerDim.access(level, dim, index)
    }
  }

  // loop over dims used to copy data to temp. buffers (e.g. position nodes/cell centers/...)
  def loopOverNodes(initBuffer : ListBuffer[IR_Statement] = ListBuffer()) : IR_LoopOverDimensions = loopOverDims(nodalLoopEnd = true, initBuffer)
  def loopOverCells(initBuffer : ListBuffer[IR_Statement] = ListBuffer()) : IR_LoopOverDimensions = loopOverDims(nodalLoopEnd = false, initBuffer)
  def loopOverDims(nodalLoopEnd : Boolean, initBuffer : ListBuffer[IR_Statement] = ListBuffer()) : IR_LoopOverDimensions = {
    val loopEnd = (0 until numDimsGrid).toArray.map(dim => (Knowledge.domain_fragmentLengthAsVec(dim) * (1 << level)) + (if (nodalLoopEnd) 1 else 0))
    IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
      IR_ExpressionIndex(Array.fill(numDimsGrid)(0 : IR_Expression)),
      IR_ExpressionIndex(loopEnd)),
      initBuffer)
  }

  def connectivityBuf = IR_IV_TemporaryBuffer(IR_IntegerDatatype, IR_AtCellCenter, "connectivity", domainIndex, ListBuffer() ++ dimsConnectivityFrag)

  // allocates and initializes buffer with connectivity info. this buffer is then passed to the I/O library
  def setupConnectivity(global : Boolean) : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()
    val sizeConnectionFrag = dimsConnectivityFrag.reduce((a, b) => a.v * b.v)

    stmts += connectivityBuf.allocateMemory

    val initBuffer : ListBuffer[IR_Statement] = connectivityForCell().indices.map(d => {
      val linearizedLoopIdx = loopOverCells().indices.linearizeIndex(IR_LoopOverDimensions.defIt(numDimsGrid))
      IR_Assignment(
        connectivityBuf.at(IR_LoopOverFragments.defIt * sizeConnectionFrag + connectivityForCell().length * linearizedLoopIdx + d),
        connectivityForCell(global)(d)) : IR_Statement
    }).to[ListBuffer]

    stmts += IR_IfCondition(IR_ConstantsWrittenToFile().isEmpty,
      IR_LoopOverFragments(
      IR_IfCondition(IR_IV_IsValidForDomain(domainIndex),
        loopOverCells(initBuffer))))

    stmts
  }

  val gridPositionsCopied : Boolean = Knowledge.grid_isAxisAligned || Knowledge.grid_isUniform // otherwise we directly use a vf's associated field
  def cellCentersBuf : ListBuffer[IR_IV_TemporaryBuffer] = (0 until numDimsGrid).to[ListBuffer].map { d =>
    val dims = (loopOverCells().indices.end - loopOverCells().indices.begin).indices.to[ListBuffer]
    IR_IV_TemporaryBuffer(IR_RealDatatype, IR_AtCellCenter, "cellCenter" + ('X' + d).toChar.toString, domainIndex, dims)
  }

  // on some occasions, the virtual field can be directly passed to the library. this buffer is then passed to the I/O library
  def initCellCenterBuf(dim : Int, copyNodePositions : Boolean = gridPositionsCopied) : ListBuffer[IR_Statement] = if (!copyNodePositions) {
    ListBuffer()
  } else {
    val indexRange = loopOverCells().indices
    val dims = (indexRange.end - indexRange.begin).indices.to[ListBuffer]
    val linearizedLoopIdx = indexRange.linearizeIndex(IR_LoopOverDimensions.defIt(numDimsGrid))
    val init = IR_Assignment(
      cellCentersBuf(dim).at(IR_LoopOverFragments.defIt * dims.reduce(_ * _) + linearizedLoopIdx),
      getPos(IR_AtCellCenter, level, dim))

    // declare, allocate and init temp. buffer with cell centers
    ListBuffer[IR_Statement](
      cellCentersBuf(dim).allocateMemory,
      IR_IfCondition(IR_ConstantsWrittenToFile().isEmpty,
        IR_LoopOverFragments(
          IR_IfCondition(IR_IV_IsValidForDomain(domainIndex),
            loopOverCells(ListBuffer(init))))))
  }

  // allocates and initializes buffer with cell-center positions on-demand.
  def setupCellCenters(copyNodePositions : Boolean = gridPositionsCopied) : ListBuffer[IR_Statement] =
    (0 until numDimsGrid).flatMap(initCellCenterBuf(_, copyNodePositions)).to[ListBuffer]

  def getFaceDir(localization: IR_Localization) : Int = {
    (0 until numDimsGrid).collectFirst { case d if IR_AtFaceCenter(d) == localization => d } getOrElse -1
  }

  def facePositionsBuf(faceDir : Int) : ListBuffer[IR_IV_TemporaryBuffer] = {
    val indexRange = Duplicate(loopOverCells().indices)
    indexRange.end(faceDir) += 1
    val dims = (indexRange.end - indexRange.begin).indices.to[ListBuffer]

    (0 until numDimsGrid).map(d => IR_IV_TemporaryBuffer(IR_RealDatatype, IR_AtFaceCenter(faceDir), "facePos" + ('X' + d).toChar.toString, domainIndex, dims)).to[ListBuffer]
  }

  def initFacePosBuf(faceDir : Int)(dim : Int) : ListBuffer[IR_Statement] = {
    val indexRange = Duplicate(loopOverCells().indices)
    indexRange.end(faceDir) += 1
    val dims = (indexRange.end - indexRange.begin).indices.to[ListBuffer]
    val linearizedLoopIdx = indexRange.linearizeIndex(IR_LoopOverDimensions.defIt(numDimsGrid))

    // declare, allocate and init temp. buffer with face positions
    ListBuffer[IR_Statement](
      facePositionsBuf(faceDir)(dim).allocateMemory,
      IR_IfCondition(IR_ConstantsWrittenToFile().isEmpty,
        IR_LoopOverFragments(
          IR_IfCondition(IR_IV_IsValidForDomain(domainIndex),
            IR_LoopOverDimensions(numDimsGrid,
              indexRange,
              IR_Assignment(
                facePositionsBuf(faceDir)(dim).at(IR_LoopOverFragments.defIt * dims.reduce(_ * _) + linearizedLoopIdx),
                getPos(IR_AtFaceCenter(faceDir), level, dim)))))))
  }

  // allocates and initializes buffer with face positions on-demand
  def setupFacePositions(faceDir : Int) : ListBuffer[IR_Statement] = (0 until numDimsGrid).flatMap(initFacePosBuf(faceDir)).to[ListBuffer]

  def nodePositionsBuf : ListBuffer[IR_IV_TemporaryBuffer] = (0 until numDimsGrid).to[ListBuffer].map { d =>
    IR_IV_TemporaryBuffer(IR_RealDatatype, IR_AtNode, "nodePosition" + ('X' + d).toChar.toString, domainIndex, dimsPositionsFrag)
  }

  def initNodePosBuf(dim : Int, copyNodePositions : Boolean = gridPositionsCopied) : ListBuffer[IR_Statement] = if (!copyNodePositions) {
    ListBuffer() // use vf's associated field directly
  } else {
    val numAccPerCell = nodeOffsets.length // for non-reduced SWE "6", otherwise "1"
    val isNodalLoop = Knowledge.swe_nodalReductionPrint || numAccPerCell == 1 // special case for SWE: cell loop (6 accesses each) for non-reduced SWE

    val initBuffer : ListBuffer[IR_Statement] = (0 until numAccPerCell).map(n => {
      val linearizedLoopIdx = loopOverDims(isNodalLoop).indices.linearizeIndex(IR_LoopOverDimensions.defIt(numDimsGrid))
      IR_Assignment(
        nodePositionsBuf(dim).at(IR_LoopOverFragments.defIt * numPointsPerFrag + numAccPerCell * linearizedLoopIdx + n),
        getPos(IR_AtNode, level, dim, IR_LoopOverDimensions.defIt(numDimsGrid) + nodeOffsets(n))) : IR_Statement
    }).to[ListBuffer]

    // declare, allocate and init temp. buffer with node position
    ListBuffer[IR_Statement](
      nodePositionsBuf(dim).allocateMemory,
      IR_IfCondition(IR_ConstantsWrittenToFile().isEmpty,
        IR_LoopOverFragments(
          IR_IfCondition(IR_IV_IsValidForDomain(domainIndex),
            loopOverDims(isNodalLoop, initBuffer)))))
  }

  // allocates and initializes buffer with the node positions on-demand
  def setupNodePositions(copyNodePositions : Boolean = gridPositionsCopied) : ListBuffer[IR_Statement] =
    (0 until numDimsGrid).flatMap(initNodePosBuf(_, copyNodePositions)).to[ListBuffer]
}
