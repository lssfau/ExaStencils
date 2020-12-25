package exastencils.visualization.ir

import scala.collection.mutable.ListBuffer
import scala.reflect.io.File

import exastencils.applications.ns.ir.IR_PrintVtkNNF
import exastencils.applications.ns.ir.IR_PrintVtkNS
import exastencils.applications.ns.ir.IR_PrintXdmfNNF
import exastencils.applications.ns.ir.IR_PrintXdmfNS
import exastencils.applications.swe.ir.IR_PrintVtkSWE
import exastencils.applications.swe.ir.IR_PrintXdmfSWE
import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_BooleanConstant
import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_ExpressionStatement
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_MemberFunctionCall
import exastencils.base.ir.IR_Multiplication
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir.IR_SpecialDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_StringDatatype
import exastencils.base.ir.IR_UnresolvedFunctionReference
import exastencils.base.ir.IR_VariableAccess
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.baseExt.ir.IR_UnduplicatedVariable
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldIO
import exastencils.grid.ir.IR_VF_NodePositionPerDim
import exastencils.io.ir.IR_IV_FragmentOffset
import exastencils.io.ir.IR_IV_NumValidFrags
import exastencils.io.ir.IR_IV_TemporaryBuffer
import exastencils.io.ir.IR_IV_TotalNumFrags
import exastencils.logger.Logger

// determines whether constants were already written to file or not
case class IR_IV_ConstantsWrittenToFile() extends IR_UnduplicatedVariable {
  override def resolveName() : String = "constantsWrittenToFile"
  override def resolveDatatype() : IR_Datatype = IR_StringDatatype
  override def resolveDefValue() : Option[IR_Expression] = Some("\"\"")
}

/// IR_PrintVisualization
// provides general functions and structure for visualization interfaces
// resolves function calls for visualization in the DSL

trait IR_PrintVisualization {
  def filename : IR_Expression
  def ioInterface : String

  // remove file extension
  def extRegex = "[.][^.]+$"
  def lastIdxSubst(vAcc : IR_VariableAccess, pattern : String) = IR_MemberFunctionCall(vAcc, "find_last_of", pattern)

  def ext : IR_Expression = filename match {
    case sc : IR_StringConstant                                         => IR_StringConstant(extRegex.r findFirstIn sc.value getOrElse "")
    case vAcc : IR_VariableAccess if vAcc.datatype == IR_StringDatatype => IR_MemberFunctionCall(vAcc, "substr", lastIdxSubst(vAcc,"\".\""))
    case _                                                              => Logger.error("Parameter \"filename\" is not a string.")
  }

  // generates the basename either as StringConstant or as expressions which extract the basename in the target code (via "substr")
  def basename(noPath : Boolean, appStr : Option[IR_StringConstant] = None) : IR_Expression = filename match {
    case sc : IR_StringConstant                                          =>
      val bnConst = IR_StringConstant(if(noPath) {
        sc.value.substring(sc.value.lastIndexOf(File.separator) + 1).replaceFirst(extRegex, "")
      } else {
        sc.value.replaceFirst(extRegex, "")
      })
      if(appStr.isDefined) bnConst + appStr.get else bnConst
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

  def numDimsGrid : Int
  def numFields : Int

  def newStream = IR_VariableAccess(IR_FieldIO.getNewStreamName(), IR_SpecialDatatype("std::ofstream"))

  def level : Int

  def connectivityForCell(global : Boolean = true) : ListBuffer[IR_Expression] // contains expressions to describe a mesh's connectivity list (e.g. 4 expressions for a quad)

  def dimsConnectivityFrag : ListBuffer[IR_IntegerConstant] = ListBuffer(connectivityForCell().length, numCells_x, numCells_y, numCells_z)
  def dimsPositionsFrag    : ListBuffer[IR_IntegerConstant]

  def numCellsPerFrag : Int
  def numPointsPerFrag : Long = dimsPositionsFrag.foldLeft(1L)((a, b) => a.v * b.v)

  def numFrags : IR_Expression = IR_IV_TotalNumFrags(someCellField.domain.index)
  def numFragsPerBlock : IR_Expression = IR_IV_NumValidFrags(someCellField.domain.index)
  def fragmentOffset : IR_Expression = IR_IV_FragmentOffset(someCellField.domain.index)

  def numCells_x : Int
  def numCells_y : Int
  def numCells_z : Int

  def numCells : IR_Expression = numCellsPerFrag * numFrags
  def numNodes : IR_Multiplication = numPointsPerFrag * numFrags

  def someCellField : IR_Field // required as base for setting up iteration spaces later

  def nodeOffsets : ListBuffer[IR_ConstIndex] // essentially only for SWE, but used here to reduce duplicate code

  def loopOverInnerDims(nodalLoopEnd : Boolean, initBuffer : ListBuffer[IR_Statement] = ListBuffer()) = IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
    IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DLB", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression)),
    IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => (if(nodalLoopEnd) 1 else 0) + someCellField.layout.idxById("DRE", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression))),
    initBuffer)

  def connectivityBuf = IR_IV_TemporaryBuffer(IR_IntegerDatatype, "connectivity", someCellField.domain.index, ListBuffer() ++ dimsConnectivityFrag)

  // allocates and initializes buffer with connectivity info. this buffer is then passed to the I/O library
  def setupConnectivity : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()
    val sizeConnectionFrag = dimsConnectivityFrag.reduce((a, b) => a.v * b.v)

    stmts += connectivityBuf.getDeclaration()
    stmts += connectivityBuf.allocateMemory

    val initBuffer : ListBuffer[IR_Statement] = connectivityForCell().indices.map(d => {
      val linearizedLoopIdx = loopOverInnerDims(nodalLoopEnd = false).indices.linearizeIndex(IR_LoopOverDimensions.defIt(numDimsGrid))
      IR_Assignment(
        connectivityBuf.at(IR_LoopOverFragments.defIt * sizeConnectionFrag + connectivityForCell().length * linearizedLoopIdx + d),
        connectivityForCell()(d)) : IR_Statement
    }).to[ListBuffer]

    stmts += IR_LoopOverFragments(
      IR_IfCondition(IR_IV_IsValidForDomain(someCellField.domain.index),
        loopOverInnerDims(nodalLoopEnd = false, initBuffer)))

    stmts
  }

  val nodePositionsCopied : Boolean = Knowledge.grid_isAxisAligned || Knowledge.grid_isUniform // otherwise we directly use virtual field
  def nodePositionsBuf : ListBuffer[IR_IV_TemporaryBuffer] = (0 until numDimsGrid).map(
    d => IR_IV_TemporaryBuffer(IR_RealDatatype, "nodePosition" + ('X' + d).toChar.toString, someCellField.domain.index, ListBuffer() ++ dimsPositionsFrag)).to[ListBuffer]

  // allocates and initializes buffer with the node positions on-demand. on some occasions, the virtual field can be directly passed to the library. this buffer is then passed to the I/O library
  def setupNodePositions : ListBuffer[IR_Statement] = if(!nodePositionsCopied) {
    ListBuffer()
  } else {
    // init buffer with node positions
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    for(d <- 0 until numDimsGrid) {
      stmts += nodePositionsBuf(d).getDeclaration()
      stmts += nodePositionsBuf(d).allocateMemory

      val numAccPerCell = nodeOffsets.length

      val initBuffer : ListBuffer[IR_Statement] = (0 until numAccPerCell).map(n => {
        val linearizedLoopIdx = loopOverInnerDims(nodalLoopEnd = true).indices.linearizeIndex(IR_LoopOverDimensions.defIt(numDimsGrid))
        IR_Assignment(
          nodePositionsBuf(d).at(IR_LoopOverFragments.defIt * numPointsPerFrag + numAccPerCell * linearizedLoopIdx + n),
          IR_VF_NodePositionPerDim.access(level, d, IR_LoopOverDimensions.defIt(numDimsGrid) + nodeOffsets(n))) : IR_Statement
      }).to[ListBuffer]

      stmts += IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(someCellField.domain.index),
          loopOverInnerDims(nodalLoopEnd = true, initBuffer)))
    }

    stmts
  }

  // frees the buffers with nodePositions/connectivity information
  def cleanupConnectivity : IR_Statement = connectivityBuf.getDtor().get
  def cleanupNodePositions : ListBuffer[IR_Statement] = (0 until numDimsGrid).map(d => nodePositionsBuf(d).getDtor().get).to[ListBuffer]
}

/// IR_ResolveVisualizationPrinters

object IR_ResolveVisualizationPrinters extends DefaultStrategy("IR_ResolveVisualizationPrinters") {
  this += new Transformation("ResolveFunctionCalls", {
    // vtk printers
    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("printVtkSWE", _), args)) =>
      args match {
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i)) => IR_PrintVtkSWE(s, i.toInt)
        case _                                                    => Logger.error("Malformed call to printVtkSWE; usage: printVtkSWE ( \"filename\", level )")
      }
    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("printVtkNS", _), args)) =>
      args match {
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i)) => IR_PrintVtkNS(s, i.toInt)
        case _                                                    => Logger.error("Malformed call to printVtkNS; usage: printVtkNS ( \"filename\", level )")
      }
    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("printVtkNNF", _), args)) =>
      args match {
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i)) => IR_PrintVtkNNF(s, i.toInt)
        case _                                                    => Logger.error("Malformed call to printVtkNNF; usage: printVtkNNF ( \"filename\", level )")
      }

    // xdmf printers
    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("printXdmfNNF", _), args)) =>
      args match {
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i), ioInterface : IR_StringConstant)                              => IR_PrintXdmfNNF(s, i.toInt, ioInterface, binaryFpp = false)
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i), ioInterface : IR_StringConstant, binFpp : IR_BooleanConstant) => IR_PrintXdmfNNF(s, i.toInt, ioInterface, binFpp.value)
        case _                                                                                                                  => Logger.error("Malformed call to printXdmfNNF; usage: printXdmfNNF ( \"filename\", level, \"ioInterface\", binFpp = false)")
      }
    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("printXdmfNS", _), args)) =>
      args match {
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i), ioInterface : IR_StringConstant)                              => IR_PrintXdmfNS(s, i.toInt, ioInterface, binaryFpp = false)
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i), ioInterface : IR_StringConstant, binFpp : IR_BooleanConstant) => IR_PrintXdmfNS(s, i.toInt, ioInterface, binFpp.value)
        case _                                                                                                                  => Logger.error("Malformed call to printXdmfNS; usage: printXdmfNS ( \"filename\", level, \"ioInterface\", binFpp = false)")
      }
    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("printXdmfSWE", _), args)) =>
      args match {
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i), ioInterface : IR_StringConstant)                              => IR_PrintXdmfSWE(s, i.toInt, ioInterface, binaryFpp = false)
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i), ioInterface : IR_StringConstant, binFpp : IR_BooleanConstant) => IR_PrintXdmfSWE(s, i.toInt, ioInterface, binFpp.value)
        case _                                                                                                                  => Logger.error("Malformed call to printXdmfSWE; usage: printXdmfSWE ( \"filename\", level, \"ioInterface\", binFpp = false)")
      }

    // TODO: resolve calls to exodus printer
  })
}
