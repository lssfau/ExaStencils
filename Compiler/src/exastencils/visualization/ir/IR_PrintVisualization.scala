package exastencils.visualization.ir

import scala.collection.mutable.ListBuffer
import scala.reflect.io.File

import exastencils.applications.ns.ir.IR_PrintVtkNNF
import exastencils.applications.ns.ir.IR_PrintVtkNS
import exastencils.applications.ns.ir.IR_PrintXdmfNNF
import exastencils.applications.ns.ir.IR_PrintXdmfNS
import exastencils.applications.swe.ir.IR_PrintVtkSWE
import exastencils.applications.swe.ir.IR_PrintXdmfSWE
import exastencils.base.ir.IR_Access
import exastencils.base.ir.IR_ArrayAccess
import exastencils.base.ir.IR_ArrayAllocation
import exastencils.base.ir.IR_ArrayFree
import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_BooleanConstant
import exastencils.base.ir.IR_BooleanDatatype
import exastencils.base.ir.IR_ConstIndex
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
import exastencils.base.ir.IR_PointerDatatype
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir.IR_SpecialDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_StringDatatype
import exastencils.base.ir.IR_UnresolvedFunctionReference
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldIO
import exastencils.grid.ir.IR_VF_NodePositionPerDim
import exastencils.logger.Logger

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

  // careful: must be in KJI order (i.e. slowest varying dimension first)
  def dimsConnectivityFrag : ListBuffer[IR_IntegerConstant] = ListBuffer(numCells_z, numCells_y, numCells_x, connectivityForCell.length)
  def dimsPositionsFrag    : ListBuffer[IR_IntegerConstant]

  def numCellsPerFrag : Int
  def numPointsPerFrag : Long = dimsPositionsFrag.foldLeft(1L)((a, b) => a.v * b.v)

  def numFrags : IR_Expression
  def numFragsPerBlock : IR_Expression

  def numCells_x : Int
  def numCells_y : Int
  def numCells_z : Int

  def numCells : IR_Expression = numCellsPerFrag * numFrags
  def numNodes : IR_Multiplication = numPointsPerFrag * numFrags

  def someCellField : IR_Field // required as base for setting up iteration spaces later

  def nodeOffsets : ListBuffer[IR_ConstIndex] // essentially only for SWE, but used here to reduce duplicate code

  // global variable used for constant data reduction
  def constantsWritten_decl = IR_VariableDeclaration(IR_BooleanDatatype, "constantsWritten", false)
  def constantsWritten = IR_VariableAccess(constantsWritten_decl)

  def connectivityForCell : ListBuffer[IR_Expression]

  def connectivity_decl = IR_VariableDeclaration(IR_PointerDatatype(IR_IntegerDatatype), "connectivity")
  def connectivity = IR_VariableAccess(connectivity_decl)

  def loopOverInnerDims(nodalLoopEnd : Boolean, initBuffer : ListBuffer[IR_Statement] = ListBuffer()) = IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
    IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DLB", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression)),
    IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => (if(nodalLoopEnd) 1 else 0) + someCellField.layout.idxById("DRE", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression))),
    initBuffer)

  // allocates and initializes buffer with connectivity info. this buffer is then passed to the I/O library
  def setupConnectivity : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()
    val sizeConnectionFrag = dimsConnectivityFrag.reduce((a, b) => a.v * b.v)

    stmts += connectivity_decl
    stmts += IR_ArrayAllocation(connectivity, IR_IntegerDatatype, numFragsPerBlock * sizeConnectionFrag)

    val initBuffer : ListBuffer[IR_Statement] = ListBuffer() ++ connectivityForCell.indices.map(d => {
      val linearizedLoopIdx = loopOverInnerDims(nodalLoopEnd = false).indices.linearizeIndex(IR_LoopOverDimensions.defIt(numDimsGrid))
      IR_Assignment(
        IR_ArrayAccess(connectivity, IR_LoopOverFragments.defIt * sizeConnectionFrag + connectivityForCell.length * linearizedLoopIdx + d),
        connectivityForCell(d))
    })

    stmts += IR_LoopOverFragments(
      IR_IfCondition(IR_IV_IsValidForDomain(someCellField.domain.index),
        loopOverInnerDims(nodalLoopEnd = false, initBuffer)))

    stmts
  }

  val nodePositionsCopied : Boolean = Knowledge.grid_isAxisAligned || Knowledge.grid_isUniform // otherwise we directly use virtual field
  def nodePositions_decl = IR_VariableDeclaration(IR_ArrayDatatype(IR_PointerDatatype(IR_RealDatatype), numDimsGrid), "nodePosition")
  def nodePositions(dim : Int) : IR_Access = if(!nodePositionsCopied)
    IR_VF_NodePositionPerDim.access(level, dim, IR_LoopOverDimensions.defIt(numDimsGrid))
  else
    IR_ArrayAccess(IR_VariableAccess(nodePositions_decl), dim)

  // allocates and initializes buffer with the node positions on-demand. on some occasions, the virtual field can be directly passed to the library. this buffer is then passed to the I/O library
  def setupNodePositions : ListBuffer[IR_Statement] = if(!nodePositionsCopied) {
    ListBuffer()
  } else {
    // init buffer with node positions
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    stmts += nodePositions_decl

    for(d <- 0 until numDimsGrid) {
      stmts += IR_ArrayAllocation(nodePositions(d), IR_RealDatatype, numPointsPerFrag * numFragsPerBlock)

      val numAccPerCell = nodeOffsets.length

      val initBuffer : ListBuffer[IR_Statement] = ListBuffer() ++ (0 until numAccPerCell).map(n => {
        val linearizedLoopIdx = loopOverInnerDims(nodalLoopEnd = true).indices.linearizeIndex(IR_LoopOverDimensions.defIt(numDimsGrid))
        IR_Assignment(
          IR_ArrayAccess(nodePositions(d), IR_LoopOverFragments.defIt * numPointsPerFrag + numAccPerCell * linearizedLoopIdx + n),
          IR_VF_NodePositionPerDim.access(level, d, IR_LoopOverDimensions.defIt(numDimsGrid) + nodeOffsets(n)))
      })

      stmts += IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(someCellField.domain.index),
          loopOverInnerDims(nodalLoopEnd = true, initBuffer)))
    }

    stmts
  }

  // frees the buffers with nodePositions/connectivity information
  def cleanupConnectivity = IR_ArrayFree(connectivity)
  def cleanupNodePositions : ListBuffer[IR_Statement] = ListBuffer[IR_Statement]() ++ (0 until numDimsGrid).map(d => IR_ArrayFree(nodePositions(d)))
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
        case _                                                                                                              => Logger.error("Malformed call to printXdmfNNF; usage: printXdmfNNF ( \"filename\", level, \"ioInterface\", binFpp = false)")
      }
    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("printXdmfNS", _), args)) =>
      args match {
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i), ioInterface : IR_StringConstant)                              => IR_PrintXdmfNS(s, i.toInt, ioInterface, binaryFpp = false)
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i), ioInterface : IR_StringConstant, binFpp : IR_BooleanConstant) => IR_PrintXdmfNS(s, i.toInt, ioInterface, binFpp.value)
        case _                                                                                                              => Logger.error("Malformed call to printXdmfNS; usage: printXdmfNS ( \"filename\", level, \"ioInterface\", binFpp = false)")
      }
    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("printXdmfSWE", _), args)) =>
      args match {
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i), ioInterface : IR_StringConstant)                              => IR_PrintXdmfSWE(s, i.toInt, ioInterface, binaryFpp = false)
        case ListBuffer(s : IR_Expression, IR_IntegerConstant(i), ioInterface : IR_StringConstant, binFpp : IR_BooleanConstant) => IR_PrintXdmfSWE(s, i.toInt, ioInterface, binFpp.value)
        case _                                                                                                              => Logger.error("Malformed call to printXdmfSWE; usage: printXdmfSWE ( \"filename\", level, \"ioInterface\", binFpp = false)")
      }

    // TODO: resolve calls to exodus printer
  })
}
