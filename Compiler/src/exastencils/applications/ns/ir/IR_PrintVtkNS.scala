package exastencils.applications.ns.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir.StatementList
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir._
import exastencils.grid.ir.IR_VF_NodePositionPerDim
import exastencils.parallelization.api.mpi._
import exastencils.util.ir.IR_Print

/// IR_PrintVtkNS

case class IR_PrintVtkNS(var filename : IR_Expression, level : Int) extends IR_Statement with IR_Expandable {
  def u = IR_FieldCollection.getByIdentifier("u", level).get
  def v = IR_FieldCollection.getByIdentifier("v", level).get
  def w = IR_FieldCollection.getByIdentifier("w", level).get
  def p = IR_FieldCollection.getByIdentifier("p", level).get

  def numDimsGrid = p.numDimsGrid

  def numCells_x = p.layout.layoutsPerDim(0).numInnerLayers
  def numCells_y = p.layout.layoutsPerDim(1).numInnerLayers
  def numCells_z = if (numDimsGrid > 2) p.layout.layoutsPerDim(2).numInnerLayers else 1
  def numPointsPerFrag = (numCells_x + 1) * (numCells_y + 1) * (if (numDimsGrid > 2) numCells_z + 1 else 1)
  def numFrags = Knowledge.domain_numFragmentsTotal
  def numCells = numCells_x * numCells_y * numCells_z * numFrags
  def numNodes = numPointsPerFrag * numFrags

  override def expand() : Output[StatementList] = {
    if (!Settings.additionalIncludes.contains("fstream"))
      Settings.additionalIncludes += "fstream"

    def separator = IR_StringConstant(" ")

    def newStream = IR_VariableAccess(IR_PrintField.getNewName(), IR_SpecialDatatype("std::ofstream"))

    var statements : ListBuffer[IR_Statement] = ListBuffer()

    def addStmtBlock(newStmts : ListBuffer[IR_Statement]) =
      if (Knowledge.mpi_enabled)
        statements += MPI_Sequential(newStmts)
      else
        statements ++= newStmts

    // reset file
    {
      val stream = newStream
      statements += IR_IfCondition(MPI_IsRootProc(),
        ListBuffer[IR_Statement](
          IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::trunc", IR_UnknownDatatype)),
          IR_MemberFunctionCall(stream, "close")))
    }

    // add file header
    {
      val stream = newStream
      statements += IR_IfCondition(MPI_IsRootProc(), ListBuffer[IR_Statement](
        IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
        IR_Print(stream, IR_StringConstant("# vtk DataFile Version 3.0"), IR_Print.endl),
        IR_Print(stream, IR_StringConstant("vtk output"), IR_Print.endl),
        IR_Print(stream, IR_StringConstant("ASCII"), IR_Print.endl),
        IR_Print(stream, IR_StringConstant("DATASET UNSTRUCTURED_GRID"), IR_Print.endl),
        IR_Print(stream, IR_StringConstant(s"POINTS ${ numPointsPerFrag * numFrags } double"), IR_Print.endl),
        IR_MemberFunctionCall(stream, "close")))
    }

    // add mesh vertices
    {
      val stream = newStream

      val pointPrint = {
        var nodePrint = ListBuffer[IR_Expression]()
        for (d <- 0 until numDimsGrid) {
          nodePrint += IR_VF_NodePositionPerDim.access(level, d, IR_LoopOverDimensions.defIt(numDimsGrid))
          nodePrint += separator
        }
        for (_ <- numDimsGrid until 3) {
          nodePrint += 0
          nodePrint += separator
        }
        nodePrint = nodePrint.dropRight(1)
        nodePrint += IR_Print.endl
        IR_Print(stream, nodePrint)
      }

      val initPoints = ListBuffer[IR_Statement](
        IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
        IR_Print(stream, "std::scientific"), //std::defaultfloat
        IR_LoopOverFragments(
          IR_IfCondition(IR_IV_IsValidForDomain(p.domain.index),
            IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => p.layout.idxById("DLB", dim) - Duplicate(p.referenceOffset(dim)) : IR_Expression)),
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => p.layout.idxById("DRE", dim) + 1 - Duplicate(p.referenceOffset(dim)) : IR_Expression))),
              pointPrint))),
        IR_MemberFunctionCall(stream, "close"))

      addStmtBlock(initPoints)
    }

    // add mesh cells
    {
      val stream = newStream

      val cellPrint = {
        val offset = (MPI_IV_MpiRank * Knowledge.domain_numFragmentsPerBlock + IR_LoopOverFragments.defIt) * numPointsPerFrag

        var cellPrint = ListBuffer[IR_Expression]()
        numDimsGrid match {
          case 2 =>
            cellPrint += 4
            cellPrint += separator
            cellPrint += offset + IR_LoopOverDimensions.defItForDim(0) + 0 + (IR_LoopOverDimensions.defItForDim(1) + 0) * (numCells_x + 1)
            cellPrint += separator
            cellPrint += offset + IR_LoopOverDimensions.defItForDim(0) + 1 + (IR_LoopOverDimensions.defItForDim(1) + 0) * (numCells_x + 1)
            cellPrint += separator
            cellPrint += offset + IR_LoopOverDimensions.defItForDim(0) + 1 + (IR_LoopOverDimensions.defItForDim(1) + 1) * (numCells_x + 1)
            cellPrint += separator
            cellPrint += offset + IR_LoopOverDimensions.defItForDim(0) + 0 + (IR_LoopOverDimensions.defItForDim(1) + 1) * (numCells_x + 1)
            cellPrint += IR_Print.endl
          case 3 =>
            cellPrint += 8
            cellPrint += separator
            cellPrint += offset + IR_LoopOverDimensions.defItForDim(0) + 0 + (IR_LoopOverDimensions.defItForDim(1) + 0) * (numCells_x + 1) + (IR_LoopOverDimensions.defItForDim(2) + 0) * (numCells_x + 1) * (numCells_y + 1)
            cellPrint += separator
            cellPrint += offset + IR_LoopOverDimensions.defItForDim(0) + 1 + (IR_LoopOverDimensions.defItForDim(1) + 0) * (numCells_x + 1) + (IR_LoopOverDimensions.defItForDim(2) + 0) * (numCells_x + 1) * (numCells_y + 1)
            cellPrint += separator
            cellPrint += offset + IR_LoopOverDimensions.defItForDim(0) + 1 + (IR_LoopOverDimensions.defItForDim(1) + 1) * (numCells_x + 1) + (IR_LoopOverDimensions.defItForDim(2) + 0) * (numCells_x + 1) * (numCells_y + 1)
            cellPrint += separator
            cellPrint += offset + IR_LoopOverDimensions.defItForDim(0) + 0 + (IR_LoopOverDimensions.defItForDim(1) + 1) * (numCells_x + 1) + (IR_LoopOverDimensions.defItForDim(2) + 0) * (numCells_x + 1) * (numCells_y + 1)
            cellPrint += separator
            cellPrint += offset + IR_LoopOverDimensions.defItForDim(0) + 0 + (IR_LoopOverDimensions.defItForDim(1) + 0) * (numCells_x + 1) + (IR_LoopOverDimensions.defItForDim(2) + 1) * (numCells_x + 1) * (numCells_y + 1)
            cellPrint += separator
            cellPrint += offset + IR_LoopOverDimensions.defItForDim(0) + 1 + (IR_LoopOverDimensions.defItForDim(1) + 0) * (numCells_x + 1) + (IR_LoopOverDimensions.defItForDim(2) + 1) * (numCells_x + 1) * (numCells_y + 1)
            cellPrint += separator
            cellPrint += offset + IR_LoopOverDimensions.defItForDim(0) + 1 + (IR_LoopOverDimensions.defItForDim(1) + 1) * (numCells_x + 1) + (IR_LoopOverDimensions.defItForDim(2) + 1) * (numCells_x + 1) * (numCells_y + 1)
            cellPrint += separator
            cellPrint += offset + IR_LoopOverDimensions.defItForDim(0) + 0 + (IR_LoopOverDimensions.defItForDim(1) + 1) * (numCells_x + 1) + (IR_LoopOverDimensions.defItForDim(2) + 1) * (numCells_x + 1) * (numCells_y + 1)
            cellPrint += IR_Print.endl
        }

        IR_Print(stream, cellPrint)
      }

      val initCells = ListBuffer[IR_Statement](
        IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
        numDimsGrid match {
          case 2 => IR_IfCondition(MPI_IsRootProc(), IR_Print(stream, IR_StringConstant("CELLS"), separator, numCells, separator, 5 * numCells, IR_Print.endl))
          case 3 => IR_IfCondition(MPI_IsRootProc(), IR_Print(stream, IR_StringConstant("CELLS"), separator, numCells, separator, 9 * numCells, IR_Print.endl))
        },
        //IR_Print(stream, "std::scientific"), //std::defaultfloat
        IR_LoopOverFragments(
          IR_IfCondition(IR_IV_IsValidForDomain(p.domain.index),
            IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => p.layout.idxById("DLB", dim) - Duplicate(p.referenceOffset(dim)) : IR_Expression)),
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => p.layout.idxById("DRE", dim) - Duplicate(p.referenceOffset(dim)) : IR_Expression))),
              cellPrint))),
        IR_MemberFunctionCall(stream, "close"))

      addStmtBlock(initCells)
    }

    // add cell types
    {
      val stream = newStream

      def it = IR_VariableAccess("i", IR_IntegerDatatype)

      val cellTypes = ListBuffer[IR_Statement](
        IR_IfCondition(MPI_IsRootProc(), ListBuffer[IR_Statement](
          IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
          IR_Print(stream, IR_StringConstant("CELL_TYPES"), separator, numCells, IR_Print.endl),
          IR_ForLoop(IR_VariableDeclaration(it, 0), IR_Lower(it, numCells), IR_PreIncrement(it),
            numDimsGrid match {
              case 2 => IR_Print(stream, ListBuffer[IR_Expression](9, separator))
              case 3 => IR_Print(stream, ListBuffer[IR_Expression](12, separator))
            }
          ),
          IR_Print(stream, IR_Print.endl),
          IR_MemberFunctionCall(stream, "close"))))

      addStmtBlock(cellTypes)
    }

    // add cell data
    {
      // add header
      {
        val stream = newStream

        addStmtBlock(ListBuffer[IR_Statement](
          IR_IfCondition(MPI_IsRootProc(), ListBuffer[IR_Statement](
            IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
            IR_Print(stream, IR_StringConstant("CELL_DATA"), separator, numCells, IR_Print.endl),
            IR_Print(stream, IR_StringConstant("FIELD"), separator, IR_StringConstant("FieldData"), separator, 2, IR_Print.endl),
            IR_MemberFunctionCall(stream, "close")))))
      }

      def addCellPrint(name : String, cellPrint : ListBuffer[IR_Expression], numComponents : Int = 1) = {
        val stream = newStream

        val print = IR_Print(stream, cellPrint)

        val initCells = ListBuffer[IR_Statement](
          IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
          IR_IfCondition(MPI_IsRootProc(),
            IR_Print(stream, IR_StringConstant(name), separator, numComponents, separator, numCells, separator, IR_StringConstant("double"), IR_Print.endl)),
          IR_Print(stream, "std::scientific"),
          IR_LoopOverFragments(
            IR_IfCondition(IR_IV_IsValidForDomain(p.domain.index),
              IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => p.layout.idxById("DLB", dim) - Duplicate(p.referenceOffset(dim)) : IR_Expression)),
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => p.layout.idxById("DRE", dim) - Duplicate(p.referenceOffset(dim)) : IR_Expression))),
                print))),
          IR_MemberFunctionCall(stream, "close"))

        addStmtBlock(initCells)
      }

      def meanU = 0.5 * (IR_FieldAccess(u, IR_IV_ActiveSlot(u), IR_LoopOverDimensions.defIt(numDimsGrid))
        + IR_FieldAccess(u, IR_IV_ActiveSlot(u), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(1, 0, 0)))

      def meanV = 0.5 * (IR_FieldAccess(v, IR_IV_ActiveSlot(v), IR_LoopOverDimensions.defIt(numDimsGrid))
        + IR_FieldAccess(v, IR_IV_ActiveSlot(v), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(0, 1, 0)))

      def meanW = 0.5 * (IR_FieldAccess(w, IR_IV_ActiveSlot(w), IR_LoopOverDimensions.defIt(numDimsGrid))
        + IR_FieldAccess(w, IR_IV_ActiveSlot(w), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(0, 0, 1)))

      // add vel
      addCellPrint("vel", {
        var cellPrint = ListBuffer[IR_Expression]()
        cellPrint += meanU
        cellPrint += separator
        cellPrint += meanV
        if (numDimsGrid > 2) {
          cellPrint += separator
          cellPrint += meanW
        }
        cellPrint += IR_Print.endl
      }, numDimsGrid)

      // add p
      addCellPrint("p", {
        var cellPrint = ListBuffer[IR_Expression]()
        cellPrint += IR_FieldAccess(p, IR_IV_ActiveSlot(p), IR_LoopOverDimensions.defIt(numDimsGrid))
        cellPrint += IR_Print.endl
      })
    }

    statements
  }
}
