package exastencils.field.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir._
import exastencils.deprecated.ir._
import exastencils.domain.ir._
import exastencils.grid.ir._
import exastencils.parallelization.api.mpi._
import exastencils.util.ir._

/// IR_PrintField

object IR_PrintField {
  private var counter : Int = 0
  def getNewName() : String = {
    counter += 1
    "fieldPrintStream_%02d".format(counter)
  }
}

case class IR_PrintField(
    var filename : IR_Expression,
    var field : IR_FieldSelection,
    var condition : IR_Expression = true,
    var includeGhostLayers : Boolean = false) extends IR_Statement with IR_Expandable {

  def numDimsGrid = field.fieldLayout.numDimsGrid
  def numDimsData = field.fieldLayout.numDimsData

  def getPos(field : IR_FieldSelection, dim : Int) : IR_Expression = {
    // TODO: add function to field (layout) to decide node/cell for given dim
    field.field.localization match {
      case IR_AtNode              => IR_VF_NodePositionPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(numDimsGrid))
      case IR_AtCellCenter        => IR_VF_CellCenterPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(numDimsGrid))
      case IR_AtFaceCenter(`dim`) => IR_VF_NodePositionPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(numDimsGrid))
      case IR_AtFaceCenter(_)     => IR_VF_CellCenterPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(numDimsGrid))
    }
  }

  override def expand() : Output[StatementList] = {
    if (!Settings.additionalIncludes.contains("fstream"))
      Settings.additionalIncludes += "fstream"

    // TODO: incorporate component accesses
    val arrayIndexRange = 0 until field.field.gridDatatype.resolveFlattendSize

    def separator = IR_StringConstant(if (Knowledge.experimental_generateParaviewFiles) "," else " ")

    val streamName = IR_PrintField.getNewName()

    def streamType = IR_SpecialDatatype("std::ofstream")

    def stream = IR_VariableAccess(streamName, streamType)

    val fileHeader = {
      var ret : IR_Statement = IR_NullStatement
      if (Knowledge.experimental_generateParaviewFiles) {
        ret = IR_Print(stream, "\"x,y,z," + arrayIndexRange.map(index => s"s$index").mkString(",") + "\"", IR_Print.endl)
        if (Knowledge.mpi_enabled)
          ret = IR_IfCondition(MPI_IsRootProc(), ret)
      }
      ret
    }

    val printComponents = ListBuffer[IR_Expression]()
    printComponents += "std::defaultfloat"
    printComponents ++= (0 until numDimsGrid).view.flatMap { dim => List(getPos(field, dim), separator) }
    printComponents += "std::scientific"
    printComponents ++= arrayIndexRange.view.flatMap { index =>
      val access = IR_FieldAccess(field, IR_LoopOverDimensions.defIt(numDimsData))
      if (numDimsData > numDimsGrid) // TODO: replace after implementing new field accessors
        access.index(numDimsData - 1) = index // TODO: assumes innermost dimension to represent vector index
      List(access, separator)
    }
    printComponents += IR_Print.endl

    val fieldBegin = if (includeGhostLayers) "GLB" else "DLB"
    val fieldEnd = if (includeGhostLayers) "GRE" else "DRE"

    // TODO: less monolithic code
    var innerLoop = ListBuffer[IR_Statement](
      IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess(if (Knowledge.mpi_enabled) "std::ios::app" else "std::ios::trunc", IR_UnknownDatatype)),
      fileHeader,
      IR_Print(stream, "std::scientific"), //std::defaultfloat
      IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(field.domainIndex),
          IR_LoopOverDimensions(numDimsData, IR_ExpressionIndexRange(
            IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => field.fieldLayout.idxById(fieldBegin, dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression)),
            IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => field.fieldLayout.idxById(fieldEnd, dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression))),
            IR_IfCondition(condition,
              IR_Print(stream, printComponents))))),
      IR_MemberFunctionCall(stream, "close"))

    var statements : ListBuffer[IR_Statement] = ListBuffer()

    if (Knowledge.mpi_enabled) {
      statements += IR_IfCondition(MPI_IsRootProc(),
        ListBuffer[IR_Statement](
          IR_ObjectInstantiation(streamType, streamName, Duplicate(filename), IR_VariableAccess("std::ios::trunc", IR_UnknownDatatype)),
          IR_MemberFunctionCall(stream, "close")))

      statements += MPI_Sequential(innerLoop)
    } else {
      statements ++= innerLoop
    }

    statements
  }
}

/*case class IR_PrintVtkSWE(var filename : IR_Expression, level : Int) extends IR_Statement with IR_Expandable {

  def numDimsGrid = 2

  override def expand() : Output[StatementList] = {
    if (!Settings.additionalIncludes.contains("fstream"))
      Settings.additionalIncludes += "fstream"

    val bath = IR_FieldCollection.getByIdentifier("bath", level).get
    val etaLower = IR_FieldCollection.getByIdentifier("etaLower", level).get
    val etaUpper = IR_FieldCollection.getByIdentifier("etaUpper", level).get
    val uLower = IR_FieldCollection.getByIdentifier("uLower", level).get
    val uUpper = IR_FieldCollection.getByIdentifier("uUpper", level).get
    val vLower = IR_FieldCollection.getByIdentifier("vLower", level).get
    val vUpper = IR_FieldCollection.getByIdentifier("vUpper", level).get
    val etaNode = IR_FieldCollection.getByIdentifier("etaNode", level).get
    val uNode = IR_FieldCollection.getByIdentifier("uNode", level).get
    val vNode = IR_FieldCollection.getByIdentifier("vNode", level).get

    val numCells_x = etaLower.fieldLayout.layoutsPerDim(0).numInnerLayers
    val numCells_y = etaLower.fieldLayout.layoutsPerDim(1).numInnerLayers
    val numPointsPerFrag = (numCells_x + 1) * (numCells_y + 1)
    val numFrags = Knowledge.domain_numFragmentsTotal
    val numCells = 2 * numCells_x * numCells_y * numFrags
    val numNodes = numPointsPerFrag * numFrags

    def separator = IR_StringConstant(" ")

    //val streamName = IR_PrintField.getNewName()
    //def streamType = IR_SpecialDatatype("std::ofstream")
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
        IR_Print(stream, IR_StringConstant(s"POINTS ${ numPointsPerFrag * numFrags } float"), IR_Print.endl),
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
          IR_IfCondition(IR_IV_IsValidForDomain(bath.domain.index),
            IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => bath.fieldLayout.idxById("DLB", dim) - Duplicate(bath.referenceOffset(dim)) : IR_Expression)),
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => bath.fieldLayout.idxById("DRE", dim) - Duplicate(bath.referenceOffset(dim)) : IR_Expression))),
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
        cellPrint += 3
        cellPrint += separator
        cellPrint += offset + IR_LoopOverDimensions.defItForDim(0) + 0 + (IR_LoopOverDimensions.defItForDim(1) + 0) * (numCells_x + 1)
        cellPrint += separator
        cellPrint += offset + IR_LoopOverDimensions.defItForDim(0) + 1 + (IR_LoopOverDimensions.defItForDim(1) + 0) * (numCells_x + 1)
        cellPrint += separator
        cellPrint += offset + IR_LoopOverDimensions.defItForDim(0) + 0 + (IR_LoopOverDimensions.defItForDim(1) + 1) * (numCells_x + 1)
        cellPrint += IR_Print.endl

        cellPrint += 3
        cellPrint += separator
        cellPrint += offset + IR_LoopOverDimensions.defItForDim(0) + 1 + (IR_LoopOverDimensions.defItForDim(1) + 0) * (numCells_x + 1)
        cellPrint += separator
        cellPrint += offset + IR_LoopOverDimensions.defItForDim(0) + 0 + (IR_LoopOverDimensions.defItForDim(1) + 1) * (numCells_x + 1)
        cellPrint += separator
        cellPrint += offset + IR_LoopOverDimensions.defItForDim(0) + 1 + (IR_LoopOverDimensions.defItForDim(1) + 1) * (numCells_x + 1)
        cellPrint += IR_Print.endl

        IR_Print(stream, cellPrint)
      }

      val initCells = ListBuffer[IR_Statement](
        IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
        IR_IfCondition(MPI_IsRootProc(), IR_Print(stream, IR_StringConstant("CELLS"), separator, numCells, separator, 4 * numCells, IR_Print.endl)),
        //IR_Print(stream, "std::scientific"), //std::defaultfloat
        IR_LoopOverFragments(
          IR_IfCondition(IR_IV_IsValidForDomain(etaLower.domain.index),
            IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaLower.fieldLayout.idxById("DLB", dim) - Duplicate(etaLower.referenceOffset(dim)) : IR_Expression)),
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaLower.fieldLayout.idxById("DRE", dim) - Duplicate(etaLower.referenceOffset(dim)) : IR_Expression))),
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
            IR_Print(stream, ListBuffer[IR_Expression](5, separator))),
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
            IR_Print(stream, IR_StringConstant("FIELD"), separator, IR_StringConstant("FieldData"), separator, 3, IR_Print.endl),
            IR_MemberFunctionCall(stream, "close")))))
      }

      def addCellPrint(name : String, cellPrint : ListBuffer[IR_Expression]) = {
        val stream = newStream

        val print = IR_Print(stream, cellPrint)

        val initCells = ListBuffer[IR_Statement](
          IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
          IR_IfCondition(MPI_IsRootProc(),
            IR_Print(stream, IR_StringConstant(name), separator, 1, separator, numCells, separator, IR_StringConstant("float"), IR_Print.endl)),
          IR_Print(stream, "std::scientific"),
          IR_LoopOverFragments(
            IR_IfCondition(IR_IV_IsValidForDomain(etaLower.domain.index),
              IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaLower.fieldLayout.idxById("DLB", dim) - Duplicate(etaLower.referenceOffset(dim)) : IR_Expression)),
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaLower.fieldLayout.idxById("DRE", dim) - Duplicate(etaLower.referenceOffset(dim)) : IR_Expression))),
                print))),
          IR_MemberFunctionCall(stream, "close"))

        addStmtBlock(initCells)
      }

      // add eta
      addCellPrint("eta", {
        var cellPrint = ListBuffer[IR_Expression]()
        cellPrint += IR_FieldAccess(IR_FieldSelection(etaLower, level, IR_IV_ActiveSlot(etaLower)), IR_LoopOverDimensions.defIt(numDimsGrid))
        cellPrint += separator
        cellPrint += IR_FieldAccess(IR_FieldSelection(etaUpper, level, IR_IV_ActiveSlot(etaUpper)), IR_LoopOverDimensions.defIt(numDimsGrid))
        cellPrint += IR_Print.endl
      })

      // add u
      addCellPrint("u", {
        var cellPrint = ListBuffer[IR_Expression]()
        cellPrint += IR_FieldAccess(IR_FieldSelection(uLower, level, IR_IV_ActiveSlot(uLower)), IR_LoopOverDimensions.defIt(numDimsGrid))
        cellPrint += separator
        cellPrint += IR_FieldAccess(IR_FieldSelection(uUpper, level, IR_IV_ActiveSlot(uUpper)), IR_LoopOverDimensions.defIt(numDimsGrid))
        cellPrint += IR_Print.endl
      })

      // add v
      addCellPrint("v", {
        var cellPrint = ListBuffer[IR_Expression]()
        cellPrint += IR_FieldAccess(IR_FieldSelection(vLower, level, IR_IV_ActiveSlot(vLower)), IR_LoopOverDimensions.defIt(numDimsGrid))
        cellPrint += separator
        cellPrint += IR_FieldAccess(IR_FieldSelection(vUpper, level, IR_IV_ActiveSlot(vUpper)), IR_LoopOverDimensions.defIt(numDimsGrid))
        cellPrint += IR_Print.endl
      })
    }

    // add node data
    {
      // add header
      {
        val stream = newStream

        addStmtBlock(ListBuffer[IR_Statement](
          IR_IfCondition(MPI_IsRootProc(), ListBuffer[IR_Statement](
            IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
            IR_Print(stream, IR_StringConstant("POINT_DATA"), separator, numNodes, IR_Print.endl),
            IR_Print(stream, IR_StringConstant("FIELD"), separator, IR_StringConstant("FieldData"), separator, 4, IR_Print.endl),
            IR_MemberFunctionCall(stream, "close")))))
      }

      def addNodePrint(name : String, cellPrint : ListBuffer[IR_Expression]) = {
        val stream = newStream

        val print = IR_Print(stream, cellPrint)

        val initCells = ListBuffer[IR_Statement](
          IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
          IR_IfCondition(MPI_IsRootProc(),
            IR_Print(stream, IR_StringConstant(name), separator, 1, separator, numNodes, separator, IR_StringConstant("float"), IR_Print.endl)),
          IR_Print(stream, "std::scientific"),
          IR_LoopOverFragments(
            IR_IfCondition(IR_IV_IsValidForDomain(bath.domain.index),
              IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => bath.fieldLayout.idxById("DLB", dim) - Duplicate(bath.referenceOffset(dim)) : IR_Expression)),
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => bath.fieldLayout.idxById("DRE", dim) - Duplicate(bath.referenceOffset(dim)) : IR_Expression))),
                print))),
          IR_MemberFunctionCall(stream, "close"))

        addStmtBlock(initCells)
      }

      // add bath
      addNodePrint("bath", {
        var nodePrint = ListBuffer[IR_Expression]()
        nodePrint += IR_FieldAccess(IR_FieldSelection(bath, level, IR_IV_ActiveSlot(bath)), IR_LoopOverDimensions.defIt(numDimsGrid))
        nodePrint += IR_Print.endl
      })

      // add eta
      addNodePrint("etaNode", {
        var nodePrint = ListBuffer[IR_Expression]()
        nodePrint += IR_FieldAccess(IR_FieldSelection(etaNode, level, IR_IV_ActiveSlot(etaNode)), IR_LoopOverDimensions.defIt(numDimsGrid))
        nodePrint += IR_Print.endl
      })

      // add u
      addNodePrint("uNode", {
        var nodePrint = ListBuffer[IR_Expression]()
        nodePrint += IR_FieldAccess(IR_FieldSelection(uNode, level, IR_IV_ActiveSlot(uNode)), IR_LoopOverDimensions.defIt(numDimsGrid))
        nodePrint += IR_Print.endl
      })

      // add v
      addNodePrint("vNode", {
        var nodePrint = ListBuffer[IR_Expression]()
        nodePrint += IR_FieldAccess(IR_FieldSelection(vNode, level, IR_IV_ActiveSlot(vNode)), IR_LoopOverDimensions.defIt(numDimsGrid))
        nodePrint += IR_Print.endl
      })
    }

    statements
  }
}*/

case class IR_PrintVtkSWE(var filename : IR_Expression, level : Int) extends IR_Statement with IR_Expandable {

  def numDimsGrid = 2

  override def expand() : Output[StatementList] = {
    if (!Settings.additionalIncludes.contains("fstream"))
      Settings.additionalIncludes += "fstream"

    val bath = IR_FieldCollection.getByIdentifier("bath", level).get
    val etaDiscLower0 = IR_FieldCollection.getByIdentifier("etaDiscLower0", level).get
    val etaDiscLower1 = IR_FieldCollection.getByIdentifier("etaDiscLower1", level).get
    val etaDiscLower2 = IR_FieldCollection.getByIdentifier("etaDiscLower2", level).get
    val etaDiscUpper0 = IR_FieldCollection.getByIdentifier("etaDiscUpper0", level).get
    val etaDiscUpper1 = IR_FieldCollection.getByIdentifier("etaDiscUpper1", level).get
    val etaDiscUpper2 = IR_FieldCollection.getByIdentifier("etaDiscUpper2", level).get
    val uDiscLower0 = IR_FieldCollection.getByIdentifier("uDiscLower0", level).get
    val uDiscLower1 = IR_FieldCollection.getByIdentifier("uDiscLower1", level).get
    val uDiscLower2 = IR_FieldCollection.getByIdentifier("uDiscLower2", level).get
    val uDiscUpper0 = IR_FieldCollection.getByIdentifier("uDiscUpper0", level).get
    val uDiscUpper1 = IR_FieldCollection.getByIdentifier("uDiscUpper1", level).get
    val uDiscUpper2 = IR_FieldCollection.getByIdentifier("uDiscUpper2", level).get
    val vDiscLower0 = IR_FieldCollection.getByIdentifier("vDiscLower0", level).get
    val vDiscLower1 = IR_FieldCollection.getByIdentifier("vDiscLower1", level).get
    val vDiscLower2 = IR_FieldCollection.getByIdentifier("vDiscLower2", level).get
    val vDiscUpper0 = IR_FieldCollection.getByIdentifier("vDiscUpper0", level).get
    val vDiscUpper1 = IR_FieldCollection.getByIdentifier("vDiscUpper1", level).get
    val vDiscUpper2 = IR_FieldCollection.getByIdentifier("vDiscUpper2", level).get

    def etaDisc = ListBuffer(etaDiscLower0, etaDiscLower1, etaDiscLower2, etaDiscUpper0, etaDiscUpper1, etaDiscUpper2)

    def uDisc = ListBuffer(uDiscLower0, uDiscLower1, uDiscLower2, uDiscUpper0, uDiscUpper1, uDiscUpper2)

    def vDisc = ListBuffer(vDiscLower0, vDiscLower1, vDiscLower2, vDiscUpper0, vDiscUpper1, vDiscUpper2)

    val numCells_x = etaDiscLower0.fieldLayout.layoutsPerDim(0).numInnerLayers
    val numCells_y = etaDiscLower0.fieldLayout.layoutsPerDim(1).numInnerLayers
    val numPointsPerFrag = 6 * numCells_x * numCells_y
    val numFrags = Knowledge.domain_numFragmentsTotal
    val numCells = 2 * numCells_x * numCells_y * numFrags
    val numNodes = numPointsPerFrag * numFrags

    def nodeOffsets = ListBuffer(IR_ConstIndex(0, 0), IR_ConstIndex(1, 0), IR_ConstIndex(0, 1), IR_ConstIndex(1, 1), IR_ConstIndex(0, 1), IR_ConstIndex(1, 0))

    def separator = IR_StringConstant(" ")

    //val streamName = IR_PrintField.getNewName()
    //def streamType = IR_SpecialDatatype("std::ofstream")
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
        IR_Print(stream, IR_StringConstant(s"POINTS ${ numPointsPerFrag * numFrags } float"), IR_Print.endl),
        IR_MemberFunctionCall(stream, "close")))
    }

    // add mesh vertices
    {
      val stream = newStream

      val triPrint = {
        nodeOffsets.map(offset => {
          var nodePrint = ListBuffer[IR_Expression]()
          for (d <- 0 until numDimsGrid) {
            nodePrint += IR_VF_NodePositionPerDim.access(level, d, IR_LoopOverDimensions.defIt(numDimsGrid) + offset)
            nodePrint += separator
          }
          for (_ <- numDimsGrid until 3) {
            nodePrint += 0
            nodePrint += separator
          }
          nodePrint = nodePrint.dropRight(1)
          nodePrint += IR_Print.endl
          IR_Print(stream, nodePrint) : IR_Statement
        })
      }

      val initPoints = ListBuffer[IR_Statement](
        IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
        IR_Print(stream, "std::scientific"), //std::defaultfloat
        IR_LoopOverFragments(
          IR_IfCondition(IR_IV_IsValidForDomain(etaDiscLower0.domain.index),
            IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.fieldLayout.idxById("IB", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression)),
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.fieldLayout.idxById("IE", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression))),
              triPrint))),
        IR_MemberFunctionCall(stream, "close"))

      addStmtBlock(initPoints)
    }

    // add mesh cells
    {
      val stream = newStream

      val cellPrint = {
        val offset = (MPI_IV_MpiRank * Knowledge.domain_numFragmentsPerBlock + IR_LoopOverFragments.defIt) * numPointsPerFrag

        var cellPrint = ListBuffer[IR_Expression]()
        cellPrint += 3
        cellPrint += separator
        cellPrint += offset + 6 * (IR_LoopOverDimensions.defItForDim(0) + IR_LoopOverDimensions.defItForDim(1) * numCells_x) + 0
        cellPrint += separator
        cellPrint += offset + 6 * (IR_LoopOverDimensions.defItForDim(0) + IR_LoopOverDimensions.defItForDim(1) * numCells_x) + 1
        cellPrint += separator
        cellPrint += offset + 6 * (IR_LoopOverDimensions.defItForDim(0) + IR_LoopOverDimensions.defItForDim(1) * numCells_x) + 2
        cellPrint += IR_Print.endl

        cellPrint += 3
        cellPrint += separator
        cellPrint += offset + 6 * (IR_LoopOverDimensions.defItForDim(0) + IR_LoopOverDimensions.defItForDim(1) * numCells_x) + 3
        cellPrint += separator
        cellPrint += offset + 6 * (IR_LoopOverDimensions.defItForDim(0) + IR_LoopOverDimensions.defItForDim(1) * numCells_x) + 4
        cellPrint += separator
        cellPrint += offset + 6 * (IR_LoopOverDimensions.defItForDim(0) + IR_LoopOverDimensions.defItForDim(1) * numCells_x) + 5
        cellPrint += IR_Print.endl

        IR_Print(stream, cellPrint)
      }

      val initCells = ListBuffer[IR_Statement](
        IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
        IR_IfCondition(MPI_IsRootProc(), IR_Print(stream, IR_StringConstant("CELLS"), separator, numCells, separator, 4 * numCells, IR_Print.endl)),
        //IR_Print(stream, "std::scientific"), //std::defaultfloat
        IR_LoopOverFragments(
          IR_IfCondition(IR_IV_IsValidForDomain(etaDiscLower0.domain.index),
            IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.fieldLayout.idxById("DLB", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression)),
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.fieldLayout.idxById("DRE", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression))),
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
            IR_Print(stream, ListBuffer[IR_Expression](5, separator))),
          IR_Print(stream, IR_Print.endl),
          IR_MemberFunctionCall(stream, "close"))))

      addStmtBlock(cellTypes)
    }

    // add node data
    {
      // add header
      {
        val stream = newStream

        addStmtBlock(ListBuffer[IR_Statement](
          IR_IfCondition(MPI_IsRootProc(), ListBuffer[IR_Statement](
            IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
            IR_Print(stream, IR_StringConstant("POINT_DATA"), separator, numNodes, IR_Print.endl),
            IR_Print(stream, IR_StringConstant("FIELD"), separator, IR_StringConstant("FieldData"), separator, 4, IR_Print.endl),
            IR_MemberFunctionCall(stream, "close")))))
      }

      def addNodePrint(name : String, cellPrint : ListBuffer[IR_Expression]) = {
        val stream = newStream

        val print = IR_Print(stream, cellPrint)

        val initCells = ListBuffer[IR_Statement](
          IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
          IR_IfCondition(MPI_IsRootProc(),
            IR_Print(stream, IR_StringConstant(name), separator, 1, separator, numNodes, separator, IR_StringConstant("float"), IR_Print.endl)),
          IR_Print(stream, "std::scientific"),
          IR_LoopOverFragments(
            IR_IfCondition(IR_IV_IsValidForDomain(etaDiscLower0.domain.index),
              IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.fieldLayout.idxById("IB", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression)),
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.fieldLayout.idxById("IE", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression))),
                print))),
          IR_MemberFunctionCall(stream, "close"))

        addStmtBlock(initCells)
      }

      // add bath
      addNodePrint("bath", {
        var nodePrint = ListBuffer[IR_Expression]()
        nodeOffsets.foreach { offset =>
          nodePrint += IR_FieldAccess(IR_FieldSelection(bath, level, IR_IV_ActiveSlot(bath)), IR_LoopOverDimensions.defIt(numDimsGrid) + offset)
          nodePrint += IR_Print.endl
        }
        nodePrint
      })

      // add eta
      addNodePrint("eta", {
        var nodePrint = ListBuffer[IR_Expression]()
        etaDisc.foreach { eta =>
          nodePrint += IR_FieldAccess(IR_FieldSelection(eta, level, IR_IV_ActiveSlot(eta)), IR_LoopOverDimensions.defIt(numDimsGrid))
          nodePrint += IR_Print.endl
        }
        nodePrint
      })

      // add u
      addNodePrint("u", {
        var nodePrint = ListBuffer[IR_Expression]()
        uDisc.foreach { u =>
          nodePrint += IR_FieldAccess(IR_FieldSelection(u, level, IR_IV_ActiveSlot(u)), IR_LoopOverDimensions.defIt(numDimsGrid))
          nodePrint += IR_Print.endl
        }
        nodePrint
      })

      // add v
      addNodePrint("v", {
        var nodePrint = ListBuffer[IR_Expression]()
        vDisc.foreach { v =>
          nodePrint += IR_FieldAccess(IR_FieldSelection(v, level, IR_IV_ActiveSlot(v)), IR_LoopOverDimensions.defIt(numDimsGrid))
          nodePrint += IR_Print.endl
        }
        nodePrint
      })
    }

    statements
  }
}

case class IR_PrintVtkNS(var filename : IR_Expression, level : Int) extends IR_Statement with IR_Expandable {

  def numDimsGrid = 2

  override def expand() : Output[StatementList] = {
    if (!Settings.additionalIncludes.contains("fstream"))
      Settings.additionalIncludes += "fstream"

    val u = IR_FieldCollection.getByIdentifier("u", level).get
    val v = IR_FieldCollection.getByIdentifier("v", level).get
    val p = IR_FieldCollection.getByIdentifier("p", level).get

    val numCells_x = p.fieldLayout.layoutsPerDim(0).numInnerLayers
    val numCells_y = p.fieldLayout.layoutsPerDim(1).numInnerLayers
    val numPointsPerFrag = (numCells_x + 1) * (numCells_y + 1)
    val numFrags = Knowledge.domain_numFragmentsTotal
    val numCells = numCells_x * numCells_y * numFrags
    val numNodes = numPointsPerFrag * numFrags

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
        IR_Print(stream, IR_StringConstant(s"POINTS ${ numPointsPerFrag * numFrags } float"), IR_Print.endl),
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
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => p.fieldLayout.idxById("DLB", dim) - Duplicate(p.referenceOffset(dim)) : IR_Expression)),
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => p.fieldLayout.idxById("DRE", dim) + 1 - Duplicate(p.referenceOffset(dim)) : IR_Expression))),
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

        IR_Print(stream, cellPrint)
      }

      val initCells = ListBuffer[IR_Statement](
        IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
        IR_IfCondition(MPI_IsRootProc(), IR_Print(stream, IR_StringConstant("CELLS"), separator, numCells, separator, 5 * numCells, IR_Print.endl)),
        //IR_Print(stream, "std::scientific"), //std::defaultfloat
        IR_LoopOverFragments(
          IR_IfCondition(IR_IV_IsValidForDomain(p.domain.index),
            IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => p.fieldLayout.idxById("DLB", dim) - Duplicate(p.referenceOffset(dim)) : IR_Expression)),
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => p.fieldLayout.idxById("DRE", dim) - Duplicate(p.referenceOffset(dim)) : IR_Expression))),
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
            IR_Print(stream, ListBuffer[IR_Expression](9, separator))),
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
            IR_Print(stream, IR_StringConstant(name), separator, numComponents, separator, numCells, separator, IR_StringConstant("float"), IR_Print.endl)),
          IR_Print(stream, "std::scientific"),
          IR_LoopOverFragments(
            IR_IfCondition(IR_IV_IsValidForDomain(p.domain.index),
              IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => p.fieldLayout.idxById("DLB", dim) - Duplicate(p.referenceOffset(dim)) : IR_Expression)),
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => p.fieldLayout.idxById("DRE", dim) - Duplicate(p.referenceOffset(dim)) : IR_Expression))),
                print))),
          IR_MemberFunctionCall(stream, "close"))

        addStmtBlock(initCells)
      }

      def meanU = 0.5 * (IR_FieldAccess(IR_FieldSelection(u, level, IR_IV_ActiveSlot(u)), IR_LoopOverDimensions.defIt(numDimsGrid))
        + IR_FieldAccess(IR_FieldSelection(u, level, IR_IV_ActiveSlot(u)), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(1, 0)))
      def meanV = 0.5 * (IR_FieldAccess(IR_FieldSelection(v, level, IR_IV_ActiveSlot(v)), IR_LoopOverDimensions.defIt(numDimsGrid))
        + IR_FieldAccess(IR_FieldSelection(v, level, IR_IV_ActiveSlot(v)), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(0, 1)))

      // add u
      addCellPrint("vel", {
        var cellPrint = ListBuffer[IR_Expression]()
        cellPrint += meanU
        cellPrint += separator
        cellPrint += meanV
        cellPrint += IR_Print.endl
      }, 2)

      // add p
      addCellPrint("p", {
        var cellPrint = ListBuffer[IR_Expression]()
        cellPrint += IR_FieldAccess(IR_FieldSelection(p, level, IR_IV_ActiveSlot(p)), IR_LoopOverDimensions.defIt(numDimsGrid))
        cellPrint += IR_Print.endl
      })
    }

    statements
  }
}
