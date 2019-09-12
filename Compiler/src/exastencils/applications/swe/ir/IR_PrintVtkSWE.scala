package exastencils.applications.swe.ir

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

/// IR_PrintVtkSWE

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

    val numCells_x = etaDiscLower0.layout.layoutsPerDim(0).numInnerLayers
    val numCells_y = etaDiscLower0.layout.layoutsPerDim(1).numInnerLayers
    val numPointsPerFrag = 6 * numCells_x * numCells_y

    def numValidFrags = IR_VariableAccess("numValidFrags", IR_IntegerDatatype)

    def totalNumFrags = IR_VariableAccess("totalNumFrags", IR_IntegerDatatype) // Knowledge.domain_numFragmentsTotal
    def fragmentOffset = IR_VariableAccess("fragmentOffset", IR_IntegerDatatype)

    val numCells = 2 * numCells_x * numCells_y * totalNumFrags
    val numNodes = numPointsPerFrag * totalNumFrags

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

    // determine number of valid fragments per block and total number of valid fragments
    statements ++= ListBuffer(
      IR_VariableDeclaration(fragmentOffset, 0),
      IR_VariableDeclaration(numValidFrags, 0),
      IR_LoopOverFragments(IR_IfCondition(IR_IV_IsValidForDomain(0), IR_Assignment(numValidFrags, numValidFrags + 1))),
      IR_VariableDeclaration(totalNumFrags, numValidFrags))
    if (Knowledge.mpi_enabled)
      statements += MPI_Reduce(0, IR_AddressOf(totalNumFrags), IR_IntegerDatatype, 1, "+")

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
        IR_Print(stream, IR_StringConstant("POINTS "), numPointsPerFrag * totalNumFrags, IR_StringConstant(" double"), IR_Print.endl),
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
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.layout.idxById("IB", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression)),
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.layout.idxById("IE", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression))),
              triPrint))),
        IR_MemberFunctionCall(stream, "close"))

      addStmtBlock(initPoints)
    }

    // add mesh cells
    {
      val stream = newStream

      val cellPrint = {
        val offset = //(MPI_IV_MpiRank * Knowledge.domain_numFragmentsPerBlock + IR_LoopOverFragments.defIt) * numPointsPerFrag
          (fragmentOffset + IR_LoopOverFragments.defIt) * numPointsPerFrag

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

      def sendRequest = IR_VariableAccess("sendRequest", "MPI_Request")

      def recvRequest = IR_VariableAccess("recvRequest", "MPI_Request")

      val initCells = ListBuffer[IR_Statement](
        IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
        IR_IfCondition(MPI_IsRootProc(), IR_Print(stream, IR_StringConstant("CELLS"), separator, numCells, separator, 4 * numCells, IR_Print.endl)),
        //IR_Print(stream, "std::scientific"), //std::defaultfloat
        IR_LoopOverFragments(
          IR_IfCondition(IR_IV_IsValidForDomain(etaDiscLower0.domain.index),
            IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.layout.idxById("DLB", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression)),
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.layout.idxById("DRE", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression))),
              cellPrint))),
        IR_MemberFunctionCall(stream, "close"),
        IR_Assignment(fragmentOffset, fragmentOffset + numValidFrags))

      if (Knowledge.mpi_enabled) {
        initCells.prepend(
          IR_IfCondition(MPI_IV_MpiRank > 0, ListBuffer[IR_Statement](
            IR_VariableDeclaration(recvRequest),
            MPI_Receive(IR_AddressOf(fragmentOffset), 1, IR_IntegerDatatype, MPI_IV_MpiRank - 1, 0, recvRequest),
            IR_FunctionCall(MPI_WaitForRequest.generateFctAccess(), IR_AddressOf(recvRequest)))))
        initCells.append(
          IR_IfCondition(MPI_IV_MpiRank < Knowledge.mpi_numThreads - 1, ListBuffer[IR_Statement](
            IR_VariableDeclaration(sendRequest),
            MPI_Send(IR_AddressOf(fragmentOffset), 1, IR_IntegerDatatype, MPI_IV_MpiRank + 1, 0, sendRequest),
            IR_FunctionCall(MPI_WaitForRequest.generateFctAccess(), IR_AddressOf(sendRequest)))))
      }

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
            IR_Print(stream, IR_StringConstant(name), separator, 1, separator, numNodes, separator, IR_StringConstant("double"), IR_Print.endl)),
          IR_Print(stream, "std::scientific"),
          IR_LoopOverFragments(
            IR_IfCondition(IR_IV_IsValidForDomain(etaDiscLower0.domain.index),
              IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.layout.idxById("IB", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression)),
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.layout.idxById("IE", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression))),
                print))),
          IR_MemberFunctionCall(stream, "close"))

        addStmtBlock(initCells)
      }

      // add bath
      addNodePrint("bath", {
        var nodePrint = ListBuffer[IR_Expression]()
        nodeOffsets.foreach { offset =>
          nodePrint += IR_FieldAccess(bath, IR_IV_ActiveSlot(bath), IR_LoopOverDimensions.defIt(numDimsGrid) + offset)
          nodePrint += IR_Print.endl
        }
        nodePrint
      })

      // add eta
      addNodePrint("eta", {
        var nodePrint = ListBuffer[IR_Expression]()
        etaDisc.foreach { eta =>
          nodePrint += IR_FieldAccess(eta, IR_IV_ActiveSlot(eta), IR_LoopOverDimensions.defIt(numDimsGrid))
          nodePrint += IR_Print.endl
        }
        nodePrint
      })

      // add u
      addNodePrint("u", {
        var nodePrint = ListBuffer[IR_Expression]()
        uDisc.foreach { u =>
          nodePrint += IR_FieldAccess(u, IR_IV_ActiveSlot(u), IR_LoopOverDimensions.defIt(numDimsGrid))
          nodePrint += IR_Print.endl
        }
        nodePrint
      })

      // add v
      addNodePrint("v", {
        var nodePrint = ListBuffer[IR_Expression]()
        vDisc.foreach { v =>
          nodePrint += IR_FieldAccess(v, IR_IV_ActiveSlot(v), IR_LoopOverDimensions.defIt(numDimsGrid))
          nodePrint += IR_Print.endl
        }
        nodePrint
      })
    }

    statements
  }
}
