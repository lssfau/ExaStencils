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
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.grid.ir._
import exastencils.parallelization.api.mpi._
import exastencils.util.ir.IR_Print

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

case class IR_PrintVtkSWE(var filename : IR_Expression, level : Int) extends IR_Statement with IR_Expandable {

  def numDimsGrid = 2

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

    val bathField = IR_FieldCollection.getByIdentifier("bath", level).get

    val numPointsPerFrag = 25
    val numFrags = 1

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

      val pointPrints = {
        val stmts = ListBuffer[IR_Statement]()

        var nodePrint = ListBuffer[IR_Expression]()
        for (d <- 0 until numDimsGrid) {
          nodePrint += IR_VF_NodePositionPerDim.access(level, d, IR_LoopOverDimensions.defIt(numDimsGrid))
          nodePrint += IR_StringConstant(" ")
        }
        for (_ <- numDimsGrid until 3) {
          nodePrint += 0
          nodePrint += IR_StringConstant(" ")
        }
        nodePrint = nodePrint.dropRight(1)
        nodePrint += IR_Print.endl
        stmts += IR_Print(stream, nodePrint)

        stmts
      }

      val initPoints = ListBuffer[IR_Statement](
        IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
        IR_Print(stream, "std::scientific"), //std::defaultfloat
        IR_LoopOverFragments(
          IR_IfCondition(IR_IV_IsValidForDomain(bathField.domain.index),
            IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => bathField.fieldLayout.idxById("DLB", dim) - Duplicate(bathField.referenceOffset(dim)) : IR_Expression)),
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => bathField.fieldLayout.idxById("DRE", dim) - Duplicate(bathField.referenceOffset(dim)) : IR_Expression))),
              pointPrints))),
        IR_MemberFunctionCall(stream, "close"))

      addStmtBlock(initPoints)
    }

    /*val printComponents = ListBuffer[IR_Expression]()
    printComponents += "std::defaultfloat"
    printComponents ++= (0 until numDimsGrid).view.flatMap { dim => List(getPos(field, dim), separator) }
    printComponents += "std::scientific"
    printComponents ++= arrayIndexRange.view.flatMap { index =>
      val access = IR_FieldAccess(field, IR_LoopOverDimensions.defIt(numDimsData))
      if (numDimsData > numDimsGrid) // TODO: replace after implementing new field accessors
        access.index(numDimsData - 1) = index // TODO: assumes innermost dimension to represent vector index
      List(access, separator)
    }
    printComponents += IR_Print.endl*/

    //CELLS 3 12
    //3 0 1 5
    //3 1 5 6
    //3 1 2 6
    //CELL_TYPES 3
    //5
    //5
    //5
    //CELL_DATA 3
    //FIELD FieldData 1
    //cell 1 3 float
    //8 7 6
    //POINT_DATA 25
    //FIELD FieldData 1
    //nodal 1 25 float
    //0 1 2 3 4 0 1 2 3 4 0 1 2 3 4 0 1 2 3 4 0 1 2 3 4

    statements
  }
}
