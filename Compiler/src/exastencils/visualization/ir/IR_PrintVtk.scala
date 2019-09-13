package exastencils.visualization.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir.StatementList
import exastencils.field.ir._
import exastencils.parallelization.api.mpi._
import exastencils.util.ir.IR_Print

/// IR_PrintVtk
// to be implemented as specific printer in exastencils.application.ir

abstract class IR_PrintVtk extends IR_Statement with IR_Expandable {
  def filename : IR_Expression
  def numDimsGrid : Int

  def separator = IR_StringConstant(" ")

  def newStream = IR_VariableAccess(IR_PrintField.getNewName(), IR_SpecialDatatype("std::ofstream"))

  def level : Int

  def numPointsPerFrag : Int
  def numFrags : IR_Expression

  def numCells_x : Int
  def numCells_y : Int
  def numCells_z : Int

  def numNodes = numPointsPerFrag * numFrags

  def someCellField : IR_Field // required as base for setting up iteration spaces later

  def genStmtBlock(newStmts : ListBuffer[IR_Statement]) : ListBuffer[IR_Statement] = {
    if (Knowledge.mpi_enabled)
      ListBuffer[IR_Statement](MPI_Sequential(newStmts))
    else
      newStmts
  }

  def stmtsForResetFile : ListBuffer[IR_Statement] = {
    val stream = newStream
    ListBuffer[IR_Statement](
      IR_IfCondition(MPI_IsRootProc(),
        ListBuffer[IR_Statement](
          IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::trunc", IR_UnknownDatatype)),
          IR_MemberFunctionCall(stream, "close"))))
  }

  def stmtsForFileHeader : ListBuffer[IR_Statement] = {
    val stream = newStream
    ListBuffer[IR_Statement](
      IR_IfCondition(MPI_IsRootProc(), ListBuffer[IR_Statement](
        IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
        IR_Print(stream, IR_StringConstant("# vtk DataFile Version 3.0"), IR_Print.endl),
        IR_Print(stream, IR_StringConstant("vtk output"), IR_Print.endl),
        IR_Print(stream, IR_StringConstant("ASCII"), IR_Print.endl),
        IR_Print(stream, IR_StringConstant("DATASET UNSTRUCTURED_GRID"), IR_Print.endl),
        IR_Print(stream, IR_StringConstant("POINTS "), numPointsPerFrag * numFrags, IR_StringConstant(" double"), IR_Print.endl),
        IR_MemberFunctionCall(stream, "close"))))
  }

  def stmtsForPreparation : ListBuffer[IR_Statement]

  def stmtsForMeshVertices : ListBuffer[IR_Statement]
  def stmtsForMeshCells : ListBuffer[IR_Statement]
  def stmtsForCellTypes : ListBuffer[IR_Statement]

  def stmtsForCellData : ListBuffer[IR_Statement]
  def stmtsForNodeData : ListBuffer[IR_Statement]

  override def expand() : Output[StatementList] = {
    if (!Settings.additionalIncludes.contains("fstream"))
      Settings.additionalIncludes += "fstream"

    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements ++= stmtsForPreparation

    statements ++= stmtsForResetFile

    statements ++= stmtsForFileHeader
    statements ++= stmtsForMeshVertices
    statements ++= stmtsForMeshCells
    statements ++= stmtsForCellTypes

    statements ++= stmtsForNodeData
    statements ++= stmtsForCellData

    statements
  }
}

