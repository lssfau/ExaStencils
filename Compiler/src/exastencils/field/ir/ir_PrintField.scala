package exastencils.field.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.core.Settings
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir.{ iv, _ }
import exastencils.grid.GridGeometry
import exastencils.knowledge._
import exastencils.mpi._
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_Print

/// IR_PrintField

object IR_PrintField {
  private var counter : Int = 0
  def getNewName() : String = {
    counter += 1
    "fieldPrintStream_%02d".format(counter)
  }
}

case class IR_PrintField(var filename : IR_Expression, var field : FieldSelection, var condition : IR_Expression = true) extends IR_Statement with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def numDimsGrid = field.fieldLayout.numDimsGrid
  def numDimsData = field.fieldLayout.numDimsData

  def getPos(field : FieldSelection, dim : Int) : IR_Expression = {
    // TODO: add function to field (layout) to decide node/cell for given dim
    field.field.discretization match {
      case "node"                                   => GridGeometry.getGeometry.nodePosition(field.level, IR_LoopOverDimensions.defIt(numDimsGrid), None, dim)
      case "cell"                                   => GridGeometry.getGeometry.cellCenter(field.level, IR_LoopOverDimensions.defIt(numDimsGrid), None, dim)
      case discr @ ("face_x" | "face_y" | "face_z") => {
        if (s"face_${ dimToString(dim) }" == discr)
          GridGeometry.getGeometry.nodePosition(field.level, IR_LoopOverDimensions.defIt(numDimsGrid), None, dim)
        else
          GridGeometry.getGeometry.cellCenter(field.level, IR_LoopOverDimensions.defIt(numDimsGrid), None, dim)
      }
    }
  }

  override def expand() : Output[StatementList] = {
    if (!Settings.additionalIncludes.contains("fstream"))
      Settings.additionalIncludes += "fstream"

    // TODO: adapt to the new type system
    val arrayIndexRange =
    if (field.arrayIndex.isEmpty) 0 until field.field.gridDatatype.resolveFlattendSize
    else field.arrayIndex.get to field.arrayIndex.get

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

    // TODO: less monolithic code
    var innerLoop = ListBuffer[IR_Statement](
      IR_ObjectInstantiation(stream, filename, IR_VariableAccess(if (Knowledge.mpi_enabled) "std::ios::app" else "std::ios::trunc")),
      fileHeader,
      IR_LoopOverFragments(
        IR_IfCondition(iv.IsValidForSubdomain(field.domainIndex),
          IR_LoopOverDimensions(numDimsData, IndexRange(
            IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim) : IR_Expression)),
            IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => field.fieldLayout.idxById("DRE", dim) - field.referenceOffset(dim) : IR_Expression))),
            IR_IfCondition(condition,
              IR_Print(stream,
                ((0 until numDimsGrid).view.flatMap { dim =>
                  List(getPos(field, dim), separator)
                } ++ arrayIndexRange.view.flatMap { index =>
                  val access = IR_FieldAccess(field, IR_LoopOverDimensions.defIt(numDimsData))
                  if (numDimsData > numDimsGrid) // TODO: replace after implementing new field accessors
                    access.index(numDimsData - 1) = index // TODO: assumes innermost dimension to represent vector index
                  List(access, separator)
                }).to[ListBuffer] += IR_Print.endl))))),
      IR_MemberFunctionCall(stream, "close"))

    var statements : ListBuffer[IR_Statement] = ListBuffer()

    if (Knowledge.mpi_enabled) {
      statements += IR_IfCondition(MPI_IsRootProc(),
        ListBuffer[IR_Statement](
          IR_ObjectInstantiation(streamType, streamName, filename, IR_VariableAccess("std::ios::trunc")),
          IR_MemberFunctionCall(stream, "close")))

      statements += MPI_Sequential(innerLoop)
    } else {
      statements ++= innerLoop
    }

    statements
  }
}
