package exastencils.util

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.{ StatementList, _ }
import exastencils.grid._
import exastencils.knowledge._
import exastencils.mpi._
import exastencils.prettyprinting._

object PrintExpression {
  val endl : IR_Expression = IR_VariableAccess("std::endl", IR_StringDatatype)
}

case class PrintExpression(var stream : IR_Expression, toPrint : ListBuffer[IR_Expression]) extends IR_Expression {
  override def datatype = stream.datatype
  def this(stream : IR_Expression, toPrint : IR_Expression*) = this(stream, toPrint.to[ListBuffer])

  override def prettyprint(out : PpStream) : Unit = {
    out << stream << " << " <<< (toPrint, " << ")
  }
}

case class BuildStringStatement(var stringName : IR_Expression, var toPrint : ListBuffer[IR_Expression]) extends IR_Statement with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand() : Output[StatementList] = {
    val streamName = BuildStringStatement.getNewName()
    def streamType = IR_SpecialDatatype("std::ostringstream")
    val statements = ListBuffer[IR_Statement](
      IR_VariableDeclaration(streamType, streamName),
      PrintExpression(IR_VariableAccess(streamName, streamType), toPrint),
      IR_Assignment(stringName, IR_MemberFunctionCall(IR_VariableAccess(streamName, IR_SpecialDatatype("std::ostringstream")), "str")))
    statements
  }
}

private object BuildStringStatement {
  private var counter : Int = 0
  def getNewName() : String = {
    counter += 1
    return "string_builder_%02d".format(counter)
  }
}

case class PrintStatement(var toPrint : ListBuffer[IR_Expression], var stream : String = "std::cout") extends IR_Statement with IR_Expandable {
  def this(toPrint : IR_Expression) = this(ListBuffer(toPrint))

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand() : Output[IR_Statement] = {
    if (toPrint.isEmpty) {
      IR_NullStatement
    } else {
      val printStmt : IR_Statement = new PrintExpression(IR_VariableAccess(stream), toPrint.view.flatMap { e => List(e, IR_StringConstant(" ")) }.to[ListBuffer] += PrintExpression.endl)
      if (Knowledge.mpi_enabled) // filter by mpi rank if required
        IR_IfCondition(MPI_IsRootProc(), printStmt)
      else
        printStmt
    }
  }
}

case class PrintFieldStatement(var filename : IR_Expression, var field : FieldSelection, var condition : IR_Expression = IR_BooleanConstant(true)) extends IR_Statement with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def numDimsGrid = field.fieldLayout.numDimsGrid
  def numDimsData = field.fieldLayout.numDimsData

  def getPos(field : FieldSelection, dim : Int) : IR_Expression = {
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

    val arrayIndexRange = (
      if (field.arrayIndex.isEmpty) (0 until field.field.gridDatatype.resolveFlattendSize)
      else (field.arrayIndex.get to field.arrayIndex.get))

    def separator = IR_StringConstant(if (Knowledge.experimental_generateParaviewFiles) "," else " ")
    val streamName = PrintFieldStatement.getNewName()
    def streamType = IR_SpecialDatatype("std::ofstream")

    val fileHeader = {
      var ret : IR_Statement = IR_NullStatement
      if (Knowledge.experimental_generateParaviewFiles) {
        ret = (streamName + " << \"x,y,z," + arrayIndexRange.map(index => s"s$index").mkString(",") + "\" << std::endl")
        if (Knowledge.mpi_enabled)
          ret = IR_IfCondition(MPI_IsRootProc(), ret)
      }
      ret
    }

    var innerLoop = ListBuffer[IR_Statement](
      IR_ObjectInstantiation(streamType, streamName, filename, IR_VariableAccess(if (Knowledge.mpi_enabled) "std::ios::app" else "std::ios::trunc")),
      fileHeader,
      IR_LoopOverFragments(
        IR_IfCondition(iv.IsValidForSubdomain(field.domainIndex),
          IR_LoopOverDimensions(numDimsData, IndexRange(
            IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => (field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim)) : IR_Expression)),
            IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => (field.fieldLayout.idxById("DRE", dim) - field.referenceOffset(dim)) : IR_Expression))),
            IR_IfCondition(condition,
              new PrintExpression(IR_VariableAccess(streamName, streamType),
                ((0 until numDimsGrid).view.flatMap { dim =>
                  List(getPos(field, dim), separator)
                } ++ arrayIndexRange.view.flatMap { index =>
                  val access = IR_FieldAccess(field, IR_LoopOverDimensions.defIt(numDimsData))
                  if (numDimsData > numDimsGrid) // TODO: replace after implementing new field accessors
                    access.index(numDimsData - 1) = index // TODO: assumes innermost dimension to represent vector index
                  List(access, separator)
                }).to[ListBuffer] += PrintExpression.endl))))),
      IR_MemberFunctionCall(IR_VariableAccess(streamName, streamType), "close"))

    var statements : ListBuffer[IR_Statement] = ListBuffer()

    if (Knowledge.mpi_enabled) {
      statements += IR_IfCondition(MPI_IsRootProc(),
        ListBuffer[IR_Statement](
          IR_ObjectInstantiation(streamType, streamName, filename, IR_VariableAccess("std::ios::trunc")),
          IR_MemberFunctionCall(IR_VariableAccess(streamName, streamType), "close")))

      statements += MPI_Sequential(innerLoop)
    } else {
      statements ++= innerLoop
    }

    statements
  }
}

object PrintFieldStatement {
  private var counter : Int = 0
  def getNewName() : String = {
    counter += 1
    return "fieldPrintStream_%02d".format(counter)
  }
}
