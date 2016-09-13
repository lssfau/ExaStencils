package exastencils.util

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir.{ StatementList, _ }
import exastencils.grid._
import exastencils.knowledge._
import exastencils.mpi._
import exastencils.prettyprinting._

object PrintExpression {
  val endl : IR_Expression = new VariableAccess("std::endl", IR_StringDatatype)
}

case class PrintExpression(var stream : IR_Expression, toPrint : ListBuffer[IR_Expression]) extends IR_Expression {
  def this(stream : IR_Expression, toPrint : IR_Expression*) = this(stream, toPrint.to[ListBuffer])

  override def prettyprint(out : PpStream) : Unit = {
    out << stream << " << " <<< (toPrint, " << ")
  }
}

case class BuildStringStatement(var stringName : IR_Expression, var toPrint : ListBuffer[IR_Expression]) extends Statement with Expandable {

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = BuildStringStatement\n"

  override def expand() : Output[StatementList] = {
    val streamName = BuildStringStatement.getNewName()
    def streamType = IR_SpecialDatatype("std::ostringstream")
    val statements = ListBuffer[Statement](
      VariableDeclarationStatement(streamType, streamName),
      PrintExpression(new VariableAccess(streamName, streamType), toPrint),
      AssignmentStatement(stringName, MemberFunctionCallExpression(VariableAccess(streamName, Some(IR_SpecialDatatype("std::ostringstream"))), "str", ListBuffer())))
    return statements
  }
}

private object BuildStringStatement {
  private var counter : Int = 0
  def getNewName() : String = {
    counter += 1
    return "string_builder_%02d".format(counter)
  }
}

case class PrintStatement(var toPrint : ListBuffer[IR_Expression], var stream : String = "std::cout") extends Statement with Expandable {
  def this(toPrint : IR_Expression) = this(ListBuffer(toPrint))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = PrintStatement\n"

  override def expand() : Output[Statement] = {
    if (toPrint.isEmpty) {
      return NullStatement
    } else {
      val printStmt : Statement = new PrintExpression(VariableAccess(stream), toPrint.view.flatMap { e => List(e, IR_StringConstant(" ")) }.to[ListBuffer] += PrintExpression.endl)
      if (Knowledge.mpi_enabled) // filter by mpi rank if required
        return new ConditionStatement(MPI_IsRootProc(), printStmt)
      else
        return printStmt
    }
  }
}

case class PrintFieldStatement(var filename : IR_Expression, var field : FieldSelection, var condition : IR_Expression = IR_BooleanConstant(true)) extends Statement with Expandable {

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = PrintFieldStatement\n"

  def numDimsGrid = field.fieldLayout.numDimsGrid
  def numDimsData = field.fieldLayout.numDimsData

  def getPos(field : FieldSelection, dim : Int) : IR_Expression = {
    field.field.discretization match {
      case "node"                                   => GridGeometry.getGeometry.nodePosition(field.level, LoopOverDimensions.defIt(numDimsGrid), None, dim)
      case "cell"                                   => GridGeometry.getGeometry.cellCenter(field.level, LoopOverDimensions.defIt(numDimsGrid), None, dim)
      case discr @ ("face_x" | "face_y" | "face_z") => {
        if (s"face_${ dimToString(dim) }" == discr)
          GridGeometry.getGeometry.nodePosition(field.level, LoopOverDimensions.defIt(numDimsGrid), None, dim)
        else
          GridGeometry.getGeometry.cellCenter(field.level, LoopOverDimensions.defIt(numDimsGrid), None, dim)
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
      var ret : Statement = NullStatement
      if (Knowledge.experimental_generateParaviewFiles) {
        ret = (streamName + " << \"x,y,z," + arrayIndexRange.map(index => s"s$index").mkString(",") + "\" << std::endl")
        if (Knowledge.mpi_enabled)
          ret = new ConditionStatement(MPI_IsRootProc(), ret)
      }
      ret
    }

    var innerLoop = ListBuffer[Statement](
      new ObjectInstantiation(streamType, streamName, filename, VariableAccess(if (Knowledge.mpi_enabled) "std::ios::app" else "std::ios::trunc")),
      fileHeader,
      new LoopOverFragments(
        new ConditionStatement(iv.IsValidForSubdomain(field.domainIndex),
          new LoopOverDimensions(numDimsData, new IndexRange(
            new MultiIndex((0 until numDimsData).toArray.map(dim => (field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim)) : IR_Expression)),
            new MultiIndex((0 until numDimsData).toArray.map(dim => (field.fieldLayout.idxById("DRE", dim) - field.referenceOffset(dim)) : IR_Expression))),
            new ConditionStatement(condition,
              new PrintExpression(new VariableAccess(streamName, streamType),
                ((0 until numDimsGrid).view.flatMap { dim =>
                  List(getPos(field, dim), separator)
                } ++ arrayIndexRange.view.flatMap { index =>
                  val access = new FieldAccess(field, LoopOverDimensions.defIt(numDimsData))
                  if (numDimsData > numDimsGrid) // TODO: replace after implementing new field accessors
                    access.index(numDimsData - 1) = index // TODO: assumes innermost dimension to represent vector index
                  List(access, separator)
                }).to[ListBuffer] += PrintExpression.endl)
            )))),
      new MemberFunctionCallExpression(new VariableAccess(streamName, streamType), "close"))

    var statements : ListBuffer[Statement] = ListBuffer()

    if (Knowledge.mpi_enabled) {
      statements += new ConditionStatement(MPI_IsRootProc(),
        ListBuffer[Statement](
          new ObjectInstantiation(streamType, streamName, filename, VariableAccess("std::ios::trunc")),
          new MemberFunctionCallExpression(new VariableAccess(streamName, streamType), "close")))

      statements += new MPI_Sequential(innerLoop)
    } else {
      statements ++= innerLoop
    }

    return statements
  }
}

object PrintFieldStatement {
  private var counter : Int = 0
  def getNewName() : String = {
    counter += 1
    return "fieldPrintStream_%02d".format(counter)
  }
}
