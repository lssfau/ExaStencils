package exastencils.util

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir.StatementList
import exastencils.grid._
import exastencils.knowledge._
import exastencils.mpi._
import exastencils.prettyprinting._

case class BuildStringStatement(var stringName : Expression, var toPrint : ListBuffer[Expression]) extends Statement with Expandable {

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = PrintStatement\n"

  override def expand : Output[StatementList] = {
    val streamName = BuildStringStatement.getNewName()
    var statements = ListBuffer[Statement](
      VariableDeclarationStatement(SpecialDatatype("std::ostringstream"), streamName),
      (streamName : Expression) ~ " << " ~ toPrint.reduceLeft((l, e) => l ~ " << " ~ e),
      AssignmentStatement(stringName, MemberFunctionCallExpression(VariableAccess(streamName, Some(SpecialDatatype("std::ostringstream"))), "str", ListBuffer())))

    /*Scope*/ (statements)
  }
}

private object BuildStringStatement {
  private var counter : Int = 0
  def getNewName() : String = {
    counter += 1
    return "string_builder_%02d".format(counter)
  }
}

case class PrintStatement(var toPrint : ListBuffer[Expression], var stream : String = "std::cout") extends Statement with Expandable {
  def this(toPrint : Expression) = this(ListBuffer(toPrint))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = PrintStatement\n"

  override def expand : Output[Statement] = {
    if (toPrint.isEmpty) {
      NullStatement
    } else {
      val printStmt : Statement = (stream : Expression) ~ " << " ~ toPrint.reduceLeft((l, e) => l ~ " << \" \" << " ~ e) ~ " << std::endl"
      if (Knowledge.mpi_enabled) // filter by mpi rank if required
        new ConditionStatement(MPI_IsRootProc(), printStmt)
      else
        printStmt
    }
  }
}

case class PrintFieldStatement(var filename : Expression, var field : FieldSelection, var condition : Expression = BooleanConstant(true)) extends Statement with Expandable {
  import PrintFieldStatement._

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = PrintFieldStatement\n"

  def numDimsGrid = field.fieldLayout.numDimsGrid
  def numDimsData = field.fieldLayout.numDimsData

  def getPos(field : FieldSelection, dim : Int) : Expression = {
    field.field.discretization match {
      case "node" => GridGeometry.getGeometry.nodePosition(field.level, LoopOverDimensions.defIt(numDimsGrid), None, dim)
      case "cell" => GridGeometry.getGeometry.cellCenter(field.level, LoopOverDimensions.defIt(numDimsGrid), None, dim)
      case discr @ ("face_x" | "face_y" | "face_z") => {
        if (s"face_${dimToString(dim)}" == discr)
          GridGeometry.getGeometry.nodePosition(field.level, LoopOverDimensions.defIt(numDimsGrid), None, dim)
        else
          GridGeometry.getGeometry.cellCenter(field.level, LoopOverDimensions.defIt(numDimsGrid), None, dim)
      }
    }
  }

  override def expand : Output[StatementList] = {
    if (!Settings.additionalIncludes.contains("fstream"))
      Settings.additionalIncludes += "fstream"

    val arrayIndexRange = (
      if (field.arrayIndex.isEmpty) (0 until field.field.gridDatatype.resolveFlattendSize)
      else (field.arrayIndex.get to field.arrayIndex.get))

    val separator = (if (Knowledge.experimental_generateParaviewFiles) "\",\"" else "\" \"")
    val streamName = s"fieldPrintStream_$counter"
    counter += 1

    val fileHeader = {
      var ret : Statement = NullStatement
      if (Knowledge.experimental_generateParaviewFiles) {
        ret = (streamName + " << \"x,y,z," + arrayIndexRange.map(index => s"s$index").mkString(",") + "\" << std::endl")
        if (Knowledge.mpi_enabled)
          ret = new ConditionStatement(new MPI_IsRootProc, ret)
      }
      ret
    }

    var innerLoop = ListBuffer[Statement](
      "std::ofstream " ~ streamName ~ "(" ~ filename ~ ", " ~ (if (Knowledge.mpi_enabled) "std::ios::app" else "std::ios::trunc") ~ ")",
      fileHeader,
      new LoopOverFragments(
        new ConditionStatement(iv.IsValidForSubdomain(field.domainIndex),
          new LoopOverDimensions(numDimsData, new IndexRange(
            new MultiIndex((0 until numDimsData).toArray.map(dim => (field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim)) : Expression)),
            new MultiIndex((0 until numDimsData).toArray.map(dim => (field.fieldLayout.idxById("DRE", dim) - field.referenceOffset(dim)) : Expression))),
            new ConditionStatement(condition,
              streamName
                ~ (0 until numDimsGrid).map(dim => " << " ~ getPos(field, dim) ~ " << " ~ separator).reduceLeft(_ ~ _)
                ~ arrayIndexRange.map(index => {
                  var access = new FieldAccess(field, LoopOverDimensions.defIt(numDimsData))
                  access.index(numDimsData - 1) = index // TODO: assumes innermost dimension to represent vector index
                  " << " ~ access
                }).reduceLeft(_ ~ " << " ~ separator ~ _)
                ~ " << std::endl")))),
      streamName ~ ".close()")

    var statements : ListBuffer[Statement] = ListBuffer()

    if (Knowledge.mpi_enabled) {
      statements += new ConditionStatement(new MPI_IsRootProc,
        ListBuffer[Statement](
          "std::ofstream " ~ streamName ~ "(" ~ filename ~ ", std::ios::trunc)",
          streamName ~ ".close()"))

      statements += new MPI_Sequential(innerLoop)
    } else {
      statements ++= innerLoop
    }

    statements
  }
}

object PrintFieldStatement {
  var counter = 0;
}
