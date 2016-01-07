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
    val streamName = s"string_builder_${BuildStringStatement.counter}"
    var statements = ListBuffer[Statement](
      VariableDeclarationStatement(SpecialDatatype("std::ostringstream"), streamName),
      (streamName : Expression) ~ " << " ~ toPrint.reduceLeft((l, e) => l ~ " << " ~ e),
      AssignmentStatement(stringName, MemberFunctionCallExpression(streamName, "str", ListBuffer())))

    BuildStringStatement.counter += 1

    /*Scope*/ (statements)
  }
}

object BuildStringStatement {
  var counter : Int = 0
}

case class PrintStatement(var toPrint : ListBuffer[Expression], var stream : String = "std::cout") extends Statement with Expandable {
  def this(toPrint : Expression) = this(ListBuffer(toPrint))

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = PrintStatement\n"

  override def expand : Output[Statement] = {
    if (toPrint.isEmpty)
      NullStatement
    else
      new ConditionStatement(MPI_IsRootProc(),
        (stream : Expression) ~ " << " ~ toPrint.reduceLeft((l, e) => l ~ " << \" \" << " ~ e) ~ " << std::endl")
  }
}

case class PrintFieldStatement(var filename : Expression, var field : FieldSelection, var condition : Expression = BooleanConstant(true)) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = PrintFieldStatement\n"

  def getPos(field : FieldSelection, dim : Int) : Expression = {
    field.field.discretization match {
      case "node" => GridGeometry.getGeometry.nodePosition(field.level, LoopOverDimensions.defIt, None, dim)
      case "cell" => GridGeometry.getGeometry.cellCenter(field.level, LoopOverDimensions.defIt, None, dim)
      case discr @ ("face_x" | "face_y" | "face_z") => {
        if (s"face_${dimToString(dim)}" == discr)
          GridGeometry.getGeometry.nodePosition(field.level, LoopOverDimensions.defIt, None, dim)
        else
          GridGeometry.getGeometry.cellCenter(field.level, LoopOverDimensions.defIt, None, dim)
      }
    }
  }

  override def expand : Output[StatementList] = {
    if (!Settings.additionalIncludes.contains("fstream"))
      Settings.additionalIncludes += "fstream"

    val arrayIndexRange = (
      if (field.arrayIndex.isEmpty) (0 until field.fieldLayout.dataType.resolveFlattendSize)
      else (field.arrayIndex.get to field.arrayIndex.get))

    val separator = (if (Knowledge.experimental_generateParaviewFiles) "\",\"" else "\" \"")

    var innerLoop = new LoopOverFragments(
      new ConditionStatement(iv.IsValidForSubdomain(field.domainIndex),
        ListBuffer[Statement](
          "std::ofstream stream(" ~ filename ~ ", " ~ (if (Knowledge.mpi_enabled) "std::ios::app" else "std::ios::trunc") ~ ")",

          (if (Knowledge.experimental_generateParaviewFiles)
            ("stream << \"x,y,z," + arrayIndexRange.map(index => s"s$index").mkString(",") + "\" << std::endl")
          else
            NullStatement),

          new LoopOverDimensions(Knowledge.dimensionality, new IndexRange(
            new MultiIndex((0 until Knowledge.dimensionality).toArray.map(dim => (field.fieldLayout.idxById("DLB", dim) - field.referenceOffset(dim)) : Expression)),
            new MultiIndex((0 until Knowledge.dimensionality).toArray.map(dim => (field.fieldLayout.idxById("DRE", dim) - field.referenceOffset(dim)) : Expression))),
            new ConditionStatement(condition,
              "stream"
                ~ (0 until Knowledge.dimensionality).map(dim => " << " ~ getPos(field, dim) ~ " << " ~ separator).reduceLeft(_ ~ _)
                ~ arrayIndexRange.map(index => {
                  var access = new FieldAccess(field, LoopOverDimensions.defIt)
                  access.index(Knowledge.dimensionality) = index
                  " << " ~ access
                }).reduceLeft(_ ~ " << " ~ separator ~ _)
                ~ " << std::endl")),
          "stream.close()")))

    var statements : ListBuffer[Statement] = ListBuffer()

    if (Knowledge.mpi_enabled) {
      statements += new ConditionStatement(new MPI_IsRootProc,
        ListBuffer[Statement](
          "std::ofstream stream(" ~ filename ~ ", std::ios::trunc)",
          "stream.close()"))

      statements += new MPI_Sequential(innerLoop)
    } else {
      statements += innerLoop
    }

    statements
  }
}
