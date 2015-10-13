package exastencils.util

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir.StatementList
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

  override def expand : Output[StatementList] = {
    if (!Settings.additionalIncludes.contains("fstream"))
      Settings.additionalIncludes += "fstream"

    var access = new FieldAccess(field, LoopOverDimensions.defIt)
    access.index(Knowledge.dimensionality) = 0

    var innerLoop = new LoopOverFragments(
      new ConditionStatement(iv.IsValidForSubdomain(field.domainIndex),
        ListBuffer[Statement](
          "std::ofstream stream(" ~ filename ~ ", " ~ (if (Knowledge.mpi_enabled) "std::ios::app" else "std::ios::trunc") ~ ")",
          new LoopOverDimensions(Knowledge.dimensionality + 1, new IndexRange(
            new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => (field.fieldLayout(i).idxDupLeftBegin - field.referenceOffset(i)) : Expression)),
            new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => (field.fieldLayout(i).idxDupRightEnd - field.referenceOffset(i)) : Expression))),
            ListBuffer[Statement](
              new InitGeomCoords(field.field, false),
              new ConditionStatement(condition,
                ("stream << xPos << \" \"" +
                  (if (Knowledge.dimensionality > 1) " << yPos << \" \"" else "") +
                  (if (Knowledge.dimensionality > 2) " << zPos << \" \"" else "") +
                  " << " : Expression) ~ access ~ " << std::endl"))),
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