package exastencils.util

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir.StatementList
import exastencils.knowledge._
import exastencils.mpi._

case class PrintStatement(var toPrint : ListBuffer[Expression]) extends Statement with Expandable {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = PrintStatement\n"

  override def expand : Output[ConditionStatement] = {
    new ConditionStatement(new MPI_IsRootProc,
      ("std::cout << " : Expression) ~ toPrint.reduceLeft((l, e) => l ~ "<< \" \" <<" ~ e) ~ "<< std::endl")
  }
}

case class PrintFieldStatement(var filename : Expression, var field : FieldSelection) extends Statement with Expandable {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = PrintFieldStatement\n"

  override def expand : Output[StatementList] = {
    // FIXME: this has to be adapted for non-mpi
    var statements : ListBuffer[Statement] = ListBuffer()

    statements += new ConditionStatement(new MPI_IsRootProc,
      ListBuffer[Statement](
        "std::ofstream stream(" ~ filename ~ ", std::ios::trunc)",
        "stream.close()"))

    var access = new FieldAccess(field, LoopOverDimensions.defIt)
    access.index(Knowledge.dimensionality) = 0
    statements += new MPI_Sequential(ListBuffer[Statement](
      new LoopOverFragments(
        new ConditionStatement(iv.IsValidForSubdomain(field.domainIndex),
          ListBuffer[Statement](
            "std::ofstream stream(" ~ filename ~ ", std::ios::app)",
            new LoopOverDimensions(Knowledge.dimensionality + 1, new IndexRange(
              new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => field.layout(i).idxDupLeftBegin)),
              new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => field.layout(i).idxDupRightEnd))),
              ListBuffer[Statement](
                new InitGeomCoords(field.field, false),
                ("stream << xPos << \" \"" +
                  (if (Knowledge.dimensionality > 1) " << yPos << \" \"" else "") +
                  (if (Knowledge.dimensionality > 2) " << zPos << \" \"" else "") +
                  " << " : Expression) ~ access ~ " << std::endl")),
            "stream.close()")))))

    statements
  }
}