package exastencils.util

import scala.collection.mutable.ListBuffer
import exastencils.knowledge._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.mpi._
import exastencils.primitives._

case class PrintStatement(var toPrint : ListBuffer[Expression]) extends Statement with Expandable {
  def cpp : String = { return "NOT VALID ; CLASS = PrintStatement\n" }

  override def expand : Scope = {
    new Scope(ListBuffer[Statement](
      new MPI_SetRankAndSize,
      new ConditionStatement(new MPI_IsRootProc,
        ("std::cout << " : Expression) ~ toPrint.reduceLeft((l, e) => l ~ "<< \" \" <<" ~ e) ~ "<< std::endl")))
  }
}

case class PrintFieldStatement(var filename : Expression, var field : UnresolvedFieldAccess) extends Statement with Expandable {
  def cpp : String = { return "NOT VALID ; CLASS = PrintFieldStatement\n" }

  override def expand : StatementBlock = {
    // FIXME: this has to be adapted for non-mpi
    // FIXME: this will use OMP parallelization and Poly transformation
    // FIXME: this calculates wrong coordinates (falsely subtracted refOffset)
    var statements : ListBuffer[Statement] = new ListBuffer

    statements += new Scope(ListBuffer[Statement](
      new MPI_SetRankAndSize,
      new ConditionStatement(new MPI_IsRootProc,
        ListBuffer[Statement](
          "std::ofstream stream(" ~ filename ~ ", std::ios::trunc)",
          "stream.close()"))))

    statements += new MPI_Sequential(ListBuffer[Statement](
      "std::ofstream stream(" ~ filename ~ ", std::ios::app)",
      new LoopOverDomain("inner" /* FIXME */ , field.fieldIdentifier, field.level, ListBuffer[Statement](
        new InitGeomCoords(field.resolveField),
        ("stream << xPos << \" \"" +
          (if (Knowledge.dimensionality > 1) " << yPos << \" \"" else "") +
          (if (Knowledge.dimensionality > 2) " << zPos << \" \"" else "") +
          " << " : Expression) ~ new FieldAccess("curFragment.", field.resolveField, 0 /* FIXME */ , DefaultLoopMultiIndex()) ~ " << std::endl")),
      "stream.close()"))

    new StatementBlock(statements)
  }
}