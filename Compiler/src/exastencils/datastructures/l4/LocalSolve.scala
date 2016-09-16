package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.field.ir.IR_FieldAccess
import exastencils.prettyprinting._
import exastencils.solver._

case class EquationExpression(var lhs : L4_Expression, var rhs : L4_Expression) extends L4_Node with L4_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) : Unit = ???
  override def progress = IR_Equation(lhs.progress, rhs.progress)
}

case class SolveLocallyStatement(var unknowns : List[L4_Expression], var equations : List[EquationExpression]) extends L4_Statement {
  override def prettyprint(out : PpStream) : Unit = ???

  override def progress : IR_LocalSolve = {
    IR_LocalSolve(
      unknowns.map(_.progress.asInstanceOf[IR_FieldAccess]).to[ListBuffer],
      equations.map(_.progress).to[ListBuffer])
  }
}
