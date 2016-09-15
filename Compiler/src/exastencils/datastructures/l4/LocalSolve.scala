package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.baseExt.ir.IR_FieldAccess
import exastencils.datastructures._
import exastencils.prettyprinting._

case class EquationExpression(var lhs : L4_Expression, var rhs : L4_Expression) extends L4_Expression {
  override def prettyprint(out : PpStream) : Unit = ???

  override def progress : ir.EquationExpression = {
    ir.EquationExpression(lhs.progress, rhs.progress)
  }
}

case class SolveLocallyStatement(var unknowns : List[L4_Expression], var equations : List[EquationExpression]) extends L4_Statement {
  override def prettyprint(out : PpStream) : Unit = ???

  override def progress : ir.SolveLocallyStatement = {
    ir.SolveLocallyStatement(
      unknowns.map(_.progress.asInstanceOf[IR_FieldAccess]).to[ListBuffer],
      equations.map(_.progress).to[ListBuffer])
  }
}
