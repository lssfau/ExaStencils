package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._
import exastencils.prettyprinting._

case class EquationExpression(var lhs : Expression, var rhs : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = ???

  override def progressToIr : ir.EquationExpression = {
    ir.EquationExpression(lhs.progressToIr, rhs.progressToIr)
  }
}

case class SolveLocallyStatement(var unknowns : List[Expression], var equations : List[EquationExpression]) extends Statement {
  override def prettyprint(out : PpStream) : Unit = ???

  override def progressToIr : ir.SolveLocallyStatement = {
    ir.SolveLocallyStatement(
      unknowns.map(_.progressToIr).to[ListBuffer],
      equations.map(_.progressToIr).to[ListBuffer])
  }
}
