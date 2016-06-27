package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.prettyprinting._

// FIXME: update with actual accessors
case class hackVecComponentAccess(var vec : VariableAccess, var i : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << vec << "(" << i << ", " << 1 << ")"
}
// FIXME: update with actual accessors
case class hackMatComponentAccess(var mat : VariableAccess, var i : Expression, var j : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << mat << "(" << i << ", " << j << ")"
}

case class EquationExpression(var lhs : Expression, var rhs : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = EquationExpression\n"
}

case class SolveLocallyStatement(var unknowns : ListBuffer[Expression], var equations : ListBuffer[EquationExpression]) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = SolveLocallyStatement\n"

  def sortEquations() = {
    // unknowns to the lhs, constants to the rhs

  }

  override def expand : Output[Scope] = {
    sortEquations()

    var stmts = ListBuffer[Statement]()

    def u = VariableAccess("_local_unknowns", Some(VectorDatatype(RealDatatype, unknowns.length, Some(false))))
    def f = VariableAccess("_local_rhs", Some(VectorDatatype(RealDatatype, unknowns.length, Some(false))))
    def A = VariableAccess("_local_matrix", Some(MatrixDatatype(RealDatatype, unknowns.length, unknowns.length)))

    // declare local variables -> to be merged later
    stmts += new VariableDeclarationStatement(u)
    stmts += new VariableDeclarationStatement(f)
    stmts += new VariableDeclarationStatement(A)

    // initialize with zero - TODO: adapt to new matrix types
    stmts += FunctionCallExpression("_local_unknowns.set", ListBuffer[Expression](0))
    stmts += FunctionCallExpression("_local_rhs.set", ListBuffer[Expression](0))
    stmts += FunctionCallExpression("_local_matrix.set", ListBuffer[Expression](0))

    // construct rhs

    // construct matrix

    // solve local system - TODO: replace inverse function call with internal function
    stmts += AssignmentStatement(u, MultiplicationExpression(ListBuffer(FunctionCallExpression("_local_matrix.inverse", ListBuffer()), f)))

    // write back results
    for (i <- 0 until unknowns.length)
      stmts += AssignmentStatement(unknowns(i), hackVecComponentAccess(u, i))

    Scope(stmts)
  }
}
