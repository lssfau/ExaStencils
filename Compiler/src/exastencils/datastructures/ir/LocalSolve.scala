package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication._
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.logger._
import exastencils.prettyprinting._
import exastencils.strategies._

// FIXME: update with actual accessors
case class hackVecComponentAccess(var vec : VariableAccess, var i : IR_Expression) extends IR_Expression {
  override def prettyprint(out : PpStream) : Unit = out << vec << "(" << i << ", " << 0 << ")"
}

// FIXME: update with actual accessors
case class hackMatComponentAccess(var mat : VariableAccess, var i : IR_Expression, var j : IR_Expression) extends IR_Expression {
  override def prettyprint(out : PpStream) : Unit = out << mat << "(" << i << ", " << j << ")"
}

case class EquationExpression(var lhs : IR_Expression, var rhs : IR_Expression) extends IR_Expression {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = EquationExpression\n"
}

case class SolveLocallyStatement(var unknowns : ListBuffer[FieldAccess], var equations : ListBuffer[EquationExpression]) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = SolveLocallyStatement\n"

  var fVals = ListBuffer[IR_AdditionExpression]()
  var AVals = ListBuffer[ListBuffer[IR_AdditionExpression]]()

  def validate() = {
    // TODO
  }

  def matchUnknowns(other : FieldAccess) : Int = {
    for (i <- 0 until unknowns.size) {
      if (other.fieldSelection.field.codeName == unknowns(i).fieldSelection.field.codeName
        && other.index == unknowns(i).index)
        return i // match
    }
    -1 // no match => constant value
  }

  def processExpression(pos : Int, ex : IR_Expression, switchSign : Boolean) : Unit = {
    ex match {
      case const : Number => fVals(pos).summands += (if (switchSign) const else IR_NegativeExpression(const))

      case IR_NegativeExpression(exp) => processExpression(pos, exp, !switchSign)

      case access : FieldAccess => {
        val uPos = matchUnknowns(access)
        if (uPos < 0)
          fVals(pos).summands += (if (switchSign) access else IR_NegativeExpression(access)) // no match -> rhs
        else
          AVals(pos)(uPos).summands += IR_RealConstant(if (switchSign) -1 else 1) // match -> matrix
      }

      case multEx @ IR_MultiplicationExpression(factors) => {
        // split into known and unknown
        var localFactors = ListBuffer[IR_Expression]()
        var localUnknowns = ListBuffer[FieldAccess]()
        for (ex <- factors) {
          ex match {
            case access : FieldAccess            =>
              if (matchUnknowns(access) < 0)
                localFactors += access
              else localUnknowns += access
            case e : IR_MultiplicationExpression =>
              Logger.warn(s"Nested multiplication expressions are currently unsupported: $e")
              localFactors += e
            case e : IR_AdditionExpression       =>
              Logger.warn(s"Nested addition expressions are currently unsupported: $e")
              localFactors += e
            case e : IR_Expression               => localFactors += e
          }
        }
        if (localUnknowns.size > 1)
          Logger.warn("Non-linear equations are currently unsupported")
        if (localUnknowns.isEmpty) // no unknowns -> add to rhs
          fVals(pos).summands += (if (switchSign) multEx else IR_NegativeExpression(multEx))
        else // unknowns detected -> add to matrix
          AVals(pos)(matchUnknowns(localUnknowns.head)).summands += (
            if (switchSign)
              IR_NegativeExpression(IR_MultiplicationExpression(localFactors))
            else
              IR_MultiplicationExpression(localFactors))
      }

      case _ => Logger.warn(s"Found unsupported node type ${ ex.getClass.getName }: $ex")
    }
  }

  def processEqSummands(pos : Int, summands : ListBuffer[IR_Expression], switchSign : Boolean = false) = {
    for (ex <- summands)
      processExpression(pos, ex, switchSign)
  }

  def sortEquations() = {
    // preparation: bring all entries to left side and simplify
    var zeroEqs = equations.map(eq => Duplicate(eq.lhs - eq.rhs) : IR_Expression)
    for (eq <- zeroEqs)
      SimplifyStrategy.doUntilDoneStandalone(eq, true)

    // scan lhs for constants
    for (eqNumber <- 0 until zeroEqs.size) {
      zeroEqs(eqNumber) match {
        case IR_AdditionExpression(adds)        => processEqSummands(eqNumber, adds)
        case IR_SubtractionExpression(pos, neg) =>
          pos match {
            case IR_AdditionExpression(adds) => processEqSummands(eqNumber, adds)
            case e : IR_Expression           => processEqSummands(eqNumber, ListBuffer(e))
          }
          neg match {
            case IR_AdditionExpression(adds) => processEqSummands(eqNumber, adds, true)
            case e : IR_Expression           => processEqSummands(eqNumber, ListBuffer(e), true)
          }
        case _                                  => Logger.warn(s"Equation doesn't hold enough information (${ zeroEqs(eqNumber).getClass.getName })")
      }
    }

    // unknowns to the lhs, constants to the rhs

  }

  def expandSpecial : Output[IR_Scope] = {
    fVals = ListBuffer.fill(unknowns.length)(IR_AdditionExpression())
    AVals = ListBuffer.fill(unknowns.length)(ListBuffer.fill(unknowns.length)(IR_AdditionExpression()))

    validate()
    sortEquations()

    var stmts = ListBuffer[IR_Statement]()

    def u = VariableAccess("_local_unknowns", Some(IR_VectorDatatype(IR_RealDatatype, unknowns.length, Some(false))))
    def f = VariableAccess("_local_rhs", Some(IR_VectorDatatype(IR_RealDatatype, unknowns.length, Some(false))))
    def A = VariableAccess("_local_matrix", Some(IR_MatrixDatatype(IR_RealDatatype, unknowns.length, unknowns.length)))

    // declare local variables -> to be merged later
    stmts += new VariableDeclarationStatement(u)
    stmts += new VariableDeclarationStatement(f)
    stmts += new VariableDeclarationStatement(A)

    // initialize with zero - TODO: adapt to new matrix types
    stmts += MemberFunctionCallExpression(u, "set", ListBuffer[IR_Expression](0))
    stmts += MemberFunctionCallExpression(f, "set", ListBuffer[IR_Expression](0))
    stmts += MemberFunctionCallExpression(A, "set", ListBuffer[IR_Expression](0))

    // construct rhs and matrix
    for (i <- 0 until unknowns.length) {
      var innerStmts = ListBuffer[IR_Statement]()
      var boundaryStmts = ListBuffer[IR_Statement]()

      innerStmts += AssignmentStatement(hackVecComponentAccess(f, i), fVals(i))
      for (j <- 0 until unknowns.length)
        innerStmts += AssignmentStatement(hackMatComponentAccess(A, i, j), AVals(i)(j))

      boundaryStmts += AssignmentStatement(hackVecComponentAccess(f, i), unknowns(i))
      for (j <- 0 until unknowns.length)
        boundaryStmts += AssignmentStatement(hackMatComponentAccess(A, i, j), if (i == j) 1 else 0)

      // check if current unknown is on/ beyond boundary
      stmts += ConditionStatement(
        IsValidPoint(unknowns(i).fieldSelection, unknowns(i).index),
        innerStmts,
        boundaryStmts)
    }

    // solve local system - TODO: replace inverse function call with internal function
    stmts += AssignmentStatement(u, IR_MultiplicationExpression(MemberFunctionCallExpression(A, "inverse", ListBuffer()), f))

    // write back results
    for (i <- 0 until unknowns.length)
      stmts += new ConditionStatement(// don't write back result on boundaries
        IsValidPoint(unknowns(i).fieldSelection, unknowns(i).index),
        AssignmentStatement(unknowns(i), hackVecComponentAccess(u, i)))

    IR_Scope(stmts)
  }
}
