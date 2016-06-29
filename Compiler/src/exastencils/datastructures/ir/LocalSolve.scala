package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer

import exastencils.communication._
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.logger._
import exastencils.prettyprinting._
import exastencils.strategies._

// FIXME: update with actual accessors
case class hackVecComponentAccess(var vec : VariableAccess, var i : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << vec << "(" << i << ", " << 0 << ")"
}
// FIXME: update with actual accessors
case class hackMatComponentAccess(var mat : VariableAccess, var i : Expression, var j : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << mat << "(" << i << ", " << j << ")"
}

case class EquationExpression(var lhs : Expression, var rhs : Expression) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = EquationExpression\n"
}

case class SolveLocallyStatement(var unknowns : ListBuffer[FieldAccess], var equations : ListBuffer[EquationExpression]) extends Statement {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = SolveLocallyStatement\n"

  var fVals = ListBuffer[AdditionExpression]()
  var AVals = ListBuffer[ListBuffer[AdditionExpression]]()

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

  def processExpression(pos : Int, ex : Expression, switchSign : Boolean) : Unit = {
    ex match {
      case const : Number          => fVals(pos).summands += (if (switchSign) const else NegativeExpression(const))

      case NegativeExpression(exp) => processExpression(pos, exp, !switchSign)

      case access : FieldAccess => {
        val uPos = matchUnknowns(access)
        if (uPos < 0)
          fVals(pos).summands += (if (switchSign) access else NegativeExpression(access)) // no match -> rhs
        else
          AVals(pos)(uPos).summands += FloatConstant(1) // match -> matrix
      }

      case multEx @ MultiplicationExpression(factors) => {
        // split into known and unknown
        var localFactors = ListBuffer[Expression]()
        var localUnknowns = ListBuffer[FieldAccess]()
        for (ex <- factors) {
          ex match {
            case access : FieldAccess =>
              if (matchUnknowns(access) < 0)
                localFactors += access
              else localUnknowns += access
            case e : MultiplicationExpression =>
              Logger.warn(s"Nested multiplication expressions are currently unsupported: $e")
              localFactors += e
            case e : AdditionExpression =>
              Logger.warn(s"Nested addition expressions are currently unsupported: $e")
              localFactors += e
            case e : Expression => localFactors += e
          }
        }
        if (localUnknowns.size > 1)
          Logger.warn("Non-linear equations are currently unsupported")
        if (localUnknowns.isEmpty) // no unknowns -> add to rhs
          fVals(pos).summands += (if (switchSign) multEx else NegativeExpression(multEx))
        else // unknowns detected -> add to matrix
          AVals(pos)(matchUnknowns(localUnknowns.head)).summands += (
            if (switchSign)
              NegativeExpression(MultiplicationExpression(localFactors))
            else
              MultiplicationExpression(localFactors))
      }

      case _ => Logger.warn(s"Found unsupported node type ${ex.getClass.getTypeName}: $ex")
    }
  }

  def processEqSummands(pos : Int, summands : ListBuffer[Expression], switchSign : Boolean = false) = {
    for (ex <- summands)
      processExpression(pos, ex, switchSign)
  }

  def sortEquations() = {
    // preparation: bring all entries to left side and simplify
    var zeroEqs = equations.map(eq => Duplicate(eq.lhs - eq.rhs) : Expression)
    for (eq <- zeroEqs)
      SimplifyStrategy.doUntilDoneStandalone(eq, true)

    // scan lhs for constants
    for (eqNumber <- 0 until zeroEqs.size) {
      zeroEqs(eqNumber) match {
        case AdditionExpression(adds) => processEqSummands(eqNumber, adds)
        case SubtractionExpression(pos, neg) =>
          pos match {
            case AdditionExpression(adds) => processEqSummands(eqNumber, adds)
            case e : Expression           => processEqSummands(eqNumber, ListBuffer(e))
          }
          neg match {
            case AdditionExpression(adds) => processEqSummands(eqNumber, adds, true)
            case e : Expression           => processEqSummands(eqNumber, ListBuffer(e), true)
          }
        case _ => Logger.warn(s"Equation doesn't hold enough information (${zeroEqs(eqNumber).getClass.getTypeName})")
      }
    }

    // unknowns to the lhs, constants to the rhs

  }

  def expandSpecial : Output[Scope] = {
    fVals = ListBuffer.fill(unknowns.length)(AdditionExpression(ListBuffer()))
    AVals = ListBuffer.fill(unknowns.length)(ListBuffer.fill(unknowns.length)(AdditionExpression(ListBuffer())))

    validate()
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

    // construct rhs and matrix
    for (i <- 0 until unknowns.length) {
      var innerStmts = ListBuffer[Statement]()
      var boundaryStmts = ListBuffer[Statement]()

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
    stmts += AssignmentStatement(u, MultiplicationExpression(ListBuffer(FunctionCallExpression("_local_matrix.inverse", ListBuffer()), f)))

    // write back results
    for (i <- 0 until unknowns.length)
      stmts += new ConditionStatement( // don't write back result on boundaries
        IsValidPoint(unknowns(i).fieldSelection, unknowns(i).index),
        AssignmentStatement(unknowns(i), hackVecComponentAccess(u, i)))

    Scope(stmts)
  }
}
