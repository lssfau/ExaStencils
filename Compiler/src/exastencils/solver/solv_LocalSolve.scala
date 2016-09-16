package exastencils.solver

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.IsValidPoint
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.strategies.SimplifyStrategy

/// IR_ResolveLocalSolve

object IR_ResolveLocalSolve extends DefaultStrategy("Resolve IR_LocalSolve nodes") {
  this += new Transformation("Perform expandSpecial for applicable nodes", {
    case solve : IR_LocalSolve => solve.expandSpecial
  })
}

/// IR_Equation

// TODO: move eq node to more fitting file/package
case class IR_Equation(var lhs : IR_Expression, var rhs : IR_Expression) extends IR_Node

/// IR_LocalSolve

case class IR_LocalSolve(var unknowns : ListBuffer[IR_FieldAccess], var equations : ListBuffer[IR_Equation]) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  var fVals = ListBuffer[IR_AdditionExpression]()
  var AVals = ListBuffer[ListBuffer[IR_AdditionExpression]]()

  def validate() = {
    // TODO
  }

  def matchUnknowns(other : IR_FieldAccess) : Int = {
    for (i <- unknowns.indices) {
      if (other.fieldSelection.field.codeName == unknowns(i).fieldSelection.field.codeName
        && other.index == unknowns(i).index)
        return i // match
    }
    -1 // no match => constant value
  }

  def processExpression(pos : Int, ex : IR_Expression, switchSign : Boolean) : Unit = {
    ex match {
      case const : IR_Number => fVals(pos).summands += (if (switchSign) const else IR_NegativeExpression(const))

      case IR_NegativeExpression(exp) => processExpression(pos, exp, !switchSign)

      case access : IR_FieldAccess => {
        val uPos = matchUnknowns(access)
        if (uPos < 0)
          fVals(pos).summands += (if (switchSign) access else IR_NegativeExpression(access)) // no match -> rhs
        else
          AVals(pos)(uPos).summands += IR_RealConstant(if (switchSign) -1 else 1) // match -> matrix
      }

      case multEx @ IR_MultiplicationExpression(factors) => {
        // split into known and unknown
        var localFactors = ListBuffer[IR_Expression]()
        var localUnknowns = ListBuffer[IR_FieldAccess]()
        for (ex <- factors) {
          ex match {
            case access : IR_FieldAccess         =>
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
    val zeroEqs = equations.map(eq => Duplicate(eq.lhs - eq.rhs) : IR_Expression)
    for (eq <- zeroEqs)
      SimplifyStrategy.doUntilDoneStandalone(eq, true)

    // scan lhs for constants
    for (eqNumber <- zeroEqs.indices) {
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

    def u = IR_VariableAccess("_local_unknowns", IR_VectorDatatype(IR_RealDatatype, unknowns.length, Some(false)))
    def f = IR_VariableAccess("_local_rhs", IR_VectorDatatype(IR_RealDatatype, unknowns.length, Some(false)))
    def A = IR_VariableAccess("_local_matrix", IR_MatrixDatatype(IR_RealDatatype, unknowns.length, unknowns.length))

    // declare local variables -> to be merged later
    stmts += IR_VariableDeclaration(u)
    stmts += IR_VariableDeclaration(f)
    stmts += IR_VariableDeclaration(A)

    // initialize with zero - TODO: adapt to new matrix types
    stmts += IR_MemberFunctionCall(u, "set", ListBuffer[IR_Expression](0))
    stmts += IR_MemberFunctionCall(f, "set", ListBuffer[IR_Expression](0))
    stmts += IR_MemberFunctionCall(A, "set", ListBuffer[IR_Expression](0))

    // construct rhs and matrix
    for (i <- unknowns.indices) {
      var innerStmts = ListBuffer[IR_Statement]()
      var boundaryStmts = ListBuffer[IR_Statement]()

      innerStmts += IR_Assignment(IR_HackVecComponentAccess(f, i), fVals(i))
      for (j <- unknowns.indices)
        innerStmts += IR_Assignment(IR_HackMatComponentAccess(A, i, j), AVals(i)(j))

      boundaryStmts += IR_Assignment(IR_HackVecComponentAccess(f, i), unknowns(i))
      for (j <- unknowns.indices)
        boundaryStmts += IR_Assignment(IR_HackMatComponentAccess(A, i, j), if (i == j) 1 else 0)

      // check if current unknown is on/ beyond boundary
      stmts += IR_IfCondition(
        IsValidPoint(unknowns(i).fieldSelection, unknowns(i).index),
        innerStmts,
        boundaryStmts)
    }

    // solve local system - TODO: replace inverse function call with internal function
    stmts += IR_Assignment(u, IR_MultiplicationExpression(IR_MemberFunctionCall(A, "inverse"), f))

    // write back results
    for (i <- unknowns.indices)
      stmts += IR_IfCondition(// don't write back result on boundaries
        IsValidPoint(unknowns(i).fieldSelection, unknowns(i).index),
        IR_Assignment(unknowns(i), IR_HackVecComponentAccess(u, i)))

    IR_Scope(stmts)
  }
}
