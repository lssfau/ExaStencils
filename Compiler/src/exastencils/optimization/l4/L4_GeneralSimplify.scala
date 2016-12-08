package exastencils.optimization.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger

/// L4_GeneralSimplify

object L4_GeneralSimplify extends DefaultStrategy("Simplify general expressions") {
  def doUntilDone(node : Option[Node] = None) = {
    do {
      apply(node)
    } while (results.last._2.matches > 0) // FIXME: cleaner code
  }

  def doUntilDoneStandalone(node : Node, compactAST : Boolean = false) = {
    val oldLvl = Logger.getLevel
    Logger.setLevel(Logger.WARNING)
    do {
      applyStandalone(node)
    } while (results.last._2.matches > 0) // FIXME: cleaner code
    Logger.setLevel(oldLvl)
  }

  this += new Transformation("Simplify", {
    // deal with constants
    case L4_Negative(L4_IntegerConstant(value)) => L4_IntegerConstant(-value)
    case L4_Negative(L4_RealConstant(value))    => L4_RealConstant(-value)

    case L4_Division(L4_IntegerConstant(left), L4_IntegerConstant(right)) => L4_IntegerConstant(left / right)
    case L4_Division(L4_IntegerConstant(left), L4_RealConstant(right))    => L4_RealConstant(left / right)
    case L4_Division(L4_RealConstant(left), L4_IntegerConstant(right))    => L4_RealConstant(left / right)
    case L4_Division(L4_RealConstant(left), L4_RealConstant(right))       => L4_RealConstant(left / right)

    case L4_Division(left : L4_Expression, L4_IntegerConstant(1))  => left
    case L4_Division(left : L4_Expression, L4_RealConstant(f))     => L4_Multiplication(left, L4_RealConstant(1.0 / f))
    case L4_Division(L4_RealConstant(0.0), right : L4_Expression)  => L4_RealConstant(0.0)
    case L4_Division(L4_IntegerConstant(0), right : L4_Expression) => L4_IntegerConstant(0)

    case L4_Modulo(L4_IntegerConstant(left), L4_IntegerConstant(right)) => L4_IntegerConstant(left % right)

    case L4_Power(L4_IntegerConstant(base), L4_IntegerConstant(exp)) => L4_IntegerConstant(pow(base, exp))
    case L4_Power(L4_RealConstant(base), L4_IntegerConstant(exp))    => L4_RealConstant(pow(base, exp))
    case L4_Power(L4_IntegerConstant(base), L4_RealConstant(exp))    => L4_RealConstant(math.pow(base, exp))
    case L4_Power(L4_RealConstant(base), L4_RealConstant(exp))       => L4_RealConstant(math.pow(base, exp))

    case L4_Power(base, L4_IntegerConstant(0))                     => L4_IntegerConstant(1)
    case L4_Power(base, L4_IntegerConstant(1))                     => base
    case L4_Power(base, L4_IntegerConstant(e)) if e >= 2 && e <= 6 => L4_Multiplication(ListBuffer.fill(e.toInt)(Duplicate(base)))
    case L4_Power(b, L4_RealConstant(e)) if e.toLong.toDouble == e => L4_Power(b, L4_IntegerConstant(e.toLong))

    // deal with negatives
    case L4_Negative(L4_Negative(expr))           => expr
    case L4_Negative(L4_Addition(sums))           => L4_Addition(sums.transform { s => L4_Negative(s) })
    case L4_Negative(L4_Subtraction(left, right)) => L4_Subtraction(right, left)

    case L4_Division(L4_Negative(l), L4_Negative(r)) => L4_Division(l, r)
    case L4_Division(l, L4_Negative(r))              => L4_Negative(L4_Division(l, r))
    case L4_Division(L4_Negative(l), r)              => L4_Negative(L4_Division(l, r))

    case L4_Negative(L4_Maximum(exps)) => L4_Minimum(exps.transform { s => L4_Negative(s) })
    case L4_Negative(L4_Minimum(exps)) => L4_Maximum(exps.transform { s => L4_Negative(s) })

    //    TODO: enable once supported
    //    // Simplify vectors
    //    case L4_Negative(v : L4_VectorExpression) =>
    //      L4_VectorExpression(v.innerDatatype, v.expressions.map { x => L4_Negative(x) }, v.rowVector)
    //
    //    // Simplify matrices
    //    case L4_Negative(m : L4_MatrixExpression) =>
    //      L4_MatrixExpression(m.innerDatatype, m.expressions.map { x => x.map { y => L4_Negative(y) : L4_Expression } })

    case L4_Scope(ListBuffer(L4_Scope(body))) => L4_Scope(body)

    case L4_IfCondition(cond, ListBuffer(L4_Scope(trueBody)), falseBody) => L4_IfCondition(cond, trueBody, falseBody)
    case L4_IfCondition(cond, trueBody, ListBuffer(L4_Scope(falseBody))) => L4_IfCondition(cond, trueBody, falseBody)

    case L4_EqEq(L4_IntegerConstant(left), L4_IntegerConstant(right))         => L4_BooleanConstant(left == right)
    case L4_Neq(L4_IntegerConstant(left), L4_IntegerConstant(right))          => L4_BooleanConstant(left != right)
    case L4_Lower(L4_IntegerConstant(left), L4_IntegerConstant(right))        => L4_BooleanConstant(left < right)
    case L4_LowerEqual(L4_IntegerConstant(left), L4_IntegerConstant(right))   => L4_BooleanConstant(left <= right)
    case L4_Greater(L4_IntegerConstant(left), L4_IntegerConstant(right))      => L4_BooleanConstant(left > right)
    case L4_GreaterEqual(L4_IntegerConstant(left), L4_IntegerConstant(right)) => L4_BooleanConstant(left >= right)

    case L4_Negation(L4_BooleanConstant(b)) => L4_BooleanConstant(!b)

    case L4_Negation(L4_EqEq(left, right)) => L4_Neq(left, right)
    case L4_Negation(L4_Neq(left, right))  => L4_EqEq(left, right)

    case L4_Negation(L4_Lower(left, right))        => L4_GreaterEqual(left, right)
    case L4_Negation(L4_GreaterEqual(left, right)) => L4_Lower(left, right)
    case L4_Negation(L4_LowerEqual(left, right))   => L4_Greater(left, right)
    case L4_Negation(L4_Greater(left, right))      => L4_LowerEqual(left, right)

    case L4_Negation(L4_AndAnd(left, right)) => L4_OrOr(L4_Negation(left), L4_Negation(right))
    case L4_Negation(L4_OrOr(left, right))   => L4_AndAnd(L4_Negation(left), L4_Negation(right))

    case L4_AndAnd(L4_BooleanConstant(true), expr : L4_Expression)  => expr
    case L4_AndAnd(expr : L4_Expression, L4_BooleanConstant(true))  => expr
    case L4_AndAnd(L4_BooleanConstant(false), expr : L4_Expression) => L4_BooleanConstant(false)
    case L4_AndAnd(expr : L4_Expression, L4_BooleanConstant(false)) => L4_BooleanConstant(false)

    case L4_OrOr(L4_BooleanConstant(true), expr : L4_Expression)  => L4_BooleanConstant(true)
    case L4_OrOr(expr : L4_Expression, L4_BooleanConstant(true))  => L4_BooleanConstant(true)
    case L4_OrOr(L4_BooleanConstant(false), expr : L4_Expression) => expr
    case L4_OrOr(expr : L4_Expression, L4_BooleanConstant(false)) => expr

    case L4_IfCondition(L4_BooleanConstant(cond), tBranch, fBranch) =>
      if (cond) {
        if (tBranch.isEmpty) L4_NullStatement else tBranch
      } else {
        if (fBranch.isEmpty) L4_NullStatement else fBranch
      }
  })

  private def pow(a : Long, b : Long) : Long = {
    if (b < 0)
      return 0
    var base : Long = a
    var exp : Long = b
    var res : Long = 1
    while (exp > 0) {
      if ((exp & 1) != 0)
        res *= base
      exp >>= 1
      base *= base
    }
    res
  }

  private def pow(a : Double, b : Long) : Double = {
    if (b < 0)
      return 0
    var base : Double = a
    var exp : Long = b
    var res : Double = 1
    while (exp > 0) {
      if ((exp & 1) != 0)
        res *= base
      exp >>= 1
      base *= base
    }
    res
  }
}