//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.optimization.l4

import scala.collection.mutable
import scala.collection.mutable.{ ArrayBuffer, ListBuffer, Queue }

import exastencils.base.l4._
import exastencils.baseExt.l4._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger

/// L4_GeneralSimplify

object L4_GeneralSimplify extends DefaultStrategy("Simplify general expressions") {
  // hack: since Addition and Multiplication lead always to a match, we don't count these if nothing was changed
  var negMatches : Int = 0
  var compactAST : Boolean = false

  def doUntilDone(node : Option[Node] = None) = {
    do {
      negMatches = 0
      apply(node)
    } while (results.last._2.matches - negMatches > 0) // FIXME: cleaner code
  }

  def doUntilDoneStandalone(node : Node, compactAST : Boolean = false) = {
    this.compactAST = compactAST
    val oldLvl = Logger.getLevel
    Logger.setLevel(Logger.WARNING)
    do {
      negMatches = 0
      applyStandalone(node)
    } while (results.last._2.matches - negMatches > 0) // FIXME: cleaner code
    Logger.setLevel(oldLvl)
    this.compactAST = false
  }

  this += new Transformation("Simplify", {
    case add : L4_Addition =>
      val nju = simplifyAdd(add.summands)
      if (nju == add)
        negMatches += 1
      nju

    case sub : L4_Subtraction =>
      val nju = simplifyAdd(List(sub))
      if (nju == sub)
        negMatches += 1
      nju

    case mult : L4_Multiplication =>
      val nju = simplifyMult(mult.factors)
      if (nju == mult)
        negMatches += 1
      nju

    case old @ L4_Negative(L4_Multiplication(facs)) =>
      val nju = simplifyMult(facs.clone() += L4_IntegerConstant(-1L))
      if (nju == old)
        negMatches += 1
      nju

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

    case L4_Scope(ListBuffer(L4_Scope(body))) => L4_Scope(body)

    case L4_IfCondition(cond, ListBuffer(L4_Scope(trueBody)), falseBody) => L4_IfCondition(cond, trueBody, falseBody)
    case L4_IfCondition(cond, trueBody, ListBuffer(L4_Scope(falseBody))) => L4_IfCondition(cond, trueBody, falseBody)
    case l @ L4_ForLoop(_, _, ListBuffer(L4_Scope(body)))                =>
      l.body = body; l // preserve ForLoopStatement instance to ensure all traits are still present

    case L4_ForLoop(_, _, stmts) if Knowledge.experimental_eliminateEmptyLoops && (stmts.isEmpty || stmts.forall(_ == L4_NullStatement)) => L4_NullStatement

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

    case L4_IfCondition(_, tBranch, fBranch) if Knowledge.experimental_eliminateEmptyConditions && tBranch.isEmpty && fBranch.isEmpty => L4_NullStatement

    case L4_IfCondition(L4_BooleanConstant(cond), tBranch, fBranch) =>
      if (cond) {
        if (tBranch.isEmpty) L4_NullStatement else tBranch
      } else {
        if (fBranch.isEmpty) L4_NullStatement else fBranch
      }

    case L4_IfCondition(L4_IntegerConstant(cond), tBranch, fBranch) if Knowledge.experimental_eliminateIntConditions =>
      if (cond != 0) {
        if (tBranch.isEmpty) L4_NullStatement else tBranch
      } else {
        if (fBranch.isEmpty) L4_NullStatement else fBranch
      }
  })

  private def simplifyAdd(sum : Seq[L4_Expression]) : L4_Expression = {
    var intCst : Long = 0L
    var floatCst : Double = 0d
    var vecExpr : L4_VectorExpression = null
    var vecPos : Boolean = true
    var matExpr : L4_MatrixExpression = null
    var matPos : Boolean = true
    val workQ = new Queue[(L4_Expression, Boolean)]()
    val posSums = new ListBuffer[L4_Expression]()
    val negSums = new ListBuffer[L4_Expression]()
    for (s <- sum) {
      workQ.enqueue((s, true)) // for nested Additions; this allows in-order processing
      do {
        val (expr, pos) = workQ.dequeue()
        expr match {
          case L4_IntegerConstant(i)       => if (pos) intCst += i else intCst -= i
          case L4_RealConstant(f)          => if (pos) floatCst += f else floatCst -= f
          case L4_Addition(sums)           => workQ.enqueue(sums.view.map { x => (x, pos) } : _*)
          case L4_Negative(e)              => workQ.enqueue((e, !pos))
          case L4_Subtraction(left, right) =>
            workQ.enqueue((left, pos))
            workQ.enqueue((right, !pos))
          case e : L4_Expression           =>
            if (pos)
              posSums += e
            else
              negSums += e
        }
      } while (workQ.nonEmpty)
    }

    // add constant at last position
    if (floatCst != 0d) {
      val cst : Double = floatCst + intCst
      // if compactAST is set, no Subtraction is created, so prevent creating a Neg(Const),
      //   which would lead to a non-terminating recursion
      // if posSums is empty we do not want to add the constant to the negSums, which would also result in a Neg(Const) -> non-terminating
      if (cst > 0.0 || compactAST || posSums.isEmpty)
        posSums += L4_RealConstant(cst)
      else
        negSums += L4_RealConstant(-cst)
    } else if (intCst != 0L)
    // if compactAST is set, no Subtraction is created, so prevent creating a Neg(Const),
    //   which would lead to a non-terminating recursion
    // if posSums is empty we do not want to add the constant to the negSums, which would also result in a Neg(Const) -> non-terminating
    if (intCst > 0 || compactAST || posSums.isEmpty)
      posSums += L4_IntegerConstant(intCst)
    else
      negSums += L4_IntegerConstant(-intCst)

    if (vecExpr != null) {
      if (posSums.isEmpty && negSums.isEmpty)
        vecExpr
      else
        Logger.error("Unable to add VectorExpression with other Expression types")

    } else if (matExpr != null) {
      if (posSums.isEmpty && negSums.isEmpty) {
        matExpr
      } else {
        L4_Addition(ListBuffer(matExpr) ++ posSums ++ negSums.transform(L4_Negative(_)))
      }

    } else if (posSums.length + negSums.length <= 1) { // result is only one summand (either a positive, or a negative, or 0)
      (posSums ++= negSums.transform(x => L4_Negative(x)) += L4_IntegerConstant(0L)).head

    } else if (posSums.length * negSums.length == 0 || compactAST) { // if compactAST is set do not create any Subtraction
      L4_Addition(posSums ++= negSums.transform(x => L4_Negative(x)))

    } else {
      val posExpr = if (posSums.length == 1) posSums.head else new L4_Addition(posSums)
      val negExpr = if (negSums.length == 1) negSums.head else new L4_Addition(negSums)
      L4_Subtraction(posExpr, negExpr)
    }
  }

  private def simplifyMult(facs : Seq[L4_Expression]) : L4_Expression = {
    var intCst : Long = 1L
    var floatCst : Double = 1d
    var workQ = mutable.Queue[L4_Expression]()
    val remA = new ArrayBuffer[L4_Expression]() // use ArrayBuffer here for a more efficient access to the last element
    var div : L4_Division = null
    for (f <- facs) {
      workQ.enqueue(f) // for nested Multiplication; this allows in-order processing
      do {
        val expr = workQ.dequeue()
        expr match {
          case L4_IntegerConstant(iv)                            => intCst *= iv
          case L4_RealConstant(fv)                               => floatCst *= fv
          case L4_Negative(e)                                    =>
            workQ = (mutable.Queue() :+ e) ++ workQ
            intCst = -intCst
          case L4_Multiplication(iFacs)                          =>
            workQ = mutable.Queue() ++ iFacs ++ workQ
          case d @ L4_Division(L4_RealConstant(fv), _)           =>
            floatCst *= fv
            d.left = L4_RealConstant(1.0)
            if (div == null)
              div = d
            remA += d
          case _ : L4_VectorExpression | _ : L4_MatrixExpression =>
            if (remA.isEmpty)
              remA += expr
            else {
              // merging with one previous only is sufficient, if simplifyMult only matches fl4st arg with vect/mat types
              val last = remA.last
              remA.trimEnd(1)
              remA ++= List(last, expr)
            }
          case r : L4_Expression                                 =>
            remA += r
        }
      } while (workQ.nonEmpty)
    }
    val rem = remA.to[ListBuffer]
    var cstDt : Option[L4_Datatype] = None
    val negative : Boolean = floatCst * intCst < 0d
    floatCst = math.abs(floatCst)
    intCst = math.abs(intCst)
    if (floatCst * intCst == 0d) {
      rem.clear()
      rem += L4_IntegerConstant(0L) // TODO: fix type
    } else if (div != null) {
      div.left = L4_RealConstant(floatCst * intCst)
    } else if (floatCst != 1d) {
      L4_RealConstant(floatCst * intCst) +=: rem // add constant at fl4st position (it is expected as rem.head later)
      cstDt = Some(L4_RealDatatype)
    } else if (intCst != 1L) {
      L4_IntegerConstant(intCst) +=: rem // add constant at fl4st position (it is expected as rem.head later)
      cstDt = Some(L4_IntegerDatatype)
    }

    var result : L4_Expression = null
    if (rem.isEmpty) {
      result = L4_IntegerConstant(1L) // TODO: fix type

    } else if (rem.length == 1 || floatCst * intCst == 0d) {
      result = rem.head

    } else {
      result = L4_Multiplication(rem)
    }

    if (negative)
      result = L4_Negative(result)
    result
  }

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
