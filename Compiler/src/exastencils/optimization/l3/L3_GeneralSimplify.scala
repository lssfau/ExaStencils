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

package exastencils.optimization.l3

import scala.collection.mutable
import scala.collection.mutable.{ ArrayBuffer, ListBuffer, Queue }

import exastencils.base.l3._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger

/// L3_GeneralSimplify

object L3_GeneralSimplify extends DefaultStrategy("Simplify general expressions") {
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
    case add : L3_Addition =>
      val nju = simplifyAdd(add.summands)
      if (nju == add)
        negMatches += 1
      nju

    case sub : L3_Subtraction =>
      val nju = simplifyAdd(List(sub))
      if (nju == sub)
        negMatches += 1
      nju

    case mult : L3_Multiplication =>
      val nju = simplifyMult(mult.factors)
      if (nju == mult)
        negMatches += 1
      nju

    case old @ L3_Negative(L3_Multiplication(facs)) =>
      val nju = simplifyMult(facs.clone() += L3_IntegerConstant(-1L))
      if (nju == old)
        negMatches += 1
      nju

    // deal with constants
    case L3_Negative(L3_IntegerConstant(value)) => L3_IntegerConstant(-value)
    case L3_Negative(L3_RealConstant(value))    => L3_RealConstant(-value)

    case L3_Division(L3_IntegerConstant(left), L3_IntegerConstant(right)) => L3_IntegerConstant(left / right)
    case L3_Division(L3_IntegerConstant(left), L3_RealConstant(right))    => L3_RealConstant(left / right)
    case L3_Division(L3_RealConstant(left), L3_IntegerConstant(right))    => L3_RealConstant(left / right)
    case L3_Division(L3_RealConstant(left), L3_RealConstant(right))       => L3_RealConstant(left / right)

    case L3_Division(left : L3_Expression, L3_IntegerConstant(1))  => left
    case L3_Division(left : L3_Expression, L3_RealConstant(f))     => L3_Multiplication(left, L3_RealConstant(1.0 / f))
    case L3_Division(L3_RealConstant(0.0), right : L3_Expression)  => L3_RealConstant(0.0)
    case L3_Division(L3_IntegerConstant(0), right : L3_Expression) => L3_IntegerConstant(0)

    case L3_Modulo(L3_IntegerConstant(left), L3_IntegerConstant(right)) => L3_IntegerConstant(left % right)

    case L3_Power(L3_IntegerConstant(base), L3_IntegerConstant(exp)) => L3_IntegerConstant(pow(base, exp))
    case L3_Power(L3_RealConstant(base), L3_IntegerConstant(exp))    => L3_RealConstant(pow(base, exp))
    case L3_Power(L3_IntegerConstant(base), L3_RealConstant(exp))    => L3_RealConstant(math.pow(base, exp))
    case L3_Power(L3_RealConstant(base), L3_RealConstant(exp))       => L3_RealConstant(math.pow(base, exp))

    case L3_Power(base, L3_IntegerConstant(0))                     => L3_IntegerConstant(1)
    case L3_Power(base, L3_IntegerConstant(1))                     => base
    case L3_Power(base, L3_IntegerConstant(e)) if e >= 2 && e <= 6 => L3_Multiplication(ListBuffer.fill(e.toInt)(Duplicate(base)))
    case L3_Power(b, L3_RealConstant(e)) if e.toLong.toDouble == e => L3_Power(b, L3_IntegerConstant(e.toLong))

    // deal with negatives
    case L3_Negative(L3_Negative(expr))           => expr
    case L3_Negative(L3_Addition(sums))           => L3_Addition(sums.transform { s => L3_Negative(s) })
    case L3_Negative(L3_Subtraction(left, right)) => L3_Subtraction(right, left)

    case L3_Division(L3_Negative(l), L3_Negative(r)) => L3_Division(l, r)
    case L3_Division(l, L3_Negative(r))              => L3_Negative(L3_Division(l, r))
    case L3_Division(L3_Negative(l), r)              => L3_Negative(L3_Division(l, r))

    case L3_Negative(L3_Maximum(exps)) => L3_Minimum(exps.transform { s => L3_Negative(s) })
    case L3_Negative(L3_Minimum(exps)) => L3_Maximum(exps.transform { s => L3_Negative(s) })

    // Simplify vectors

    case L3_EqEq(L3_IntegerConstant(left), L3_IntegerConstant(right))         => L3_BooleanConstant(left == right)
    case L3_Neq(L3_IntegerConstant(left), L3_IntegerConstant(right))          => L3_BooleanConstant(left != right)
    case L3_Lower(L3_IntegerConstant(left), L3_IntegerConstant(right))        => L3_BooleanConstant(left < right)
    case L3_LowerEqual(L3_IntegerConstant(left), L3_IntegerConstant(right))   => L3_BooleanConstant(left <= right)
    case L3_Greater(L3_IntegerConstant(left), L3_IntegerConstant(right))      => L3_BooleanConstant(left > right)
    case L3_GreaterEqual(L3_IntegerConstant(left), L3_IntegerConstant(right)) => L3_BooleanConstant(left >= right)

    case L3_Negation(L3_BooleanConstant(b)) => L3_BooleanConstant(!b)

    case L3_Negation(L3_EqEq(left, right)) => L3_Neq(left, right)
    case L3_Negation(L3_Neq(left, right))  => L3_EqEq(left, right)

    case L3_Negation(L3_Lower(left, right))        => L3_GreaterEqual(left, right)
    case L3_Negation(L3_GreaterEqual(left, right)) => L3_Lower(left, right)
    case L3_Negation(L3_LowerEqual(left, right))   => L3_Greater(left, right)
    case L3_Negation(L3_Greater(left, right))      => L3_LowerEqual(left, right)

    case L3_Negation(L3_AndAnd(left, right)) => L3_OrOr(L3_Negation(left), L3_Negation(right))
    case L3_Negation(L3_OrOr(left, right))   => L3_AndAnd(L3_Negation(left), L3_Negation(right))

    case L3_AndAnd(L3_BooleanConstant(true), expr : L3_Expression)  => expr
    case L3_AndAnd(expr : L3_Expression, L3_BooleanConstant(true))  => expr
    case L3_AndAnd(L3_BooleanConstant(false), expr : L3_Expression) => L3_BooleanConstant(false)
    case L3_AndAnd(expr : L3_Expression, L3_BooleanConstant(false)) => L3_BooleanConstant(false)

    case L3_OrOr(L3_BooleanConstant(true), expr : L3_Expression)  => L3_BooleanConstant(true)
    case L3_OrOr(expr : L3_Expression, L3_BooleanConstant(true))  => L3_BooleanConstant(true)
    case L3_OrOr(L3_BooleanConstant(false), expr : L3_Expression) => expr
    case L3_OrOr(expr : L3_Expression, L3_BooleanConstant(false)) => expr
  })

  private def simplifyAdd(sum : Seq[L3_Expression]) : L3_Expression = {
    var intCst : Long = 0L
    var floatCst : Double = 0d
    var vecPos : Boolean = true
    var matPos : Boolean = true
    val workQ = new Queue[(L3_Expression, Boolean)]()
    val posSums = new ListBuffer[L3_Expression]()
    val negSums = new ListBuffer[L3_Expression]()
    for (s <- sum) {
      workQ.enqueue((s, true)) // for nested Additions; this allows in-order processing
      do {
        val (expr, pos) = workQ.dequeue()
        expr match {
          case L3_IntegerConstant(i)       => if (pos) intCst += i else intCst -= i
          case L3_RealConstant(f)          => if (pos) floatCst += f else floatCst -= f
          case L3_Addition(sums)           => workQ.enqueue(sums.view.map { x => (x, pos) } : _*)
          case L3_Negative(e)              => workQ.enqueue((e, !pos))
          case L3_Subtraction(left, right) =>
            workQ.enqueue((left, pos))
            workQ.enqueue((right, !pos))
          // if some more simplifications with vectors or matrices are requl2ed, a similar approach than for a
          //    Multiplication is possible here
          case e : L3_Expression =>
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
        posSums += L3_RealConstant(cst)
      else
        negSums += L3_RealConstant(-cst)
    } else if (intCst != 0L)
    // if compactAST is set, no Subtraction is created, so prevent creating a Neg(Const),
    //   which would lead to a non-terminating recursion
    // if posSums is empty we do not want to add the constant to the negSums, which would also result in a Neg(Const) -> non-terminating
    if (intCst > 0 || compactAST || posSums.isEmpty)
      posSums += L3_IntegerConstant(intCst)
    else
      negSums += L3_IntegerConstant(-intCst)

    if (posSums.length + negSums.length <= 1) { // result is only one summand (either a positive, or a negative, or 0)
      (posSums ++= negSums.transform(x => L3_Negative(x)) += L3_IntegerConstant(0L)).head

    } else if (posSums.length * negSums.length == 0 || compactAST) { // if compactAST is set do not create any Subtraction
      L3_Addition(posSums ++= negSums.transform(x => L3_Negative(x)))

    } else {
      val posExpr = if (posSums.length == 1) posSums.head else new L3_Addition(posSums)
      val negExpr = if (negSums.length == 1) negSums.head else new L3_Addition(negSums)
      L3_Subtraction(posExpr, negExpr)
    }
  }

  private def simplifyMult(facs : Seq[L3_Expression]) : L3_Expression = {
    var intCst : Long = 1L
    var floatCst : Double = 1d
    var workQ = mutable.Queue[L3_Expression]()
    val remA = new ArrayBuffer[L3_Expression]() // use ArrayBuffer here for a more efficient access to the last element
    var div : L3_Division = null
    for (f <- facs) {
      workQ.enqueue(f) // for nested Multiplication; this allows in-order processing
      do {
        val expr = workQ.dequeue()
        expr match {
          case L3_IntegerConstant(iv)                  => intCst *= iv
          case L3_RealConstant(fv)                     => floatCst *= fv
          case L3_Negative(e)                          =>
            workQ = (mutable.Queue() :+ e) ++ workQ
            intCst = -intCst
          case L3_Multiplication(iFacs)                =>
            workQ = mutable.Queue() ++ iFacs ++ workQ
          case d @ L3_Division(L3_RealConstant(fv), _) =>
            floatCst *= fv
            d.left = L3_RealConstant(1.0)
            if (div == null)
              div = d
            remA += d
          case r : L3_Expression                       =>
            remA += r
        }
      } while (workQ.nonEmpty)
    }
    val rem = remA.to[ListBuffer]
    var cstDt : Option[L3_Datatype] = None
    val negative : Boolean = floatCst * intCst < 0d
    floatCst = math.abs(floatCst)
    intCst = math.abs(intCst)
    if (floatCst * intCst == 0d) {
      rem.clear()
      rem += L3_IntegerConstant(0L) // TODO: fix type
    } else if (div != null) {
      div.left = L3_RealConstant(floatCst * intCst)
    } else if (floatCst != 1d) {
      L3_RealConstant(floatCst * intCst) +=: rem // add constant at fl2st position (it is expected as rem.head later)
      cstDt = Some(L3_RealDatatype)
    } else if (intCst != 1L) {
      L3_IntegerConstant(intCst) +=: rem // add constant at fl2st position (it is expected as rem.head later)
      cstDt = Some(L3_IntegerDatatype)
    }

    var result : L3_Expression = null
    if (rem.isEmpty) {
      result = L3_IntegerConstant(1L) // TODO: fix type

    } else if (rem.length == 1 || floatCst * intCst == 0d) {
      result = rem.head

    } else {
      result = L3_Multiplication(rem)
    }

    if (negative)
      result = L3_Negative(result)
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
