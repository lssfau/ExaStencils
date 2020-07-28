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

package exastencils.optimization.l2

import scala.collection.mutable
import scala.collection.mutable.{ ArrayBuffer, ListBuffer, Queue }

import exastencils.base.l2._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger

/// L2_GeneralSimplify

object L2_GeneralSimplify extends DefaultStrategy("Simplify general expressions") {
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
    case add : L2_Addition =>
      val nju = simplifyAdd(add.summands)
      if (nju == add)
        negMatches += 1
      nju

    case sub : L2_Subtraction =>
      val nju = simplifyAdd(List(sub))
      if (nju == sub)
        negMatches += 1
      nju

    case mult : L2_Multiplication =>
      val nju = simplifyMult(mult.factors)
      if (nju == mult)
        negMatches += 1
      nju

    case old @ L2_Negative(L2_Multiplication(facs)) =>
      val nju = simplifyMult(facs.clone() += L2_IntegerConstant(-1L))
      if (nju == old)
        negMatches += 1
      nju

    // deal with constants
    case L2_Negative(L2_IntegerConstant(value)) => L2_IntegerConstant(-value)
    case L2_Negative(L2_RealConstant(value))    => L2_RealConstant(-value)

    case L2_Division(L2_IntegerConstant(left), L2_IntegerConstant(right)) => L2_IntegerConstant(left / right)
    case L2_Division(L2_IntegerConstant(left), L2_RealConstant(right))    => L2_RealConstant(left / right)
    case L2_Division(L2_RealConstant(left), L2_IntegerConstant(right))    => L2_RealConstant(left / right)
    case L2_Division(L2_RealConstant(left), L2_RealConstant(right))       => L2_RealConstant(left / right)

    case L2_Division(left : L2_Expression, L2_IntegerConstant(1))  => left
    case L2_Division(left : L2_Expression, L2_RealConstant(f))     => L2_Multiplication(left, L2_RealConstant(1.0 / f))
    case L2_Division(L2_RealConstant(0.0), right : L2_Expression)  => L2_RealConstant(0.0)
    case L2_Division(L2_IntegerConstant(0), right : L2_Expression) => L2_IntegerConstant(0)

    case L2_Modulo(L2_IntegerConstant(left), L2_IntegerConstant(right)) => L2_IntegerConstant(left % right)

    case L2_Power(L2_IntegerConstant(base), L2_IntegerConstant(exp)) => L2_IntegerConstant(pow(base, exp))
    case L2_Power(L2_RealConstant(base), L2_IntegerConstant(exp))    => L2_RealConstant(pow(base, exp))
    case L2_Power(L2_IntegerConstant(base), L2_RealConstant(exp))    => L2_RealConstant(math.pow(base, exp))
    case L2_Power(L2_RealConstant(base), L2_RealConstant(exp))       => L2_RealConstant(math.pow(base, exp))

    case L2_Power(base, L2_IntegerConstant(0))                     => L2_IntegerConstant(1)
    case L2_Power(base, L2_IntegerConstant(1))                     => base
    case L2_Power(base, L2_IntegerConstant(e)) if e >= 2 && e <= 6 => L2_Multiplication(ListBuffer.fill(e.toInt)(Duplicate(base)))
    case L2_Power(b, L2_RealConstant(e)) if e.toLong.toDouble == e => L2_Power(b, L2_IntegerConstant(e.toLong))

    // deal with negatives
    case L2_Negative(L2_Negative(expr))           => expr
    case L2_Negative(L2_Addition(sums))           => L2_Addition(sums.transform { s => L2_Negative(s) })
    case L2_Negative(L2_Subtraction(left, right)) => L2_Subtraction(right, left)

    case L2_Division(L2_Negative(l), L2_Negative(r)) => L2_Division(l, r)
    case L2_Division(l, L2_Negative(r))              => L2_Negative(L2_Division(l, r))
    case L2_Division(L2_Negative(l), r)              => L2_Negative(L2_Division(l, r))

    case L2_Negative(L2_Maximum(exps)) => L2_Minimum(exps.transform { s => L2_Negative(s) })
    case L2_Negative(L2_Minimum(exps)) => L2_Maximum(exps.transform { s => L2_Negative(s) })

    // Simplify vectors

    case L2_EqEq(L2_IntegerConstant(left), L2_IntegerConstant(right))         => L2_BooleanConstant(left == right)
    case L2_Neq(L2_IntegerConstant(left), L2_IntegerConstant(right))          => L2_BooleanConstant(left != right)
    case L2_Lower(L2_IntegerConstant(left), L2_IntegerConstant(right))        => L2_BooleanConstant(left < right)
    case L2_LowerEqual(L2_IntegerConstant(left), L2_IntegerConstant(right))   => L2_BooleanConstant(left <= right)
    case L2_Greater(L2_IntegerConstant(left), L2_IntegerConstant(right))      => L2_BooleanConstant(left > right)
    case L2_GreaterEqual(L2_IntegerConstant(left), L2_IntegerConstant(right)) => L2_BooleanConstant(left >= right)

    case L2_Negation(L2_BooleanConstant(b)) => L2_BooleanConstant(!b)

    case L2_Negation(L2_EqEq(left, right)) => L2_Neq(left, right)
    case L2_Negation(L2_Neq(left, right))  => L2_EqEq(left, right)

    case L2_Negation(L2_Lower(left, right))        => L2_GreaterEqual(left, right)
    case L2_Negation(L2_GreaterEqual(left, right)) => L2_Lower(left, right)
    case L2_Negation(L2_LowerEqual(left, right))   => L2_Greater(left, right)
    case L2_Negation(L2_Greater(left, right))      => L2_LowerEqual(left, right)

    case L2_Negation(L2_AndAnd(left, right)) => L2_OrOr(L2_Negation(left), L2_Negation(right))
    case L2_Negation(L2_OrOr(left, right))   => L2_AndAnd(L2_Negation(left), L2_Negation(right))

    case L2_AndAnd(L2_BooleanConstant(true), expr : L2_Expression)  => expr
    case L2_AndAnd(expr : L2_Expression, L2_BooleanConstant(true))  => expr
    case L2_AndAnd(L2_BooleanConstant(false), expr : L2_Expression) => L2_BooleanConstant(false)
    case L2_AndAnd(expr : L2_Expression, L2_BooleanConstant(false)) => L2_BooleanConstant(false)

    case L2_OrOr(L2_BooleanConstant(true), expr : L2_Expression)  => L2_BooleanConstant(true)
    case L2_OrOr(expr : L2_Expression, L2_BooleanConstant(true))  => L2_BooleanConstant(true)
    case L2_OrOr(L2_BooleanConstant(false), expr : L2_Expression) => expr
    case L2_OrOr(expr : L2_Expression, L2_BooleanConstant(false)) => expr
  })

  private def simplifyAdd(sum : Seq[L2_Expression]) : L2_Expression = {
    var intCst : Long = 0L
    var floatCst : Double = 0d
    var vecPos : Boolean = true
    var matPos : Boolean = true
    val workQ = new Queue[(L2_Expression, Boolean)]()
    val posSums = new ListBuffer[L2_Expression]()
    val negSums = new ListBuffer[L2_Expression]()
    for (s <- sum) {
      workQ.enqueue((s, true)) // for nested Additions; this allows in-order processing
      do {
        val (expr, pos) = workQ.dequeue()
        expr match {
          case L2_IntegerConstant(i)       => if (pos) intCst += i else intCst -= i
          case L2_RealConstant(f)          => if (pos) floatCst += f else floatCst -= f
          case L2_Addition(sums)           => workQ.enqueue(sums.view.map { x => (x, pos) } : _*)
          case L2_Negative(e)              => workQ.enqueue((e, !pos))
          case L2_Subtraction(left, right) =>
            workQ.enqueue((left, pos))
            workQ.enqueue((right, !pos))
          // if some more simplifications with vectors or matrices are requl2ed, a similar approach than for a
          //    Multiplication is possible here
          case e : L2_Expression =>
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
        posSums += L2_RealConstant(cst)
      else
        negSums += L2_RealConstant(-cst)
    } else if (intCst != 0L)
    // if compactAST is set, no Subtraction is created, so prevent creating a Neg(Const),
    //   which would lead to a non-terminating recursion
    // if posSums is empty we do not want to add the constant to the negSums, which would also result in a Neg(Const) -> non-terminating
    if (intCst > 0 || compactAST || posSums.isEmpty)
      posSums += L2_IntegerConstant(intCst)
    else
      negSums += L2_IntegerConstant(-intCst)

    if (posSums.length + negSums.length <= 1) { // result is only one summand (either a positive, or a negative, or 0)
      (posSums ++= negSums.transform(x => L2_Negative(x)) += L2_IntegerConstant(0L)).head

    } else if (posSums.length * negSums.length == 0 || compactAST) { // if compactAST is set do not create any Subtraction
      L2_Addition(posSums ++= negSums.transform(x => L2_Negative(x)))

    } else {
      val posExpr = if (posSums.length == 1) posSums.head else new L2_Addition(posSums)
      val negExpr = if (negSums.length == 1) negSums.head else new L2_Addition(negSums)
      L2_Subtraction(posExpr, negExpr)
    }
  }

  private def simplifyMult(facs : Seq[L2_Expression]) : L2_Expression = {
    var intCst : Long = 1L
    var floatCst : Double = 1d
    var workQ = mutable.Queue[L2_Expression]()
    val remA = new ArrayBuffer[L2_Expression]() // use ArrayBuffer here for a more efficient access to the last element
    var div : L2_Division = null
    for (f <- facs) {
      workQ.enqueue(f) // for nested Multiplication; this allows in-order processing
      do {
        val expr = workQ.dequeue()
        expr match {
          case L2_IntegerConstant(iv)                  => intCst *= iv
          case L2_RealConstant(fv)                     => floatCst *= fv
          case L2_Negative(e)                          =>
            workQ = (mutable.Queue() :+ e) ++ workQ
            intCst = -intCst
          case L2_Multiplication(iFacs)                =>
            workQ = mutable.Queue() ++ iFacs ++ workQ
          case d @ L2_Division(L2_RealConstant(fv), _) =>
            floatCst *= fv
            d.left = L2_RealConstant(1.0)
            if (div == null)
              div = d
            remA += d
          case r : L2_Expression                       =>
            remA += r
        }
      } while (workQ.nonEmpty)
    }
    val rem = remA.to[ListBuffer]
    var cstDt : Option[L2_Datatype] = None
    val negative : Boolean = floatCst * intCst < 0d
    floatCst = math.abs(floatCst)
    intCst = math.abs(intCst)
    if (floatCst * intCst == 0d) {
      rem.clear()
      rem += L2_IntegerConstant(0L) // TODO: fix type
    } else if (div != null) {
      div.left = L2_RealConstant(floatCst * intCst)
    } else if (floatCst != 1d) {
      L2_RealConstant(floatCst * intCst) +=: rem // add constant at fl2st position (it is expected as rem.head later)
      cstDt = Some(L2_RealDatatype)
    } else if (intCst != 1L) {
      L2_IntegerConstant(intCst) +=: rem // add constant at fl2st position (it is expected as rem.head later)
      cstDt = Some(L2_IntegerDatatype)
    }

    var result : L2_Expression = null
    if (rem.isEmpty) {
      result = L2_IntegerConstant(1L) // TODO: fix type

    } else if (rem.length == 1 || floatCst * intCst == 0d) {
      result = rem.head

    } else {
      result = L2_Multiplication(rem)
    }

    if (negative)
      result = L2_Negative(result)
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
