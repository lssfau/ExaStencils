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

package exastencils.optimization.l1

import scala.collection.mutable
import scala.collection.mutable.{ ArrayBuffer, ListBuffer, Queue }

import exastencils.base.l1._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger

/// L1_GeneralSimplify

object L1_GeneralSimplify extends DefaultStrategy("Simplify general expressions") {
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
    case add : L1_Addition =>
      val nju = simplifyAdd(add.summands)
      if (nju == add)
        negMatches += 1
      nju

    case sub : L1_Subtraction =>
      val nju = simplifyAdd(List(sub))
      if (nju == sub)
        negMatches += 1
      nju

    case mult : L1_Multiplication =>
      val nju = simplifyMult(mult.factors)
      if (nju == mult)
        negMatches += 1
      nju

    case old @ L1_Negative(L1_Multiplication(facs)) =>
      val nju = simplifyMult(facs.clone() += L1_IntegerConstant(-1L))
      if (nju == old)
        negMatches += 1
      nju

    // deal with constants
    case L1_Negative(L1_IntegerConstant(value)) => L1_IntegerConstant(-value)
    case L1_Negative(L1_RealConstant(value))    => L1_RealConstant(-value)

    case L1_Division(L1_IntegerConstant(left), L1_IntegerConstant(right)) => L1_IntegerConstant(left / right)
    case L1_Division(L1_IntegerConstant(left), L1_RealConstant(right))    => L1_RealConstant(left / right)
    case L1_Division(L1_RealConstant(left), L1_IntegerConstant(right))    => L1_RealConstant(left / right)
    case L1_Division(L1_RealConstant(left), L1_RealConstant(right))       => L1_RealConstant(left / right)

    case L1_Division(left : L1_Expression, L1_IntegerConstant(1))  => left
    case L1_Division(left : L1_Expression, L1_RealConstant(f))     => L1_Multiplication(left, L1_RealConstant(1.0 / f))
    case L1_Division(L1_RealConstant(0.0), right : L1_Expression)  => L1_RealConstant(0.0)
    case L1_Division(L1_IntegerConstant(0), right : L1_Expression) => L1_IntegerConstant(0)

    case L1_Modulo(L1_IntegerConstant(left), L1_IntegerConstant(right)) => L1_IntegerConstant(left % right)

    case L1_Power(L1_IntegerConstant(base), L1_IntegerConstant(exp)) => L1_IntegerConstant(pow(base, exp))
    case L1_Power(L1_RealConstant(base), L1_IntegerConstant(exp))    => L1_RealConstant(pow(base, exp))
    case L1_Power(L1_IntegerConstant(base), L1_RealConstant(exp))    => L1_RealConstant(math.pow(base, exp))
    case L1_Power(L1_RealConstant(base), L1_RealConstant(exp))       => L1_RealConstant(math.pow(base, exp))

    case L1_Power(base, L1_IntegerConstant(0))                     => L1_IntegerConstant(1)
    case L1_Power(base, L1_IntegerConstant(1))                     => base
    case L1_Power(base, L1_IntegerConstant(e)) if e >= 2 && e <= 6 => L1_Multiplication(ListBuffer.fill(e.toInt)(Duplicate(base)))
    case L1_Power(b, L1_RealConstant(e)) if e.toLong.toDouble == e => L1_Power(b, L1_IntegerConstant(e.toLong))

    // deal with negatives
    case L1_Negative(L1_Negative(expr))           => expr
    case L1_Negative(L1_Addition(sums))           => L1_Addition(sums.transform { s => L1_Negative(s) })
    case L1_Negative(L1_Subtraction(left, right)) => L1_Subtraction(right, left)

    case L1_Division(L1_Negative(l), L1_Negative(r)) => L1_Division(l, r)
    case L1_Division(l, L1_Negative(r))              => L1_Negative(L1_Division(l, r))
    case L1_Division(L1_Negative(l), r)              => L1_Negative(L1_Division(l, r))

    case L1_Negative(L1_Maximum(exps)) => L1_Minimum(exps.transform { s => L1_Negative(s) })
    case L1_Negative(L1_Minimum(exps)) => L1_Maximum(exps.transform { s => L1_Negative(s) })

    // Simplify vectors

    case L1_EqEq(L1_IntegerConstant(left), L1_IntegerConstant(right))         => L1_BooleanConstant(left == right)
    case L1_Neq(L1_IntegerConstant(left), L1_IntegerConstant(right))          => L1_BooleanConstant(left != right)
    case L1_Lower(L1_IntegerConstant(left), L1_IntegerConstant(right))        => L1_BooleanConstant(left < right)
    case L1_LowerEqual(L1_IntegerConstant(left), L1_IntegerConstant(right))   => L1_BooleanConstant(left <= right)
    case L1_Greater(L1_IntegerConstant(left), L1_IntegerConstant(right))      => L1_BooleanConstant(left > right)
    case L1_GreaterEqual(L1_IntegerConstant(left), L1_IntegerConstant(right)) => L1_BooleanConstant(left >= right)

    case L1_Negation(L1_BooleanConstant(b)) => L1_BooleanConstant(!b)

    case L1_Negation(L1_EqEq(left, right)) => L1_Neq(left, right)
    case L1_Negation(L1_Neq(left, right))  => L1_EqEq(left, right)

    case L1_Negation(L1_Lower(left, right))        => L1_GreaterEqual(left, right)
    case L1_Negation(L1_GreaterEqual(left, right)) => L1_Lower(left, right)
    case L1_Negation(L1_LowerEqual(left, right))   => L1_Greater(left, right)
    case L1_Negation(L1_Greater(left, right))      => L1_LowerEqual(left, right)

    case L1_Negation(L1_AndAnd(left, right)) => L1_OrOr(L1_Negation(left), L1_Negation(right))
    case L1_Negation(L1_OrOr(left, right))   => L1_AndAnd(L1_Negation(left), L1_Negation(right))

    case L1_AndAnd(L1_BooleanConstant(true), expr : L1_Expression)  => expr
    case L1_AndAnd(expr : L1_Expression, L1_BooleanConstant(true))  => expr
    case L1_AndAnd(L1_BooleanConstant(false), expr : L1_Expression) => L1_BooleanConstant(false)
    case L1_AndAnd(expr : L1_Expression, L1_BooleanConstant(false)) => L1_BooleanConstant(false)

    case L1_OrOr(L1_BooleanConstant(true), expr : L1_Expression)  => L1_BooleanConstant(true)
    case L1_OrOr(expr : L1_Expression, L1_BooleanConstant(true))  => L1_BooleanConstant(true)
    case L1_OrOr(L1_BooleanConstant(false), expr : L1_Expression) => expr
    case L1_OrOr(expr : L1_Expression, L1_BooleanConstant(false)) => expr
  })

  private def simplifyAdd(sum : Seq[L1_Expression]) : L1_Expression = {
    var intCst : Long = 0L
    var floatCst : Double = 0d
    var vecPos : Boolean = true
    var matPos : Boolean = true
    val workQ = new Queue[(L1_Expression, Boolean)]()
    val posSums = new ListBuffer[L1_Expression]()
    val negSums = new ListBuffer[L1_Expression]()
    for (s <- sum) {
      workQ.enqueue((s, true)) // for nested Additions; this allows in-order processing
      do {
        val (expr, pos) = workQ.dequeue()
        expr match {
          case L1_IntegerConstant(i)       => if (pos) intCst += i else intCst -= i
          case L1_RealConstant(f)          => if (pos) floatCst += f else floatCst -= f
          case L1_Addition(sums)           => workQ.enqueue(sums.view.map { x => (x, pos) } : _*)
          case L1_Negative(e)              => workQ.enqueue((e, !pos))
          case L1_Subtraction(left, right) =>
            workQ.enqueue((left, pos))
            workQ.enqueue((right, !pos))
          // if some more simplifications with vectors or matrices are requl2ed, a similar approach than for a
          //    Multiplication is possible here
          case e : L1_Expression =>
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
        posSums += L1_RealConstant(cst)
      else
        negSums += L1_RealConstant(-cst)
    } else if (intCst != 0L)
    // if compactAST is set, no Subtraction is created, so prevent creating a Neg(Const),
    //   which would lead to a non-terminating recursion
    // if posSums is empty we do not want to add the constant to the negSums, which would also result in a Neg(Const) -> non-terminating
    if (intCst > 0 || compactAST || posSums.isEmpty)
      posSums += L1_IntegerConstant(intCst)
    else
      negSums += L1_IntegerConstant(-intCst)

    if (posSums.length + negSums.length <= 1) { // result is only one summand (either a positive, or a negative, or 0)
      (posSums ++= negSums.transform(x => L1_Negative(x)) += L1_IntegerConstant(0L)).head

    } else if (posSums.length * negSums.length == 0 || compactAST) { // if compactAST is set do not create any Subtraction
      L1_Addition(posSums ++= negSums.transform(x => L1_Negative(x)))

    } else {
      val posExpr = if (posSums.length == 1) posSums.head else new L1_Addition(posSums)
      val negExpr = if (negSums.length == 1) negSums.head else new L1_Addition(negSums)
      L1_Subtraction(posExpr, negExpr)
    }
  }

  private def simplifyMult(facs : Seq[L1_Expression]) : L1_Expression = {
    var intCst : Long = 1L
    var floatCst : Double = 1d
    var workQ = mutable.Queue[L1_Expression]()
    val remA = new ArrayBuffer[L1_Expression]() // use ArrayBuffer here for a more efficient access to the last element
    var div : L1_Division = null
    for (f <- facs) {
      workQ.enqueue(f) // for nested Multiplication; this allows in-order processing
      do {
        val expr = workQ.dequeue()
        expr match {
          case L1_IntegerConstant(iv)                  => intCst *= iv
          case L1_RealConstant(fv)                     => floatCst *= fv
          case L1_Negative(e)                          =>
            workQ = (mutable.Queue() :+ e) ++ workQ
            intCst = -intCst
          case L1_Multiplication(iFacs)                =>
            workQ = mutable.Queue() ++ iFacs ++ workQ
          case d @ L1_Division(L1_RealConstant(fv), _) =>
            floatCst *= fv
            d.left = L1_RealConstant(1.0)
            if (div == null)
              div = d
            remA += d
          case r : L1_Expression                       =>
            remA += r
        }
      } while (workQ.nonEmpty)
    }
    val rem = remA.to[ListBuffer]
    var cstDt : Option[L1_Datatype] = None
    val negative : Boolean = floatCst * intCst < 0d
    floatCst = math.abs(floatCst)
    intCst = math.abs(intCst)
    if (floatCst * intCst == 0d) {
      rem.clear()
      rem += L1_IntegerConstant(0L) // TODO: fix type
    } else if (div != null) {
      div.left = L1_RealConstant(floatCst * intCst)
    } else if (floatCst != 1d) {
      L1_RealConstant(floatCst * intCst) +=: rem // add constant at fl2st position (it is expected as rem.head later)
      cstDt = Some(L1_RealDatatype)
    } else if (intCst != 1L) {
      L1_IntegerConstant(intCst) +=: rem // add constant at fl2st position (it is expected as rem.head later)
      cstDt = Some(L1_IntegerDatatype)
    }

    var result : L1_Expression = null
    if (rem.isEmpty) {
      result = L1_IntegerConstant(1L) // TODO: fix type

    } else if (rem.length == 1 || floatCst * intCst == 0d) {
      result = rem.head

    } else {
      result = L1_Multiplication(rem)
    }

    if (negative)
      result = L1_Negative(result)
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
