package exastencils.optimization.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_GeneralSimplify extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/optimization/|LAYER_LC|/|LAYER_UC|_GeneralSimplify.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.optimization.|LAYER_LC|

import scala.collection.mutable.{ ArrayBuffer, ListBuffer, Queue }

import exastencils.base.|LAYER_LC|._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger

/// |LAYER_UC|_GeneralSimplify

object |LAYER_UC|_GeneralSimplify extends DefaultStrategy("Simplify general expressions") {
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
    case add : |LAYER_UC|_Addition =>
      val nju = simplifyAdd(add.summands)
      if (nju == add)
        negMatches += 1
      nju

    case sub : |LAYER_UC|_Subtraction =>
      val nju = simplifyAdd(List(sub))
      if (nju == sub)
        negMatches += 1
      nju

    case mult : |LAYER_UC|_Multiplication =>
      val nju = simplifyMult(mult.factors)
      if (nju == mult)
        negMatches += 1
      nju

    case old @ |LAYER_UC|_Negative(|LAYER_UC|_Multiplication(facs)) =>
      val nju = simplifyMult(facs.clone() += |LAYER_UC|_IntegerConstant(-1L))
      if (nju == old)
        negMatches += 1
      nju

    // deal with constants
    case |LAYER_UC|_Negative(|LAYER_UC|_IntegerConstant(value)) => |LAYER_UC|_IntegerConstant(-value)
    case |LAYER_UC|_Negative(|LAYER_UC|_RealConstant(value))    => |LAYER_UC|_RealConstant(-value)

    case |LAYER_UC|_Division(|LAYER_UC|_IntegerConstant(left), |LAYER_UC|_IntegerConstant(right)) => |LAYER_UC|_IntegerConstant(left / right)
    case |LAYER_UC|_Division(|LAYER_UC|_IntegerConstant(left), |LAYER_UC|_RealConstant(right))    => |LAYER_UC|_RealConstant(left / right)
    case |LAYER_UC|_Division(|LAYER_UC|_RealConstant(left), |LAYER_UC|_IntegerConstant(right))    => |LAYER_UC|_RealConstant(left / right)
    case |LAYER_UC|_Division(|LAYER_UC|_RealConstant(left), |LAYER_UC|_RealConstant(right))       => |LAYER_UC|_RealConstant(left / right)

    case |LAYER_UC|_Division(left : |LAYER_UC|_Expression, |LAYER_UC|_IntegerConstant(1))  => left
    case |LAYER_UC|_Division(left : |LAYER_UC|_Expression, |LAYER_UC|_RealConstant(f))     => |LAYER_UC|_Multiplication(left, |LAYER_UC|_RealConstant(1.0 / f))
    case |LAYER_UC|_Division(|LAYER_UC|_RealConstant(0.0), right : |LAYER_UC|_Expression)  => |LAYER_UC|_RealConstant(0.0)
    case |LAYER_UC|_Division(|LAYER_UC|_IntegerConstant(0), right : |LAYER_UC|_Expression) => |LAYER_UC|_IntegerConstant(0)

    case |LAYER_UC|_Modulo(|LAYER_UC|_IntegerConstant(left), |LAYER_UC|_IntegerConstant(right)) => |LAYER_UC|_IntegerConstant(left % right)

    case |LAYER_UC|_Power(|LAYER_UC|_IntegerConstant(base), |LAYER_UC|_IntegerConstant(exp)) => |LAYER_UC|_IntegerConstant(pow(base, exp))
    case |LAYER_UC|_Power(|LAYER_UC|_RealConstant(base), |LAYER_UC|_IntegerConstant(exp))    => |LAYER_UC|_RealConstant(pow(base, exp))
    case |LAYER_UC|_Power(|LAYER_UC|_IntegerConstant(base), |LAYER_UC|_RealConstant(exp))    => |LAYER_UC|_RealConstant(math.pow(base, exp))
    case |LAYER_UC|_Power(|LAYER_UC|_RealConstant(base), |LAYER_UC|_RealConstant(exp))       => |LAYER_UC|_RealConstant(math.pow(base, exp))

    case |LAYER_UC|_Power(base, |LAYER_UC|_IntegerConstant(0))                     => |LAYER_UC|_IntegerConstant(1)
    case |LAYER_UC|_Power(base, |LAYER_UC|_IntegerConstant(1))                     => base
    case |LAYER_UC|_Power(base, |LAYER_UC|_IntegerConstant(e)) if e >= 2 && e <= 6 => |LAYER_UC|_Multiplication(ListBuffer.fill(e.toInt)(Duplicate(base)))
    case |LAYER_UC|_Power(b, |LAYER_UC|_RealConstant(e)) if e.toLong.toDouble == e => |LAYER_UC|_Power(b, |LAYER_UC|_IntegerConstant(e.toLong))

    // deal with negatives
    case |LAYER_UC|_Negative(|LAYER_UC|_Negative(expr))           => expr
    case |LAYER_UC|_Negative(|LAYER_UC|_Addition(sums))           => |LAYER_UC|_Addition(sums.transform { s => |LAYER_UC|_Negative(s) })
    case |LAYER_UC|_Negative(|LAYER_UC|_Subtraction(left, right)) => |LAYER_UC|_Subtraction(right, left)

    case |LAYER_UC|_Division(|LAYER_UC|_Negative(l), |LAYER_UC|_Negative(r)) => |LAYER_UC|_Division(l, r)
    case |LAYER_UC|_Division(l, |LAYER_UC|_Negative(r))              => |LAYER_UC|_Negative(|LAYER_UC|_Division(l, r))
    case |LAYER_UC|_Division(|LAYER_UC|_Negative(l), r)              => |LAYER_UC|_Negative(|LAYER_UC|_Division(l, r))

    case |LAYER_UC|_Negative(|LAYER_UC|_Maximum(exps)) => |LAYER_UC|_Minimum(exps.transform { s => |LAYER_UC|_Negative(s) })
    case |LAYER_UC|_Negative(|LAYER_UC|_Minimum(exps)) => |LAYER_UC|_Maximum(exps.transform { s => |LAYER_UC|_Negative(s) })

    // Simplify vectors

    case |LAYER_UC|_EqEq(|LAYER_UC|_IntegerConstant(left), |LAYER_UC|_IntegerConstant(right))         => |LAYER_UC|_BooleanConstant(left == right)
    case |LAYER_UC|_Neq(|LAYER_UC|_IntegerConstant(left), |LAYER_UC|_IntegerConstant(right))          => |LAYER_UC|_BooleanConstant(left != right)
    case |LAYER_UC|_Lower(|LAYER_UC|_IntegerConstant(left), |LAYER_UC|_IntegerConstant(right))        => |LAYER_UC|_BooleanConstant(left < right)
    case |LAYER_UC|_LowerEqual(|LAYER_UC|_IntegerConstant(left), |LAYER_UC|_IntegerConstant(right))   => |LAYER_UC|_BooleanConstant(left <= right)
    case |LAYER_UC|_Greater(|LAYER_UC|_IntegerConstant(left), |LAYER_UC|_IntegerConstant(right))      => |LAYER_UC|_BooleanConstant(left > right)
    case |LAYER_UC|_GreaterEqual(|LAYER_UC|_IntegerConstant(left), |LAYER_UC|_IntegerConstant(right)) => |LAYER_UC|_BooleanConstant(left >= right)

    case |LAYER_UC|_Negation(|LAYER_UC|_BooleanConstant(b)) => |LAYER_UC|_BooleanConstant(!b)

    case |LAYER_UC|_Negation(|LAYER_UC|_EqEq(left, right)) => |LAYER_UC|_Neq(left, right)
    case |LAYER_UC|_Negation(|LAYER_UC|_Neq(left, right))  => |LAYER_UC|_EqEq(left, right)

    case |LAYER_UC|_Negation(|LAYER_UC|_Lower(left, right))        => |LAYER_UC|_GreaterEqual(left, right)
    case |LAYER_UC|_Negation(|LAYER_UC|_GreaterEqual(left, right)) => |LAYER_UC|_Lower(left, right)
    case |LAYER_UC|_Negation(|LAYER_UC|_LowerEqual(left, right))   => |LAYER_UC|_Greater(left, right)
    case |LAYER_UC|_Negation(|LAYER_UC|_Greater(left, right))      => |LAYER_UC|_LowerEqual(left, right)

    case |LAYER_UC|_Negation(|LAYER_UC|_AndAnd(left, right)) => |LAYER_UC|_OrOr(|LAYER_UC|_Negation(left), |LAYER_UC|_Negation(right))
    case |LAYER_UC|_Negation(|LAYER_UC|_OrOr(left, right))   => |LAYER_UC|_AndAnd(|LAYER_UC|_Negation(left), |LAYER_UC|_Negation(right))

    case |LAYER_UC|_AndAnd(|LAYER_UC|_BooleanConstant(true), expr : |LAYER_UC|_Expression)  => expr
    case |LAYER_UC|_AndAnd(expr : |LAYER_UC|_Expression, |LAYER_UC|_BooleanConstant(true))  => expr
    case |LAYER_UC|_AndAnd(|LAYER_UC|_BooleanConstant(false), expr : |LAYER_UC|_Expression) => |LAYER_UC|_BooleanConstant(false)
    case |LAYER_UC|_AndAnd(expr : |LAYER_UC|_Expression, |LAYER_UC|_BooleanConstant(false)) => |LAYER_UC|_BooleanConstant(false)

    case |LAYER_UC|_OrOr(|LAYER_UC|_BooleanConstant(true), expr : |LAYER_UC|_Expression)  => |LAYER_UC|_BooleanConstant(true)
    case |LAYER_UC|_OrOr(expr : |LAYER_UC|_Expression, |LAYER_UC|_BooleanConstant(true))  => |LAYER_UC|_BooleanConstant(true)
    case |LAYER_UC|_OrOr(|LAYER_UC|_BooleanConstant(false), expr : |LAYER_UC|_Expression) => expr
    case |LAYER_UC|_OrOr(expr : |LAYER_UC|_Expression, |LAYER_UC|_BooleanConstant(false)) => expr
  })

  private def simplifyAdd(sum : Seq[|LAYER_UC|_Expression]) : |LAYER_UC|_Expression = {
    var intCst : Long = 0L
    var floatCst : Double = 0d
    var vecPos : Boolean = true
    var matPos : Boolean = true
    val workQ = new Queue[(|LAYER_UC|_Expression, Boolean)]()
    val posSums = new ListBuffer[|LAYER_UC|_Expression]()
    val negSums = new ListBuffer[|LAYER_UC|_Expression]()
    for (s <- sum) {
      workQ.enqueue((s, true)) // for nested Additions; this allows in-order processing
      do {
        val (expr, pos) = workQ.dequeue()
        expr match {
          case |LAYER_UC|_IntegerConstant(i)       => if (pos) intCst += i else intCst -= i
          case |LAYER_UC|_RealConstant(f)          => if (pos) floatCst += f else floatCst -= f
          case |LAYER_UC|_Addition(sums)           => workQ.enqueue(sums.view.map { x => (x, pos) } : _*)
          case |LAYER_UC|_Negative(e)              => workQ.enqueue((e, !pos))
          case |LAYER_UC|_Subtraction(left, right) =>
            workQ.enqueue((left, pos))
            workQ.enqueue((right, !pos))
          // if some more simplifications with vectors or matrices are requl2ed, a similar approach than for a
          //    Multiplication is possible here
          case e : |LAYER_UC|_Expression =>
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
        posSums += |LAYER_UC|_RealConstant(cst)
      else
        negSums += |LAYER_UC|_RealConstant(-cst)
    } else if (intCst != 0L)
    // if compactAST is set, no Subtraction is created, so prevent creating a Neg(Const),
    //   which would lead to a non-terminating recursion
    // if posSums is empty we do not want to add the constant to the negSums, which would also result in a Neg(Const) -> non-terminating
      if (intCst > 0 || compactAST || posSums.isEmpty)
        posSums += |LAYER_UC|_IntegerConstant(intCst)
      else
        negSums += |LAYER_UC|_IntegerConstant(-intCst)

    if (posSums.length + negSums.length <= 1) { // result is only one summand (either a positive, or a negative, or 0)
      (posSums ++= negSums.transform(x => |LAYER_UC|_Negative(x)) += |LAYER_UC|_IntegerConstant(0L)).head

    } else if (posSums.length * negSums.length == 0 || compactAST) { // if compactAST is set do not create any Subtraction
      |LAYER_UC|_Addition(posSums ++= negSums.transform(x => |LAYER_UC|_Negative(x)))

    } else {
      val posExpr = if (posSums.length == 1) posSums.head else new |LAYER_UC|_Addition(posSums)
      val negExpr = if (negSums.length == 1) negSums.head else new |LAYER_UC|_Addition(negSums)
      |LAYER_UC|_Subtraction(posExpr, negExpr)
    }
  }

  private def simplifyMult(facs : Seq[|LAYER_UC|_Expression]) : |LAYER_UC|_Expression = {
    var intCst : Long = 1L
    var floatCst : Double = 1d
    val workQ = new Queue[|LAYER_UC|_Expression]()
    val remA = new ArrayBuffer[|LAYER_UC|_Expression]() // use ArrayBuffer here for a more efficient access to the last element
    var div : |LAYER_UC|_Division = null
    for (f <- facs) {
      workQ.enqueue(f) // for nested Multiplication; this allows in-order processing
      do {
        val expr = workQ.dequeue()
        expr match {
          case |LAYER_UC|_IntegerConstant(iv)                  => intCst *= iv
          case |LAYER_UC|_RealConstant(fv)                     => floatCst *= fv
          case |LAYER_UC|_Negative(e)                          =>
            workQ.enqueue(e)
            intCst = -intCst
          case |LAYER_UC|_Multiplication(iFacs)                =>
            workQ.enqueue(iFacs : _*)
          case d @ |LAYER_UC|_Division(|LAYER_UC|_RealConstant(fv), _) =>
            floatCst *= fv
            d.left = |LAYER_UC|_RealConstant(1.0)
            if (div == null)
              div = d
            remA += d
          case r : |LAYER_UC|_Expression                       =>
            remA += r
        }
      } while (workQ.nonEmpty)
    }
    val rem = remA.to[ListBuffer]
    var cstDt : Option[|LAYER_UC|_Datatype] = None
    val negative : Boolean = floatCst * intCst < 0d
    floatCst = math.abs(floatCst)
    intCst = math.abs(intCst)
    if (floatCst * intCst == 0d) {
      rem.clear()
      rem += |LAYER_UC|_IntegerConstant(0L) // TODO: fix type
    } else if (div != null) {
      div.left = |LAYER_UC|_RealConstant(floatCst * intCst)
    } else if (floatCst != 1d) {
      |LAYER_UC|_RealConstant(floatCst * intCst) +=: rem // add constant at fl2st position (it is expected as rem.head later)
      cstDt = Some(|LAYER_UC|_RealDatatype)
    } else if (intCst != 1L) {
      |LAYER_UC|_IntegerConstant(intCst) +=: rem // add constant at fl2st position (it is expected as rem.head later)
      cstDt = Some(|LAYER_UC|_IntegerDatatype)
    }

    var result : |LAYER_UC|_Expression = null
    if (rem.isEmpty) {
      result = |LAYER_UC|_IntegerConstant(1L) // TODO: fix type

    } else if (rem.length == 1 || floatCst * intCst == 0d) {
      result = rem.head

    } else {
      result = |LAYER_UC|_Multiplication(rem)
    }

    if (negative)
      result = |LAYER_UC|_Negative(result)
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
"""
  }
}
