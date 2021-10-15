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

package exastencils.optimization.ir

import scala.collection.mutable
import scala.collection.mutable.{ ArrayBuffer, ListBuffer, Queue }

import java.util.concurrent.atomic.AtomicInteger

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.core._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.util.ir.IR_MathFunctionReference
import exastencils.util.ir.IR_MathFunctions
import exastencils.util.ir.IR_ResultingDatatype

/// IR_GeneralSimplify

object IR_GeneralSimplify extends DefaultStrategy("Simplify general expressions") {
  // hack: since Addition and Multiplication lead always to a match, we don't count these if nothing was changed
  var negMatches = new AtomicInteger(0)
  var compactAST : Boolean = false

  def doUntilDone(node : Option[Node] = None) = {
    do {
      negMatches.set(0)
      apply(node)
    } while (results.last._2.matches - negMatches.get() > 0) // FIXME: cleaner code
  }

  def doUntilDoneStandalone(node : Node, compactAST : Boolean = false) = {
    this.compactAST = compactAST
    val oldLvl = Logger.getLevel
    Logger.setLevel(Logger.WARNING)
    do {
      negMatches.set(0)
      applyStandalone(node)
    } while (results.last._2.matches - negMatches.get() > 0) // FIXME: cleaner code
    Logger.setLevel(oldLvl)
    this.compactAST = false
  }

  this += new Transformation("Simplify", {
    case add : IR_Addition =>
      val nju = simplifyAdd(add.summands)
      if (nju == add)
        negMatches.incrementAndGet()
      nju

    case sub : IR_Subtraction =>
      val nju = simplifyAdd(List(sub))
      if (nju == sub)
        negMatches.incrementAndGet()
      nju

    case mult @ IR_Multiplication(facs) =>
      val nju = simplifyMult(facs, false)
      if (nju == mult)
        negMatches.incrementAndGet()
      nju

    case old @ IR_Negative(IR_Multiplication(facs)) =>
      val nju = simplifyMult(facs, true)
      if (nju == old)
        negMatches.incrementAndGet()
      nju

    // deal with constants
    case IR_Negative(IR_IntegerConstant(value)) => IR_IntegerConstant(-value)
    case IR_Negative(IR_RealConstant(value))    => IR_RealConstant(-value)

    case IR_Division(IR_IntegerConstant(left), IR_IntegerConstant(right)) => IR_IntegerConstant(left / right)
    case IR_Division(IR_IntegerConstant(left), IR_RealConstant(right))    => IR_RealConstant(left / right)
    case IR_Division(IR_RealConstant(left), IR_IntegerConstant(right))    => IR_RealConstant(left / right)
    case IR_Division(IR_RealConstant(left), IR_RealConstant(right))       => IR_RealConstant(left / right)

    case IR_Division(left : IR_Expression, IR_IntegerConstant(1))  => left
    case IR_Division(left : IR_Expression, IR_RealConstant(f))     => IR_Multiplication(left, IR_RealConstant(1.0 / f))
    case IR_Division(IR_RealConstant(0.0), right : IR_Expression)  => IR_RealConstant(0.0)
    case IR_Division(IR_IntegerConstant(0), right : IR_Expression) => IR_IntegerConstant(0)

    case IR_Modulo(IR_IntegerConstant(left), IR_IntegerConstant(right)) => IR_IntegerConstant(left % right)

    case IR_Power(IR_IntegerConstant(base), IR_IntegerConstant(exp)) => IR_IntegerConstant(pow(base, exp))
    case IR_Power(IR_RealConstant(base), IR_IntegerConstant(exp))    => IR_RealConstant(pow(base, exp))
    case IR_Power(IR_IntegerConstant(base), IR_RealConstant(exp))    => IR_RealConstant(math.pow(base, exp))
    case IR_Power(IR_RealConstant(base), IR_RealConstant(exp))       => IR_RealConstant(math.pow(base, exp))

    case IR_Power(base, IR_IntegerConstant(0))                     => IR_IntegerConstant(1)
    case IR_Power(base, IR_IntegerConstant(1))                     => base
    case IR_Power(base, IR_IntegerConstant(e)) if e >= 2 && e <= 6 => IR_Multiplication(ListBuffer.fill(e.toInt)(Duplicate(base)))
    case IR_Power(b, IR_RealConstant(e)) if e.toLong.toDouble == e => IR_Power(b, IR_IntegerConstant(e.toLong))

    // deal with negatives
    case IR_Negative(IR_Negative(expr))           => expr
    case IR_Negative(IR_Addition(sums))           => IR_Addition(sums.transform { s => IR_Negative(s) })
    case IR_Negative(IR_Subtraction(left, right)) => IR_Subtraction(right, left)

    case IR_Division(IR_Negative(l), IR_Negative(r)) => IR_Division(l, r)
    case IR_Division(l, IR_Negative(r))              => IR_Negative(IR_Division(l, r))
    case IR_Division(IR_Negative(l), r)              => IR_Negative(IR_Division(l, r))

    case IR_Negative(IR_Maximum(exps)) => IR_Minimum(exps.transform { s => IR_Negative(s) })
    case IR_Negative(IR_Minimum(exps)) => IR_Maximum(exps.transform { s => IR_Negative(s) })

    // Simplify vectors
    case IR_Negative(v : IR_VectorExpression) =>
      IR_VectorExpression(v.innerDatatype, v.expressions.map { x => IR_Negative(x) }, v.rowVector)

    // Simplify matrices
    case IR_Negative(m : IR_MatrixExpression) => m.expressions = m.expressions.map { y => IR_Negative(y) : IR_Expression }; m

    case m @ IR_MatrixExpression(_, 1, 1,_) => m.get(0, 0)
    case m @ IR_MatrixDatatype(dt, 1, 1)  => dt

    case IR_Scope(body) if body.forall(_.isInstanceOf[IR_ScopedStatement]) => body

    case IR_IfCondition(cond, ListBuffer(IR_Scope(trueBody)), falseBody)         => IR_IfCondition(cond, trueBody, falseBody)
    case IR_IfCondition(cond, trueBody, ListBuffer(IR_Scope(falseBody)))         => IR_IfCondition(cond, trueBody, falseBody)
    case l @ IR_ForLoop(beg, end, inc, ListBuffer(IR_Scope(body)), red)          =>
      l.body = body; l // preserve original node instance to ensure all traits and annotations are still present
    case l @ IR_LoopOverDimensions(_, _, ListBuffer(IR_Scope(body)), _, _, _, _) =>
      l.body = body; l // preserve original node instance to ensure all traits and annotations are still present

    // resolve compound assignments if lhs also occurs in rhs (to merge both)
    case ass @ IR_Assignment(dst, src, op) if List("+=", "-=", "*=", "/=").contains(op)
      && StateManager.findFirst({ n : IR_Expression => n == dst }, src).isDefined =>
      op match {
        case "+=" => ass.src = IR_Addition(Duplicate(dst), src)
        case "-=" => ass.src = IR_Subtraction(Duplicate(dst), src)
        case "*=" => ass.src = IR_Multiplication(Duplicate(dst), src)
        case "/=" => ass.src = IR_Division(Duplicate(dst), src)
      }
      ass.op = "="
      ass

    // simplify math functions applied to constant fp values
    case IR_FunctionCall(IR_MathFunctionReference(name, _), ListBuffer(arg1 : IR_RealConstant)) =>
      IR_RealConstant(IR_MathFunctions.evaluateMathFunction(name, arg1.value))
    case IR_FunctionCall(IR_MathFunctionReference(name, _), ListBuffer(arg1 : IR_RealConstant, arg2 : IR_RealConstant)) =>
      IR_RealConstant(IR_MathFunctions.evaluateMathFunction(name, arg1.value, arg2.value))

    // Simplify boolean expressions
    case IR_EqEq(IR_IntegerConstant(left), IR_IntegerConstant(right))         => IR_BooleanConstant(left == right)
    case IR_Neq(IR_IntegerConstant(left), IR_IntegerConstant(right))          => IR_BooleanConstant(left != right)
    case IR_Lower(IR_IntegerConstant(left), IR_IntegerConstant(right))        => IR_BooleanConstant(left < right)
    case IR_LowerEqual(IR_IntegerConstant(left), IR_IntegerConstant(right))   => IR_BooleanConstant(left <= right)
    case IR_Greater(IR_IntegerConstant(left), IR_IntegerConstant(right))      => IR_BooleanConstant(left > right)
    case IR_GreaterEqual(IR_IntegerConstant(left), IR_IntegerConstant(right)) => IR_BooleanConstant(left >= right)

    case IR_Negation(IR_BooleanConstant(b)) => IR_BooleanConstant(!b)

    case IR_Negation(IR_EqEq(left, right)) => IR_Neq(left, right)
    case IR_Negation(IR_Neq(left, right))  => IR_EqEq(left, right)

    case IR_Negation(IR_Lower(left, right))        => IR_GreaterEqual(left, right)
    case IR_Negation(IR_GreaterEqual(left, right)) => IR_Lower(left, right)
    case IR_Negation(IR_LowerEqual(left, right))   => IR_Greater(left, right)
    case IR_Negation(IR_Greater(left, right))      => IR_LowerEqual(left, right)

    case IR_Negation(IR_AndAnd(left, right)) => IR_OrOr(IR_Negation(left), IR_Negation(right))
    case IR_Negation(IR_OrOr(left, right))   => IR_AndAnd(IR_Negation(left), IR_Negation(right))

    case IR_AndAnd(IR_BooleanConstant(true), expr : IR_Expression)  => expr
    case IR_AndAnd(expr : IR_Expression, IR_BooleanConstant(true))  => expr
    case IR_AndAnd(IR_BooleanConstant(false), expr : IR_Expression) => IR_BooleanConstant(false)
    case IR_AndAnd(expr : IR_Expression, IR_BooleanConstant(false)) => IR_BooleanConstant(false)

    case IR_OrOr(IR_BooleanConstant(true), expr : IR_Expression)  => IR_BooleanConstant(true)
    case IR_OrOr(expr : IR_Expression, IR_BooleanConstant(true))  => IR_BooleanConstant(true)
    case IR_OrOr(IR_BooleanConstant(false), expr : IR_Expression) => expr
    case IR_OrOr(expr : IR_Expression, IR_BooleanConstant(false)) => expr

    case IR_IfCondition(IR_BooleanConstant(cond), tBranch, fBranch) =>
      if (cond) {
        if (tBranch.isEmpty) IR_NullStatement else tBranch
      } else {
        if (fBranch.isEmpty) IR_NullStatement else fBranch
      }

    case IR_IfCondition(IR_IntegerConstant(cond), tBranch, fBranch) if Knowledge.experimental_emliminateIntConditions =>
      if (cond != 0) {
        if (tBranch.isEmpty) IR_NullStatement else tBranch
      } else {
        if (fBranch.isEmpty) IR_NullStatement else fBranch
      }

    // simplify redundant operations
    case IR_FunctionCall(a : IR_FunctionReference, ListBuffer(IR_FunctionCall(b : IR_FunctionReference, l))) if (a.name == "inverse" && b.name == "inverse" && l.length == 1) => l

  }, isParallel = true)

  private def simplifyAdd(sum : Seq[IR_Expression]) : IR_Expression = {
    var intCst : Long = 0L
    var floatCst : Double = 0d
    var vecExpr : IR_VectorExpression = null
    var vecPos : Boolean = true
    var matExpr : IR_MatrixExpression = null
    var matPos : Boolean = true
    val workQ = new Queue[(IR_Expression, Boolean)]()
    val posSums = new ListBuffer[IR_Expression]()
    val negSums = new ListBuffer[IR_Expression]()
    for (s <- sum) {
      workQ.enqueue((s, true)) // for nested Additions; this allows in-order processing
      do {
        val (expr, pos) = workQ.dequeue()
        expr match {
          case IR_IntegerConstant(i)       => if (pos) intCst += i else intCst -= i
          case IR_RealConstant(f)          => if (pos) floatCst += f else floatCst -= f
          case IR_Addition(sums)           => workQ.enqueue(sums.view.map { x => (x, pos) } : _*)
          case IR_Negative(e)              => workQ.enqueue((e, !pos))
          case IR_Subtraction(left, right) =>
            workQ.enqueue((left, pos))
            workQ.enqueue((right, !pos))
          // if some more simplifications with vectors or matrices are required, a similar approach than for a
          //    Multiplication is possible here
          case v : IR_VectorExpression =>
            if (vecExpr == null) {
              vecPos = pos
              vecExpr = v
            } else {
              if (vecExpr.rowVector.getOrElse(true) != v.rowVector.getOrElse(true))
                Logger.error("Vector types must match for addition")
              if (vecExpr.length != v.length)
                Logger.error("Vector sizes must match for addition")
              val vecExprsView = if (vecPos) vecExpr.expressions.view else vecExpr.expressions.view.map { x => IR_Negative(x) }
              val vExprs = if (pos) v.expressions else v.expressions.view.map { x => IR_Negative(x) }
              vecExpr =
                IR_VectorExpression(
                  Some(IR_ResultingDatatype(vecExpr.datatype, v.innerDatatype.getOrElse(IR_RealDatatype))),
                  vecExprsView.zip(vExprs).map { x => x._1 + x._2 : IR_Expression }.to[ListBuffer],
                  if (vecExpr.rowVector.isDefined) vecExpr.rowVector else v.rowVector
                )
            }
          case m : IR_MatrixExpression =>
            if (matExpr == null) {
              matPos = pos
              matExpr = m
            } else {
              if (matExpr.rows != m.rows || matExpr.columns != m.columns) Logger.error("Matrix sizes must match for addition")
              val matExprsView = if (matPos) matExpr.expressions.view else matExpr.expressions.view.map { x => IR_Negative(x) }
              val mExprs = if (pos) m.expressions.view else m.expressions.view.map { x => IR_Negative(x) }
              matExpr = IR_MatrixExpression(Some(IR_ResultingDatatype(matExpr.innerDatatype.getOrElse(IR_RealDatatype), m.innerDatatype.getOrElse(IR_RealDatatype))), m.rows, m.columns, matExprsView.zip(mExprs).map { x => x._1 + x._2 : IR_Expression }.to[Array])
              matPos = true
            }
          case e : IR_Expression       =>
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
        posSums += IR_RealConstant(cst)
      else
        negSums += IR_RealConstant(-cst)
    } else if (intCst != 0L)
    // if compactAST is set, no Subtraction is created, so prevent creating a Neg(Const),
    //   which would lead to a non-terminating recursion
    // if posSums is empty we do not want to add the constant to the negSums, which would also result in a Neg(Const) -> non-terminating
    if (intCst > 0 || compactAST || posSums.isEmpty)
      posSums += IR_IntegerConstant(intCst)
    else
      negSums += IR_IntegerConstant(-intCst)

    if (vecExpr != null) {
      if (posSums.isEmpty && negSums.isEmpty)
        vecExpr
      else
        Logger.error("Unable to add VectorExpression with other Expression types")

    } else if (matExpr != null) {
      if (posSums.isEmpty && negSums.isEmpty) {
        matExpr
      } else {
        if (!matPos) matExpr.expressions.transform { x => IR_Negative(x) }
        posSums.transform {
          case exp : IR_Expression if exp.datatype.isInstanceOf[IR_ScalarDatatype] => IR_MatrixExpression.fromSingleExpression(exp.datatype, matExpr.rows, matExpr.columns, exp)
          case other                                                               => other
        }
        negSums.transform {
          case exp : IR_Expression if exp.datatype.isInstanceOf[IR_ScalarDatatype] => IR_MatrixExpression.fromSingleExpression(exp.datatype, matExpr.rows, matExpr.columns, IR_Negative(exp))
          case other                                                               => IR_Negative(other)
        }
        IR_Addition(ListBuffer(matExpr) ++ posSums ++ negSums)
      }

    } else if (posSums.length + negSums.length <= 1) { // result is only one summand (either a positive, or a negative, or 0)
      (posSums ++= negSums.transform(x => IR_Negative(x)) += IR_IntegerConstant(0L)).head

    } else if (posSums.length * negSums.length == 0 || compactAST) { // if compactAST is set do not create any Subtraction
      IR_Addition(posSums ++= negSums.transform(x => IR_Negative(x)))

    } else {
      val posExpr = if (posSums.length == 1) posSums.head else new IR_Addition(posSums)
      val negExpr = if (negSums.length == 1) negSums.head else new IR_Addition(negSums)
      IR_Subtraction(posExpr, negExpr)
    }
  }

  private def simplifyMult(facs : ListBuffer[IR_Expression], negate : Boolean) : IR_Expression = {
    // specialized simplification: (c/X) * (... + X*e + ...)  ->  c*e + (c/x) * (... + ...)
    // used for smoothers with variable coefficients
    var res : IR_Expression = null
    facs match {
      case ListBuffer(IR_Division(n, d), e) => res = cancelDownSummands(n, d, e)
      case ListBuffer(e, IR_Division(n, d)) => res = cancelDownSummands(n, d, e)
      case _                                =>
    }
    if (res != null)
      return if (negate) IR_Negative(res) else res

    var intCst : Long = if (negate) -1L else 1L
    var floatCst : Double = 1d
    var workQ = new mutable.Queue[IR_Expression]()
    val remA = new ArrayBuffer[IR_Expression]() // use ArrayBuffer here for a more efficient access to the last element
    var div : IR_Division = null
    for (f <- facs) {
      workQ.enqueue(f) // for nested Multiplication; this allows in-order processing
      do {
        val expr = workQ.dequeue()
        expr match {
          case IR_IntegerConstant(iv)                  => intCst *= iv
          case IR_RealConstant(fv)                     => floatCst *= fv
          case IR_Negative(e)                          =>
            workQ = (mutable.Queue() :+ e) ++ workQ
            intCst = -intCst
          case IR_Multiplication(iFacs)                =>
            workQ = mutable.Queue() ++ iFacs ++ workQ
          case d @ IR_Division(IR_RealConstant(fv), _) =>
            floatCst *= fv
            d.left = IR_RealConstant(1.0)
            if (div == null)
              div = d
            remA += d
          //          case _ : IR_VectorExpression | _ : IR_MatrixExpression =>
          //            if (remA.isEmpty)
          //              remA += expr
          //            else {
          //              // merging with one previous only is sufficient, if simplifyMult only matches first arg with vect/mat types
          //              val last = remA.last
          //              remA.trimEnd(1)
          //              remA ++= simplifyBinMult(last, expr)
          //            }
          // case r : IR_Expression => remA += r
          case r : IR_Expression =>
            if (remA.isEmpty) remA += r
            else {
              val last = remA.last
              remA.trimEnd(1)
              remA ++= simplifyBinMult(last, expr)
            }
        }

      } while (workQ.nonEmpty)
    }
    val rem = remA.to[ListBuffer]
    var cstDt : Option[IR_Datatype] = None
    val negative : Boolean = floatCst * intCst < 0d
    floatCst = math.abs(floatCst)
    intCst = math.abs(intCst)
    if (floatCst * intCst == 0d) {
      rem.clear()
      rem += IR_IntegerConstant(0L) // TODO: fix type
    } else if (div != null) {
      div.left = IR_RealConstant(floatCst * intCst)
    } else if (floatCst != 1d) {
      IR_RealConstant(floatCst * intCst) +=: rem // add constant at first position (it is expected as rem.head later)
      cstDt = Some(IR_RealDatatype)
    } else if (intCst != 1L) {
      IR_IntegerConstant(intCst) +=: rem // add constant at first position (it is expected as rem.head later)
      cstDt = Some(IR_IntegerDatatype)
    }

    var result : IR_Expression = null
    if (rem.isEmpty) {
      result = IR_IntegerConstant(1L) // TODO: fix type

    } else if (rem.length == 1 || floatCst * intCst == 0d) {
      result = rem.head

    } else {
      if (cstDt.isDefined) {
        var found : Boolean = false
        val coeff : IR_Expression = rem.head // this must be the constant factor (as added a few lines above)
        rem.transform {
          case v : IR_VectorExpression if !found =>
            found = true
            IR_VectorExpression(Some(IR_ResultingDatatype(cstDt.get, v.innerDatatype.getOrElse(IR_RealDatatype))), v.expressions.map(Duplicate(coeff) * _), v.rowVector)
          case m : IR_MatrixExpression if !found =>
            found = true
            IR_MatrixExpression(Some(IR_ResultingDatatype(cstDt.get, m.innerDatatype.getOrElse(IR_RealDatatype))), m.rows, m.columns, m.expressions.map(Duplicate(coeff) * _ : IR_Expression))
          case x                                 =>
            x
        }
        if (found)
          rem.remove(0)
      }
      result = IR_Multiplication(rem)
    }

    if (negative)
      result = IR_Negative(result)
    result
  }

  // specialized simplification: (c/X) * (... + X*e + ...)  ->  c*e + (c/x) * (... + ...)
  private def cancelDownSummands(num : IR_Expression, div : IR_Expression, expr : IR_Expression) : IR_Addition = {
    if (div.isInstanceOf[IR_IntegerConstant] || div.isInstanceOf[IR_FloatConstant])
      return null // constant divisiors will be simplified elsewhere
    val (orig, canc) = cancelDown(expr, div)
    if (!canc.isEmpty)
      IR_Addition(ListBuffer[IR_Expression](
        IR_Multiplication(num, IR_Addition(canc)),
        IR_Multiplication(IR_Division(num, div), IR_Addition(orig))))
    else
      null
  }

  private def cancelDown(expr : IR_Expression, div : IR_Expression) : (ListBuffer[IR_Expression], ListBuffer[IR_Expression]) = {
    val original = ListBuffer[IR_Expression]()
    val canceled = ListBuffer[IR_Expression]()
    expr match {
      case _ if expr == div                          =>
        canceled += IR_IntegerConstant(1L)
      case IR_Multiplication(fs) if fs.contains(div) =>
        canceled += IR_Multiplication(fs.filter(_ != div))
      case IR_Addition(sum)                          =>
        for (s <- sum) {
          val r = cancelDown(s, div)
          original ++= r._1
          canceled ++= r._2
        }
      case IR_Negative(e)                            =>
        val r = cancelDown(e, div)
        original ++= r._1.view.map(IR_Negative)
        canceled ++= r._2.view.map(IR_Negative)
      case IR_Subtraction(pos, neg)                  =>
        val rp = cancelDown(pos, div)
        original ++= rp._1
        canceled ++= rp._2
        val rn = cancelDown(neg, div)
        original ++= rn._1.view.map(IR_Negative)
        canceled ++= rn._2.view.map(IR_Negative)
      case _                                         =>
        original += expr
    }
    (original, canceled)
  }

  private def simplifyBinMult(le : IR_Expression, ri : IR_Expression) : Seq[IR_Expression] = {
    (le, ri) match { // matching for constants is not required here (this is already handled by the caller)
      case (left : IR_VectorExpression, right : IR_VectorExpression) =>
        if (left.length != right.length) Logger.error("Vector sizes must match for multiplication")
        if (left.rowVector.getOrElse(true) != right.rowVector.getOrElse(true)) Logger.error("Vector types must match for multiplication")
        List(IR_Addition(left.expressions.view.zip(right.expressions).map { x => x._1 * x._2 : IR_Expression }.to[ListBuffer]))

      case (left : IR_MatrixExpression, right : IR_MatrixExpression) =>
        if (left.columns != right.rows) Logger.error(s"Matrix sizes must match for multiplication - attempting ${ left.rows }x${ left.columns } * ${ right.rows }x${ right.columns }")
        val m = IR_MatrixExpression(IR_ResultingDatatype(left.innerDatatype.getOrElse(IR_RealDatatype), right.innerDatatype.getOrElse(IR_RealDatatype)), left.rows, right.columns)
        for (row <- 0 until m.rows) {
          for (col <- 0 until m.columns) {
            val entry = IR_Addition()
            for (k <- 0 until left.columns) {
              entry.summands += Duplicate(left.get(row, k)) * Duplicate(right.get(k, col))
            }
            m.set(row, col, entry)
          }
        }
        List(m)

      case (m : IR_MatrixExpression, v : IR_Expression) if (v.datatype.isInstanceOf[IR_ScalarDatatype]) => {
        m.expressions.transform(_ * Duplicate(v))
        List(m)
      }
      case (v : IR_Expression, m : IR_MatrixExpression) if (v.datatype.isInstanceOf[IR_ScalarDatatype]) => {
        m.expressions.transform(Duplicate(v) * _)
        List(m)
      }

      case _ => List(le, ri)
    }
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
