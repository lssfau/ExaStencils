package exastencils.util

import scala.collection.mutable.HashMap

import exastencils.datastructures.ir.AdditionExpression
import exastencils.datastructures.ir.DivisionExpression
import exastencils.datastructures.ir.Expression
import exastencils.datastructures.ir.FloatConstant
import exastencils.datastructures.ir.IntegerConstant
import exastencils.datastructures.ir.MultiplicationExpression
import exastencils.datastructures.ir.OffsetIndex
import exastencils.datastructures.ir.StringConstant
import exastencils.datastructures.ir.SubtractionExpression
import exastencils.datastructures.ir.VariableAccess

object SimplifyExpression {

  /**
    * Completely evaluates an integral expression.
    * Only IntegerConstants are allowed!
    * Other scalar constants or variable accesses lead to an EvaluationException.
    */
  def evalIntegral(expr : Expression) : Long = expr match {
    case IntegerConstant(v)                                       => v
    case AdditionExpression(l : Expression, r : Expression)       => evalIntegral(l) + evalIntegral(r)
    case SubtractionExpression(l : Expression, r : Expression)    => evalIntegral(l) - evalIntegral(r)
    case MultiplicationExpression(l : Expression, r : Expression) => evalIntegral(l) * evalIntegral(r)
    case DivisionExpression(l : Expression, r : Expression)       => evalIntegral(l) / evalIntegral(r)
    case _ =>
      throw new EvaluationException("unknown expression type for evaluation: " + expr.getClass())
  }

  /**
    * Completely evaluates an floating expression.
    * Only FloatConstants are allowed!
    * Other scalar constants or variable accesses lead to an EvaluationException.
    */
  def evalFloating(expr : Expression) : Double = expr match {
    case FloatConstant(v)                                         => v
    case AdditionExpression(l : Expression, r : Expression)       => evalFloating(l) + evalFloating(r)
    case SubtractionExpression(l : Expression, r : Expression)    => evalFloating(l) - evalFloating(r)
    case MultiplicationExpression(l : Expression, r : Expression) => evalFloating(l) * evalFloating(r)
    case DivisionExpression(l : Expression, r : Expression)       => evalFloating(l) / evalFloating(r)
    case _ =>
      throw new EvaluationException("unknown expression type for evaluation: " + expr.getClass())
  }

  /**
    * Constant string that is used to hold the additive constant of an affine expression in the result map of
    * evalIntegralAffine(Expression).
    */
  final val constName : String = ""

  /**
    * Evaluates an affine integral expression.
    * No float or boolean constants are allowed and the expression must be affine.
    * (Otherwise an EvaluationException is thrown.)
    *
    * Returns a map from all present variables to their corresponding coefficients.
    * The additive constant is stored beside the string specified by the field SimplifyExpression.constName.
    * The given expression is equivalent to: map(constName) + \sum_{n \in names} map(n) * n
    */
  def evalIntegralAffine(expr : Expression) : HashMap[String, Long] = {

    var res : HashMap[String, Long] = null

    expr match {

      case IntegerConstant(i) =>
        res = new HashMap[String, Long]()
        res(constName) = i

      case VariableAccess(varName, _) =>
        res = new HashMap[String, Long]()
        res(varName) = 1

      case StringConstant(varName) =>
        res = new HashMap[String, Long]()
        res(varName) = 1

      case AdditionExpression(l, r) =>
        res = evalIntegralAffine(l)
        for ((name : String, value : Long) <- evalIntegralAffine(r))
          res(name) = res.get(name).getOrElse(0L) + value

      case OffsetIndex(_, _, ind, off) =>
        res = evalIntegralAffine(ind)
        for ((name : String, value : Long) <- evalIntegralAffine(off))
          res(name) = res.get(name).getOrElse(0L) + value

      case SubtractionExpression(l, r) =>
        res = evalIntegralAffine(l)
        for ((name : String, value : Long) <- evalIntegralAffine(r))
          res(name) = res.get(name).getOrElse(0L) - value

      case MultiplicationExpression(l, r) =>
        val mapL = evalIntegralAffine(l)
        val mapR = evalIntegralAffine(r)
        var coeff : Long = 1
        if (mapL.size == 1 && mapL.contains(constName)) {
          coeff = mapL(constName)
          res = mapR
        } else if (mapR.size == 1 && mapR.contains(constName)) {
          coeff = mapR(constName)
          res = mapL
        } else
          throw new EvaluationException("non-constant * non-constant is not affine anymore")
        for ((name : String, value : Long) <- res)
          res(name) = value * coeff

      case DivisionExpression(l, r) =>
        val mapR = evalIntegralAffine(r)
        if (!(mapR.size == 1 && mapR.contains(constName)))
          throw new EvaluationException("only constant divisor allowed")
        val div : Long = mapR(constName)
        res = evalIntegralAffine(l)
        for ((name : String, value : Long) <- res)
          if (value % div == 0)
            res(name) = value / div
          else
            throw new EvaluationException("cannot handle division yet")

      case _ =>
        throw new EvaluationException("unknown expression type for evaluation: " + expr.getClass())
    }

    return res
  }
}

case class EvaluationException(msg : String) extends Exception(msg) {}
