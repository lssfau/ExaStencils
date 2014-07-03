package exastencils.util

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import exastencils.datastructures.ir.AdditionExpression
import exastencils.datastructures.ir.DivisionExpression
import exastencils.datastructures.ir.Expression
import exastencils.datastructures.ir.FloatConstant
import exastencils.datastructures.ir.IntegerConstant
import exastencils.datastructures.ir.IntegerDatatype
import exastencils.datastructures.ir.ModuloExpression
import exastencils.datastructures.ir.MultiplicationExpression
import exastencils.datastructures.ir.NullExpression
import exastencils.datastructures.ir.StringConstant
import exastencils.datastructures.ir.SubtractionExpression
import exastencils.datastructures.ir.VariableAccess
import exastencils.datastructures.ir.UnaryExpression
import exastencils.datastructures.ir.UnaryOperators

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
  final val constName : Expression = NullExpression()

  /**
    * Evaluates and (implicitly) simplifies an integral expression.
    * No float or boolean constants are allowed.
    * (Otherwise an EvaluationException is thrown.)
    *
    * Returns a sum from all present summands to their corresponding coefficients.
    * The additive constant is stored beside the string specified by the field SimplifyExpression.constName.
    * The given expression is equivalent to: map(constName) + \sum_{n \in names} map(n) * n
    */
  def extractIntegralSum(expr : Expression) : HashMap[Expression, Long] = {

    var res : HashMap[Expression, Long] = null

    // prevent concurrent modification: use the following two buffers
    val rem = new ListBuffer[Expression]()
    val add = new ListBuffer[(Expression, Long)]()

    expr match {

      case IntegerConstant(i) =>
        res = new HashMap[Expression, Long]()
        if (i != 0)
          res(constName) = i

      case VariableAccess(varName, _) =>
        res = new HashMap[Expression, Long]()
        res(VariableAccess(varName, Some(IntegerDatatype()))) = 1L

      case StringConstant(varName) =>
        res = new HashMap[Expression, Long]()
        res(VariableAccess(varName, Some(IntegerDatatype()))) = 1L

      case UnaryExpression(UnaryOperators.Negative, expr) =>
        res = extractIntegralSum(expr)
        for ((name : Expression, value : Long) <- extractIntegralSum(expr))
          res(name) = -value

      case AdditionExpression(l, r) =>
        res = extractIntegralSum(l)
        for ((name : Expression, value : Long) <- extractIntegralSum(r)) {
          val r = res.getOrElse(name, 0L) + value
          if (r == 0L)
            res.remove(name)
          else
            res(name) = r
        }

      case SubtractionExpression(l, r) =>
        res = extractIntegralSum(l)
        for ((name : Expression, value : Long) <- extractIntegralSum(r)) {
          val r = res.getOrElse(name, 0L) - value
          if (r == 0L)
            res.remove(name)
          else
            res(name) = r
        }

      case MultiplicationExpression(l, r) =>
        val mapL = extractIntegralSum(l)
        val mapR = extractIntegralSum(r)
        var coeff : Long = 1L
        if (mapL.size == 1 && mapL.contains(constName)) {
          coeff = mapL(constName)
          res = mapR
        } else if (mapR.size == 1 && mapR.contains(constName)) {
          coeff = mapR(constName)
          res = mapL
        } else
          throw new EvaluationException("non-constant * non-constant is not yet implemented")
        if (coeff == 0L)
          res.clear()
        else
          for ((name : Expression, value : Long) <- res)
            res(name) = value * coeff

      case DivisionExpression(l, r) =>
        val mapR = extractIntegralSum(r)
        if (!(mapR.size == 1 && mapR.contains(constName)))
          throw new EvaluationException("only constant divisor allowed yet")
        val div : Long = mapR(constName)
        res = extractIntegralSum(l)
        for ((name : Expression, value : Long) <- res)
          if (value % div == 0L)
            res(name) = value / div
          else {
            rem += name
            if (value == 1L)
              add += ((DivisionExpression(name, IntegerConstant(div)), 1L))
            else
              add += ((DivisionExpression(MultiplicationExpression(IntegerConstant(value), name), IntegerConstant(div)), 1L))
          }

      case ModuloExpression(l, r) =>
        val mapR = extractIntegralSum(r)
        if (!(mapR.size == 1 && mapR.contains(constName)))
          throw new EvaluationException("only constant divisor allowed")
        val mod : Long = mapR(constName)
        res = extractIntegralSum(l)
        for ((name : Expression, value : Long) <- res) {
          rem += name
          add += ((ModuloExpression(MultiplicationExpression(IntegerConstant(value), name), IntegerConstant(mod)), 1L))
        }

      case _ =>
        throw new EvaluationException("unknown expression type for evaluation: " + expr.getClass())
    }

    for (r : Expression <- rem)
      res.remove(r)
    for ((e, v) <- add) {
      val r = res.getOrElse(e, 0L) + v
      if (r == 0L)
        res.remove(e)
      else
        res(e) = r
    }

    return res
  }

  /**
    * Takes the output from extractIntegralSum(..) and recreates an AST for this sum.
    */
  def recreateExpressionFromSum(map : HashMap[Expression, Long]) : Expression = {

    if (map.isEmpty)
      return new IntegerConstant(0)

    var res : Expression = null
    val const : Option[Long] = map.remove(constName)
    if (const.isDefined)
      res = IntegerConstant(const.get)
    for ((expr : Expression, value : Long) <- map) {
      val summand : Expression = if (value == 1L) expr else MultiplicationExpression(IntegerConstant(value), expr)
      if (res == null)
        res = summand
      else
        res = AdditionExpression(res, summand)
    }

    if (res == null)
      res = IntegerConstant(0)

    return res
  }
}

case class EvaluationException(msg : String) extends Exception(msg) {}
