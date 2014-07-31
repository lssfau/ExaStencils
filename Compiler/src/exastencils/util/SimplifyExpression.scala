package exastencils.util

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.datastructures.ir.AdditionExpression
import exastencils.datastructures.ir.ArrayAccess
import exastencils.datastructures.ir.DivisionExpression
import exastencils.datastructures.ir.Expression
import exastencils.datastructures.ir.FloatConstant
import exastencils.datastructures.ir.IntegerConstant
import exastencils.datastructures.ir.IntegerDatatype
import exastencils.datastructures.ir.ModuloExpression
import exastencils.datastructures.ir.MultiplicationExpression
import exastencils.datastructures.ir.NullExpression
import exastencils.datastructures.ir.RealDatatype
import exastencils.datastructures.ir.StringConstant
import exastencils.datastructures.ir.SubtractionExpression
import exastencils.datastructures.ir.UnaryExpression
import exastencils.datastructures.ir.UnaryOperators
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
    case ModuloExpression(l : Expression, r : Expression)         => evalIntegral(l) % evalIntegral(r)
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
    * Returns a map from all present summands to their corresponding coefficients.
    * The additive constant is stored beside the string specified by the field SimplifyExpression.constName.
    * The given expression is equivalent to: map(constName) + \sum_{n \in names} map(n) * n
    *
    * Only VariableAccess nodes are used as keys. (NO StringConstant)
    */
  def extractIntegralSum(expr : Expression) : HashMap[Expression, Long] = {

    var res : HashMap[Expression, Long] = null

    // prevent concurrent modification: use the following two buffers
    val remove = new ListBuffer[Expression]()
    var add : Expression = null

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
        res(VariableAccess(varName, Some(IntegerDatatype()))) = 1L // ONLY VariableAccess in res keys, NO StringConstant

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
        for ((name : Expression, value : Long) <- res) {
          if (value % div == 0L)
            res(name) = value / div
          else {
            remove += name
            val njuSummand : Expression =
              value match {
                case 1L  => name
                case -1L => UnaryExpression(UnaryOperators.Negative, name)
                case _   => MultiplicationExpression(IntegerConstant(value), name)
              }
            add = if (add == null) njuSummand else AdditionExpression(add, njuSummand)
          }
        }
        if (add != null)
          add = DivisionExpression(add, IntegerConstant(div))

      case ModuloExpression(l, r) =>
        val mapR = extractIntegralSum(r)
        if (!(mapR.size == 1 && mapR.contains(constName)))
          throw new EvaluationException("only constant divisor allowed")
        val mod : Long = mapR(constName)
        res = extractIntegralSum(l)
        for ((name : Expression, value : Long) <- res) {
          remove += name
          if (value % mod != 0L) {
            val njuSummand : Expression =
              value match {
                case 1L  => name
                case -1L => UnaryExpression(UnaryOperators.Negative, name)
                case _   => MultiplicationExpression(IntegerConstant(value), name)
              }
            add = if (add == null) njuSummand else AdditionExpression(add, njuSummand)
          }
        }
        if (add != null)
          add = ModuloExpression(add, IntegerConstant(mod))

      case _ =>
        throw new EvaluationException("unknown expression type for evaluation: " + expr.getClass())
    }

    for (r : Expression <- remove)
      res.remove(r)

    if (add != null) {
      val r = res.getOrElse(add, 0L) + 1L
      if (r == 0L)
        res.remove(add)
      else
        res(add) = r
    }

    return res
  }

  /**
    * Takes the output from extractIntegralSum(..) and recreates an AST for this sum.
    */
  def recreateExprFromIntSum(map : HashMap[Expression, Long]) : Expression = {

    var res : Expression = null
    val const : Option[Long] = map.remove(constName)

    if (map.isEmpty)
      return IntegerConstant(const.getOrElse(0L))

    for ((expr : Expression, value : Long) <- map) {
      val summand : Expression =
        value match {
          case 1L  => expr
          case -1L => UnaryExpression(UnaryOperators.Negative, expr)
          case _   => MultiplicationExpression(IntegerConstant(value), expr)
        }
      res = if (res == null) summand else AdditionExpression(res, summand)
    }

    if (const.isDefined)
      res = AdditionExpression(res, IntegerConstant(const.get))

    return res
  }

  def simplifyIntegralExpr(expr : Expression) : Expression = {
    return recreateExprFromIntSum(extractIntegralSum(expr))
  }

  /**
    * Evaluates and (implicitly) simplifies an floating-point expression.
    * No boolean constants are allowed.
    * (Otherwise an EvaluationException is thrown.)
    *
    * Returns a map from all present summands to their corresponding coefficients.
    * The additive constant is stored beside the string specified by the field SimplifyExpression.constName.
    * The given expression is equivalent to: map(constName) + \sum_{n \in names} map(n) * n
    *
    * Only VariableAccess and ArrayAccess nodes are used as keys. (NO StringConstant)
    */
  def extractFloatingSum(expr : Expression) : HashMap[Expression, Double] = {

    var res : HashMap[Expression, Double] = null

    expr match {

      case IntegerConstant(i) =>
        res = new HashMap[Expression, Double]()
        if (i != 0)
          res(constName) = i

      case FloatConstant(d) =>
        res = new HashMap[Expression, Double]()
        if (d != 0)
          res(constName) = d

      case VariableAccess(varName, _) =>
        res = new HashMap[Expression, Double]()
        res(VariableAccess(varName, Some(RealDatatype()))) = 1d

      case StringConstant(varName) =>
        res = new HashMap[Expression, Double]()
        res(VariableAccess(varName, Some(RealDatatype()))) = 1d // ONLY VariableAccess in res keys, NO StringConstant

      case aAcc : ArrayAccess =>
        res = new HashMap[Expression, Double]()
        res(aAcc) = 1d // ONLY VariableAccess in res keys, NO StringConstant

      case UnaryExpression(UnaryOperators.Negative, expr) =>
        res = extractFloatingSum(expr)
        for ((name : Expression, value : Double) <- extractFloatingSum(expr))
          res(name) = -value

      case AdditionExpression(l, r) =>
        res = extractFloatingSum(l)
        for ((name : Expression, value : Double) <- extractFloatingSum(r)) {
          val r = res.getOrElse(name, 0d) + value
          if (r == 0d)
            res.remove(name)
          else
            res(name) = r
        }

      case SubtractionExpression(l, r) =>
        res = extractFloatingSum(l)
        for ((name : Expression, value : Double) <- extractFloatingSum(r)) {
          val r = res.getOrElse(name, 0d) - value
          if (r == 0d)
            res.remove(name)
          else
            res(name) = r
        }

      case MultiplicationExpression(l, r) =>
        val mapL = extractFloatingSum(l)
        val mapR = extractFloatingSum(r)
        var coeff : Double = 1d
        if (mapL.size == 1 && mapL.contains(constName)) {
          coeff = mapL(constName)
          res = mapR
        } else if (mapR.size == 1 && mapR.contains(constName)) {
          coeff = mapR(constName)
          res = mapL
        } else
          throw new EvaluationException("non-constant * non-constant is not yet implemented")
        if (coeff == 0d)
          res.clear()
        else
          for ((name : Expression, value : Double) <- res)
            res(name) = value * coeff

      case DivisionExpression(l, r) =>
        val mapR = extractFloatingSum(r)
        if (!(mapR.size == 1 && mapR.contains(constName)))
          throw new EvaluationException("only constant divisor allowed yet")
        val div : Double = mapR(constName)
        res = extractFloatingSum(l)
        for ((name : Expression, value : Double) <- res)
          res(name) = value / div

      case ModuloExpression(l, r) =>
        val mapR = extractFloatingSum(r)
        if (!(mapR.size == 1 && mapR.contains(constName)))
          throw new EvaluationException("only constant divisor allowed")
        val mod : Double = mapR(constName)
        res = extractFloatingSum(l)
        for ((name : Expression, value : Double) <- res)
          res(name) = value % mod

      case _ =>
        throw new EvaluationException("unknown expression type for evaluation: " + expr.getClass())
    }

    return res
  }

  /**
    * Takes the output from extractFloatingSum(..) and recreates an AST for this sum.
    */
  def recreateExprFromFloatSum(map : HashMap[Expression, Double]) : Expression = {

    var res : Expression = null
    val const : Option[Double] = map.remove(constName)

    if (map.isEmpty)
      return FloatConstant(const.getOrElse(0d))

    // use distributive property
    val reverse = new HashMap[Double, Expression]()
    for ((njuExpr : Expression, value : Double) <- map) {
      val expr : Option[Expression] = reverse.get(value)
      reverse(value) =
        if (expr.isDefined)
          AdditionExpression(expr.get, njuExpr)
        else
          njuExpr
    }

    for ((value : Double, expr : Expression) <- reverse) {
      val summand : Expression =
        value match {
          case 1d  => expr
          case -1d => UnaryExpression(UnaryOperators.Negative, expr)
          case _   => MultiplicationExpression(FloatConstant(value), expr)
        }
      res = if (res == null) summand else AdditionExpression(res, summand)
    }

    if (const.isDefined)
      res = AdditionExpression(res, FloatConstant(const.get))

    return res
  }

  def simplifyFloatingExpr(expr : Expression) : Expression = {
    return recreateExprFromFloatSum(extractFloatingSum(expr))
  }
}

case class EvaluationException(msg : String) extends Exception(msg) {}
