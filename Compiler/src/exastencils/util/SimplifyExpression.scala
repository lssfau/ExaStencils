package exastencils.util

import scala.collection.immutable.TreeMap
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.datastructures.ir._

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
    case MinimumExpression(l : ListBuffer[Expression])            => l.map(e => evalIntegral(e)).reduce((l, r) => l min r)
    case MaximumExpression(l : ListBuffer[Expression])            => l.map(e => evalIntegral(e)).reduce((l, r) => l max r)
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
    case MinimumExpression(l : ListBuffer[Expression])            => l.map(e => evalFloating(e)).reduce((l, r) => l min r)
    case MaximumExpression(l : ListBuffer[Expression])            => l.map(e => evalFloating(e)).reduce((l, r) => l max r)
    case _ =>
      throw new EvaluationException("unknown expression type for evaluation: " + expr.getClass())
  }

  /**
    * Constant string that is used to hold the additive constant of an affine expression in the result map of
    * evalIntegralAffine(Expression).
    */
  final val constName : Expression = NullExpression

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

      case acc : ArrayAccess =>
        res = new HashMap[Expression, Long]()
        res(acc) = 1L

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
        if ((mapL.size == 1 && mapL.contains(constName)) || mapL.isEmpty) {
          coeff = mapL.getOrElse(constName, 0L)
          res = mapR
        } else if ((mapR.size == 1 && mapR.contains(constName)) || mapR.isEmpty) {
          coeff = mapR.getOrElse(constName, 0L)
          res = mapL
        } else
          throw new EvaluationException("non-constant * non-constant is not yet implemented")
        if (coeff == 0L)
          res.clear()
        else
          for ((name : Expression, value : Long) <- res)
            res(name) = value * coeff

      case DivisionExpression(l, r) =>
        val tmp = extractIntegralSum(r)
        if (tmp.isEmpty)
          throw new EvaluationException("BOOM! (divide by zero)")
        if (!(tmp.size == 1 && tmp.contains(constName)))
          throw new EvaluationException("only constant divisor allowed yet")
        val div : Long = tmp(constName)
        tmp.clear()
        res = new HashMap[Expression, Long]()
        val mapL = extractIntegralSum(l)
        for ((name : Expression, value : Long) <- mapL)
          if (value % div == 0L) res.put(name, value / div)
          else tmp.put(name, value)
        val cstOpt = tmp.remove(constName) // const part in remaining dividend must not be larger then divisor
        if (cstOpt.isDefined) {
          val cst = cstOpt.get
          val cstMod = (cst % div + div) % div // mathematical modulo
          val cstRes = (cst - cstMod) / div
          tmp.put(constName, cstMod)
          res.put(constName, cstRes)
        }
        val dividend = recreateExprFromIntSum(tmp)
        val (name, update) : (Expression, Long) = dividend match {
          case IntegerConstant(x) => (constName, x / div)
          case _                  => (DivisionExpression(dividend, IntegerConstant(div)), 1L)
        }
        val njuVal : Long = res.get(name).getOrElse(0L) + update
        if (njuVal == 0) res.remove(name)
        else res.put(name, njuVal)

      case ModuloExpression(l, r) =>
        val tmp = extractIntegralSum(r)
        if (tmp.isEmpty)
          throw new EvaluationException("BOOM! (divide by zero)")
        if (!(tmp.size == 1 && tmp.contains(constName)))
          throw new EvaluationException("only constant divisor allowed")
        val mod : Long = tmp(constName)
        res = new HashMap[Expression, Long]()
        val dividendMap : HashMap[Expression, Long] = extractIntegralSum(l).filter(elem => elem._2 % mod != 0L)
        val cstOpt = dividendMap.remove(constName) // const part can be reduced
        if (cstOpt.isDefined)
          dividendMap.put(constName, (cstOpt.get % mod + mod) % mod) // mathematical modulo
        val dividend : Expression = recreateExprFromIntSum(dividendMap)
        dividend match {
          case IntegerConstant(x) => res.put(constName, (x % mod + mod) % mod) // java % != our modulo (symmetric vs mathematical version)
          case _                  => res.put(ModuloExpression(dividend, IntegerConstant(mod)), 1L)
        }

      case me : MinimumExpression =>
        res = new HashMap[Expression, Long]()
        val evaled = evalIntegral(me)
        if (evaled != 0)
          res(constName) = evaled

      case me : MaximumExpression =>
        res = new HashMap[Expression, Long]()
        val evaled = evalIntegral(me)
        if (evaled != 0)
          res(constName) = evaled

      case _ =>
        throw new EvaluationException("unknown expression type for evaluation: " + expr.getClass())
    }

    return res
  }

  /**
    * Takes the output from extractIntegralSum(..) and recreates an AST for this sum.
    */
  def recreateExprFromIntSum(unsortedMap : HashMap[Expression, Long]) : Expression = {

    var res : Expression = null
    val const : Option[Long] = unsortedMap.remove(constName)

    var map = TreeMap(unsortedMap.toSeq : _*)(Ordering.by(_.prettyprint))

    if (map.isEmpty)
      return IntegerConstant(const.getOrElse(0L))

    for ((expr : Expression, value : Long) <- map) {
      if (res == null) {
        res = value match {
          case 1L  => expr
          case -1L => UnaryExpression(UnaryOperators.Negative, expr)
          case _   => MultiplicationExpression(IntegerConstant(value), expr)
        }
      } else {
        val (summand, negative) : (Expression, Boolean) =
          value match {
            case 1L  => (expr, false)
            case -1L => (expr, true)
            case _   => (MultiplicationExpression(IntegerConstant(math.abs(value)), expr), value < 0)
          }
        res =
          if (negative) SubtractionExpression(res, summand)
          else AdditionExpression(res, summand)
      }
    }

    if (const.isDefined)
      res = if (const.get >= 0)
        AdditionExpression(res, IntegerConstant(const.get))
      else
        SubtractionExpression(res, IntegerConstant(-const.get))

    return res
  }

  def simplifyIntegralExpr(expr : Expression) : Expression = {
    try {
      return recreateExprFromIntSum(extractIntegralSum(expr))
    } catch {
      case EvaluationException(msg) =>
        throw new EvaluationException(msg + ";  in " + expr.prettyprint())
    }
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
        if (varName.contains("std::rand")) // HACK
          throw new EvaluationException("don't optimze code containing a call to std::rand")
        res = new HashMap[Expression, Double]()
        res(VariableAccess(varName, Some(RealDatatype()))) = 1d // ONLY VariableAccess in res keys, NO StringConstant

      case aAcc : ArrayAccess =>
        res = new HashMap[Expression, Double]()
        res(aAcc) = 1d

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
          throw new EvaluationException("non-constant * non-constant is not yet implemented:  " +
            l.prettyprint() + "  *  " + r.prettyprint())
        if (coeff == 0d)
          res.clear()
        else
          for ((name : Expression, value : Double) <- res)
            res(name) = value * coeff

      case DivisionExpression(l, r) =>
        val mapR = extractFloatingSum(r)
        if (!(mapR.size == 1 && mapR.contains(constName)))
          throw new EvaluationException("only constant divisor allowed yet:  " + l.prettyprint() + "  /  " + r.prettyprint())
        val div : Double = mapR(constName)
        res = extractFloatingSum(l)
        for ((name : Expression, value : Double) <- res)
          res(name) = value / div

      case ModuloExpression(l, r) =>
        val mapR = extractFloatingSum(r)
        if (!(mapR.size == 1 && mapR.contains(constName)))
          throw new EvaluationException("only constant divisor allowed" +
            l.prettyprint() + "  %  " + r.prettyprint())
        val mod : Double = mapR(constName)
        res = extractFloatingSum(l)
        for ((name : Expression, value : Double) <- res)
          res(name) = value % mod

      case me : MinimumExpression =>
        res = new HashMap[Expression, Double]()
        val evaled = evalFloating(me)
        if (evaled != 0)
          res(constName) = evaled

      case me : MaximumExpression =>
        res = new HashMap[Expression, Double]()
        val evaled = evalFloating(me)
        if (evaled != 0)
          res(constName) = evaled

      case _ =>
        throw new EvaluationException("unknown expression type for evaluation: " + expr.getClass() + " in " + expr.prettyprint())
    }

    return res
  }

  /**
    * Takes the output from extractFloatingSum(..) and recreates an AST for this sum.
    */
  def recreateExprFromFloatSum(unsortedMap : HashMap[Expression, Double]) : Expression = {

    var res : Expression = null
    val const : Option[Double] = unsortedMap.remove(constName)

    var map = TreeMap(unsortedMap.toSeq : _*)(Ordering.by(_.prettyprint))

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
      if (res == null) {
        res = value match {
          case 1d  => expr
          case -1d => UnaryExpression(UnaryOperators.Negative, expr)
          case _   => MultiplicationExpression(FloatConstant(value), expr)
        }
      } else {
        val (summand, negative) : (Expression, Boolean) =
          value match {
            case 1d  => (expr, false)
            case -1d => (expr, true)
            case _   => (MultiplicationExpression(FloatConstant(math.abs(value)), expr), value < 0)
          }
        res =
          if (negative) SubtractionExpression(res, summand)
          else AdditionExpression(res, summand)
      }
    }

    if (const.isDefined)
      res = AdditionExpression(res, FloatConstant(const.get))

    return res
  }

  def simplifyFloatingExpr(expr : Expression) : Expression = {
    try {
      return recreateExprFromFloatSum(extractFloatingSum(expr))
    } catch {
      case EvaluationException(msg) =>
        throw new EvaluationException(msg + ";  in " + expr.prettyprint())
    }
  }
}

case class EvaluationException(msg : String) extends Exception(msg) {}
