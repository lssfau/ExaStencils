package exastencils.optimization.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_SimplifyExpression extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/optimization/|LAYER_LC|/|LAYER_UC|_SimplifyExpression.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.optimization.|LAYER_LC|

import scala.collection._
import scala.collection.mutable.{ HashMap, ListBuffer }

import exastencils.base.|LAYER_LC|._
import exastencils.baseExt.|LAYER_LC|.|LAYER_UC|_FieldIteratorAccess
import exastencils.core._
import exastencils.datastructures._
import exastencils.util.|LAYER_LC|.|LAYER_UC|_MathFunctions

/// |LAYER_UC|_SimplifyExpression

// TODO: refactor -> less (convoluted) code
object |LAYER_UC|_SimplifyExpression {

  /**
    * Completely evaluates an integral expression.
    * Only IntegerConstants are allowed!
    * Other scalar constants or variable accesses lead to an EvaluationException.
    */
  def evalIntegral(expr : |LAYER_UC|_Expression) : Long = expr match {
    case |LAYER_UC|_IntegerConstant(v)                                => v
    case |LAYER_UC|_Addition(sums : ListBuffer[|LAYER_UC|_Expression])        => sums.view.map(s => evalIntegral(s)).sum
    case |LAYER_UC|_Subtraction(l : |LAYER_UC|_Expression, r : |LAYER_UC|_Expression) => evalIntegral(l) - evalIntegral(r)
    case |LAYER_UC|_Multiplication(facs : ListBuffer[|LAYER_UC|_Expression])  => facs.view.map(s => evalIntegral(s)).product
    case |LAYER_UC|_Division(l : |LAYER_UC|_Expression, r : |LAYER_UC|_Expression)    => evalIntegral(l) / evalIntegral(r)
    case |LAYER_UC|_Modulo(l : |LAYER_UC|_Expression, r : |LAYER_UC|_Expression)      => evalIntegral(l) % evalIntegral(r)
    case |LAYER_UC|_Minimum(l : ListBuffer[|LAYER_UC|_Expression])            => l.view.map(e => evalIntegral(e)).min
    case |LAYER_UC|_Maximum(l : ListBuffer[|LAYER_UC|_Expression])            => l.view.map(e => evalIntegral(e)).max
    case _                                                    =>
      throw EvaluationException("unknown expression type for evaluation: " + expr.getClass)
  }

  final val EXTREMA_MAP : String = "extremaMap" // associated value must be HashMap[String, (Long,Long)]

  def evalIntegralExtrema(expr : |LAYER_UC|_Expression) : (Long, Long) = {
    evalIntegralExtrema(expr, mutable.Map[String, (Long, Long)]())
  }

  /**
    * Completely evaluates an integral expression and computes a lower bound and an upper bound
    * for its value depending on the minOffset and maxOffset in potential OffsetIndex nodes.
    * Only IntegerConstants are allowed! (Except for the offset field in OffsetIndex, which is not evaluated at all.)
    * Other scalar constants or variable accesses lead to an EvaluationException.
    */
  def evalIntegralExtrema(expr : |LAYER_UC|_Expression, extremaLookup : Map[String, (Long, Long)]) : (Long, Long) = expr match {
    case |LAYER_UC|_IntegerConstant(v) =>
      (v, v)

    case |LAYER_UC|_StringConstant(value) if extremaLookup.contains(value) =>
      extremaLookup(value)

    case |LAYER_UC|_StringLiteral(value) if extremaLookup.contains(value) =>
      extremaLookup(value)

    case |LAYER_UC|_Addition(sums : ListBuffer[|LAYER_UC|_Expression]) =>
      sums.view.map(s => evalIntegralExtrema(s, extremaLookup)).reduce { (x, y) =>
        (x._1 + y._1, x._2 + y._2)
      }

    case |LAYER_UC|_Subtraction(l : |LAYER_UC|_Expression, r : |LAYER_UC|_Expression) =>
      val x = evalIntegralExtrema(l, extremaLookup)
      val y = evalIntegralExtrema(r, extremaLookup)
      (x._1 - y._2, x._2 - y._1)

    case |LAYER_UC|_Multiplication(facs : ListBuffer[|LAYER_UC|_Expression]) =>
      facs.view.map(s => evalIntegralExtrema(s, extremaLookup)).reduce { (x, y) =>
        val a = x._1 * y._1
        val b = x._1 * y._2
        val c = x._2 * y._1
        val d = x._2 * y._2
        (a min b min c min d, a max b max c max d)
      }

    case |LAYER_UC|_Division(l : |LAYER_UC|_Expression, r : |LAYER_UC|_Expression) =>
      val x = evalIntegralExtrema(l, extremaLookup)
      val y = evalIntegralExtrema(r, extremaLookup)
      val a = x._1 / y._1
      val b = x._1 / y._2
      val c = x._2 / y._1
      val d = x._2 / y._2
      (a min b min c min d, a max b max c max d)

    case |LAYER_UC|_Modulo(l : |LAYER_UC|_Expression, r : |LAYER_UC|_Expression) =>
      val x = evalIntegralExtrema(l, extremaLookup)
      val y = evalIntegralExtrema(r, extremaLookup)
      val a = x._1 % y._1
      val b = x._1 % y._2
      val c = x._2 % y._1
      val d = x._2 % y._2
      (a min b min c min d, a max b max c max d)

    case |LAYER_UC|_Minimum(l : ListBuffer[|LAYER_UC|_Expression]) =>
      l.view.map(e => evalIntegralExtrema(e, extremaLookup)).reduce { (x, y) =>
        (x._1 min y._1, x._2 min y._2)
      }

    case |LAYER_UC|_Maximum(l : ListBuffer[|LAYER_UC|_Expression]) =>
      l.view.map(e => evalIntegralExtrema(e, extremaLookup)).reduce { (x, y) =>
        (x._1 max y._1, x._2 max y._2)
      }

    case |LAYER_UC|_Negative(left : |LAYER_UC|_Expression) =>
      val (min, max) = evalIntegralExtrema(left, extremaLookup)
      (-max, -min)

    case |LAYER_UC|_FunctionCall(function, ListBuffer(l : |LAYER_UC|_Expression, r : |LAYER_UC|_Expression)) if "floord" == function.name =>
      evalIntegralExtrema(|LAYER_UC|_Division(l, r), extremaLookup)

    case _ =>
      throw EvaluationException("unknown expression type for evaluation: " + expr.getClass)
  }

  /**
    * Completely evaluates an floating expression.
    * Only FloatConstants are allowed!
    * Other scalar constants or variable accesses lead to an EvaluationException.
    */
  def evalFloating(expr : |LAYER_UC|_Expression) : Double = expr match {
    case |LAYER_UC|_RealConstant(v)                                   => v
    case |LAYER_UC|_Addition(sums : ListBuffer[|LAYER_UC|_Expression])        => sums.view.map(s => evalFloating(s)).sum
    case |LAYER_UC|_Subtraction(l : |LAYER_UC|_Expression, r : |LAYER_UC|_Expression) => evalFloating(l) - evalFloating(r)
    case |LAYER_UC|_Multiplication(facs : ListBuffer[|LAYER_UC|_Expression])  => facs.view.map(s => evalFloating(s)).product
    case |LAYER_UC|_Division(l : |LAYER_UC|_Expression, r : |LAYER_UC|_Expression)    => evalFloating(l) / evalFloating(r)
    case |LAYER_UC|_Minimum(l : ListBuffer[|LAYER_UC|_Expression])            => l.view.map(e => evalFloating(e)).min
    case |LAYER_UC|_Maximum(l : ListBuffer[|LAYER_UC|_Expression])            => l.view.map(e => evalFloating(e)).max
    case _                                                    =>
      throw EvaluationException("unknown expression type for evaluation: " + expr.getClass)
  }

  /**
    * Constant string that is used to hold the additive constant of an affine expression in the result map of
    * evalIntegralAffine(Expression).
    */
  final val constName : |LAYER_UC|_Expression = |LAYER_UC|_NullExpression

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
  def extractIntegralSum(expr : |LAYER_UC|_Expression) : HashMap[|LAYER_UC|_Expression, Long] = {
    extractIntegralSumRec(expr)
  }

  private def extractIntegralSumDivision(l : |LAYER_UC|_Expression, r : |LAYER_UC|_Expression, floor : Boolean) : HashMap[|LAYER_UC|_Expression, Long] = {
    var tmp = extractIntegralSumRec(r)
    if (tmp.isEmpty)
      throw EvaluationException("BOOM! (divide by zero)")
    if (!(tmp.size == 1 && tmp.contains(constName)))
      throw EvaluationException("only constant divisor allowed yet")
    val divs : Long = tmp(constName)
    tmp.clear()
    val res = new HashMap[|LAYER_UC|_Expression, Long]()
    val mapL = extractIntegralSumRec(l)
    if (floor) { // do only remove parts of the dividend when the rounding dl2ection is "uniform" (truncate rounds towards 0)
      for ((name : |LAYER_UC|_Expression, value : Long) <- mapL)
        if (value % divs == 0L) res(name) = value / divs
        else tmp(name) = value
      val cstOpt = tmp.remove(constName) // const part in remaining dividend must not be larger then divisor
      if (cstOpt.isDefined) {
        val cst = cstOpt.get
        val cstMod = (cst % divs + divs) % divs // mathematical modulo
        val cstRes = (cst - cstMod) / divs
        tmp(constName) = cstMod
        res(constName) = cstRes
      }
    } else
      tmp = mapL
    val dividend = recreateExprFromIntSum(tmp)
    val (name, update) : (|LAYER_UC|_Expression, Long) = dividend match {
      case |LAYER_UC|_IntegerConstant(x) =>
        val res =
          if (floor) {
            if (x < 0 && divs > 0)
              (x - divs + 1) / divs
            else if (x > 0 && divs < 0)
              (x - divs - 1) / divs
            else
              x / divs
          } else
            x / divs
        (constName, res)

      case |LAYER_UC|_Division(x, |LAYER_UC|_IntegerConstant(divs2)) if !floor                                                     =>
        (|LAYER_UC|_Division(x, |LAYER_UC|_IntegerConstant(divs * divs2)), 1L)
      case |LAYER_UC|_Addition(ListBuffer(|LAYER_UC|_Division(x, |LAYER_UC|_IntegerConstant(divs2)), |LAYER_UC|_IntegerConstant(const))) if !floor =>
        (simplifyIntegralExpr(|LAYER_UC|_Division(x + |LAYER_UC|_IntegerConstant(const * divs2), |LAYER_UC|_IntegerConstant(divs * divs2))), 1L)
      case |LAYER_UC|_Addition(ListBuffer(|LAYER_UC|_IntegerConstant(const), |LAYER_UC|_Division(x, |LAYER_UC|_IntegerConstant(divs2)))) if !floor =>
        (simplifyIntegralExpr(|LAYER_UC|_Division(x + |LAYER_UC|_IntegerConstant(const * divs2), |LAYER_UC|_IntegerConstant(divs * divs2))), 1L)

      case |LAYER_UC|_FunctionCall(function, ListBuffer(x, |LAYER_UC|_IntegerConstant(divs2))) if floor && "floord" == function.name                                                     =>
        (|LAYER_UC|_FunctionCall("floord", ListBuffer(x, |LAYER_UC|_IntegerConstant(divs * divs2))), 1L)
      case |LAYER_UC|_Addition(ListBuffer(|LAYER_UC|_FunctionCall(function, ListBuffer(x, |LAYER_UC|_IntegerConstant(divs2))), |LAYER_UC|_IntegerConstant(const))) if floor && "floord" == function.name =>
        (simplifyIntegralExpr(|LAYER_UC|_FunctionCall("floord", x + |LAYER_UC|_IntegerConstant(const * divs2), |LAYER_UC|_IntegerConstant(divs * divs2))), 1L)
      case |LAYER_UC|_Addition(ListBuffer(|LAYER_UC|_IntegerConstant(const), |LAYER_UC|_FunctionCall(function, ListBuffer(x, |LAYER_UC|_IntegerConstant(divs2))))) if floor && "floord" == function.name =>
        (simplifyIntegralExpr(|LAYER_UC|_FunctionCall("floord", x + |LAYER_UC|_IntegerConstant(const * divs2), |LAYER_UC|_IntegerConstant(divs * divs2))), 1L)
      case divd                                                                                                                                                          =>
        if (floor)
          (|LAYER_UC|_FunctionCall("floord", divd, |LAYER_UC|_IntegerConstant(divs)), 1L)
        else
          (|LAYER_UC|_Division(divd, |LAYER_UC|_IntegerConstant(divs)), 1L)
    }
    res(name) = res.getOrElse(name, 0L) + update
    res
  }

  private def extractIntegralSumRec(expr : |LAYER_UC|_Expression) : HashMap[|LAYER_UC|_Expression, Long] = {

    var res : HashMap[|LAYER_UC|_Expression, Long] = null

    expr match {

      case |LAYER_UC|_IntegerConstant(i) =>
        res = new HashMap[|LAYER_UC|_Expression, Long]()
        res(constName) = i

      case fia : |LAYER_UC|_FieldIteratorAccess =>
        res = new HashMap[|LAYER_UC|_Expression, Long]()
        res(fia) = 1l // preserve datatype if some

      case |LAYER_UC|_VariableAccess(varName, dt) =>
        res = new HashMap[|LAYER_UC|_Expression, Long]()
        res(|LAYER_UC|_VariableAccess(varName, dt)) = 1l // preserve datatype if some

      case |LAYER_UC|_Negative(neg) =>
        res = extractIntegralSumRec(neg)
        for ((name : |LAYER_UC|_Expression, value : Long) <- res)
          res(name) = -value

      case |LAYER_UC|_Addition(summands) =>
        res = new HashMap[|LAYER_UC|_Expression, Long]()
        for (s <- summands)
          for ((name : |LAYER_UC|_Expression, value : Long) <- extractIntegralSumRec(s))
            res(name) = res.getOrElse(name, 0L) + value
        // opt:  (x/2) + (x%2)  ==>  (x+1)/2
        val toOpt = new HashMap[|LAYER_UC|_Expression, (|LAYER_UC|_Division, |LAYER_UC|_Modulo, Long)]()
        for ((ex, coeff) <- res) ex match {
          case divd @ |LAYER_UC|_Division(x, |LAYER_UC|_IntegerConstant(2)) =>
            toOpt.get(x) match {
              case None                             => toOpt(x) = (divd, null, coeff)
              case Some((_, modd, c)) if c == coeff => toOpt(x) = (divd, modd, coeff)
              case Some(_)                          => toOpt -= x // coefficient is not matching...
            }
          case modd @ |LAYER_UC|_Modulo(x, |LAYER_UC|_IntegerConstant(2))   =>
            toOpt.get(x) match {
              case None                             => toOpt(x) = (null, modd, coeff)
              case Some((divd, _, c)) if c == coeff => toOpt(x) = (divd, modd, coeff)
              case Some(_)                          => toOpt -= x // coefficient is not matching...
            }
          case _                                            =>
        }
        for ((x, (div, mod, coeff)) <- toOpt) if (div != null && mod != null) {
          // ensure resulting map only contains normalized version created by recreateExprFromIntSum
          val nju = recreateExprFromIntSum(extractIntegralSumRec((x + |LAYER_UC|_IntegerConstant(1L)) / |LAYER_UC|_IntegerConstant(2L)))
          res -= div -= mod
          res(nju) = coeff + res.getOrElse(nju, 0L)
        }

      case |LAYER_UC|_Subtraction(l, r) =>
        res = extractIntegralSumRec(l)
        for ((name : |LAYER_UC|_Expression, value : Long) <- extractIntegralSumRec(r))
          res(name) = res.getOrElse(name, 0L) - value

      case |LAYER_UC|_Multiplication(facs) =>
        var coeff : Long = 1L
        val nonCst = new ListBuffer[|LAYER_UC|_Expression]()
        var nonCstMap : HashMap[|LAYER_UC|_Expression, Long] = null
        for (f <- facs) {
          val map = extractIntegralSumRec(f)
          if (map.size == 1 && map.contains(constName) || map.isEmpty) {
            coeff *= map.getOrElse(constName, 0L)
          } else {
            var gcdL : Long = math.abs(map.head._2)
            for ((_, c) <- map)
              gcdL = gcd(c, gcdL)
            for ((e, c) <- map)
              map(e) = c / gcdL
            coeff *= gcdL
            nonCstMap = map
            nonCst += recreateExprFromIntSum(map)
          }
        }
        res = new HashMap[|LAYER_UC|_Expression, Long]()
        if (nonCst.isEmpty)
          res(constName) = coeff
        else if (nonCst.length == 1)
          for ((name : |LAYER_UC|_Expression, value : Long) <- nonCstMap)
            res(name) = value * coeff
        else
          res(|LAYER_UC|_Multiplication(nonCst.sortBy(_.prettyprint()))) = coeff

      case |LAYER_UC|_Division(l, r) =>
        res = extractIntegralSumDivision(l, r, false)

      case |LAYER_UC|_FunctionCall(function, ListBuffer(l, r)) if "floord" == function.name =>
        res = extractIntegralSumDivision(l, r, true)

      case |LAYER_UC|_Modulo(l, r) =>
        val tmp = extractIntegralSumRec(r)
        if (tmp.isEmpty)
          throw EvaluationException("BOOM! (divide by zero)")
        if (!(tmp.size == 1 && tmp.contains(constName)))
          throw EvaluationException("only constant divisor allowed")
        val mod : Long = tmp(constName)
        res = new HashMap[|LAYER_UC|_Expression, Long]()
        val dividendMap : HashMap[|LAYER_UC|_Expression, Long] = extractIntegralSumRec(l)
        val dividend : |LAYER_UC|_Expression = recreateExprFromIntSum(dividendMap)
        dividend match {
          case |LAYER_UC|_IntegerConstant(x) => res(constName) = x % mod
          case _                     => res(|LAYER_UC|_Modulo(dividend, |LAYER_UC|_IntegerConstant(mod))) = 1L
        }

      case |LAYER_UC|_Minimum(args : ListBuffer[|LAYER_UC|_Expression]) =>
        val exprs = new ListBuffer[|LAYER_UC|_Expression]
        var min : java.lang.Long = null
        for (arg <- args) simplifyIntegralExpr(arg) match {
          case |LAYER_UC|_IntegerConstant(c)   => min = if (min == null || min > c) c else min
          case e if !exprs.contains(e) => exprs += e
          case _                       => // we already found a (syntactically) identical expression, so skip this one
        }
        res = new HashMap[|LAYER_UC|_Expression, Long]()
        if (exprs.isEmpty)
          res(constName) = min
        else {
          if (min != null)
            exprs += |LAYER_UC|_IntegerConstant(min)
          res(|LAYER_UC|_Minimum(exprs)) = 1L
        }

      case |LAYER_UC|_Maximum(args : ListBuffer[|LAYER_UC|_Expression]) =>
        val exprs = new ListBuffer[|LAYER_UC|_Expression]
        var max : java.lang.Long = null
        for (arg <- args) simplifyIntegralExpr(arg) match {
          case |LAYER_UC|_IntegerConstant(c)   => max = if (max == null || max < c) c else max
          case e if !exprs.contains(e) => exprs += e
          case _                       => // we already found a (syntactically) identical expression, so skip this one
        }
        res = new HashMap[|LAYER_UC|_Expression, Long]()
        if (exprs.isEmpty)
          res(constName) = max
        else {
          if (max != null)
            exprs += |LAYER_UC|_IntegerConstant(max)
          res(|LAYER_UC|_Minimum(exprs)) = 1L
        }

      case _ =>
        throw EvaluationException("unknown expression type for evaluation: " + expr.getClass)
    }

    res.filter(e => e._2 != 0L)
  }

  def gcd(x : Long, y : Long) : Long = {
    var a : Long = x
    var b : Long = y
    while (b != 0) {
      val h = a % b
      a = b
      b = h
    }
    math.abs(a)
  }

  /**
    * Takes the output from extractIntegralSum(..) and recreates an AST for this sum.
    */
  def recreateExprFromIntSum(sumMap : HashMap[|LAYER_UC|_Expression, Long]) : |LAYER_UC|_Expression = {

    val const : Long = sumMap.getOrElse(constName, 0L)

    val sumSeq = sumMap.view.filter(s => s._1 != constName && s._2 != 0L).toSeq.sortWith({
      case ((e1, _), (e2, _)) => e1.prettyprint() < e2.prettyprint()
    })

    if (sumSeq.isEmpty)
      return |LAYER_UC|_IntegerConstant(const)

    // use distributive property
    val reverse = new HashMap[Long, (ListBuffer[|LAYER_UC|_Expression], ListBuffer[|LAYER_UC|_Expression])]()
    def empty = (new ListBuffer[|LAYER_UC|_Expression](), new ListBuffer[|LAYER_UC|_Expression]())
    for ((njuExpr : |LAYER_UC|_Expression, value : Long) <- sumSeq)
      if (value > 0L)
        reverse.getOrElseUpdate(value, empty)._1 += njuExpr
      else
        reverse.getOrElseUpdate(-value, empty)._2 += njuExpr

    val posSums = new ListBuffer[|LAYER_UC|_Expression]()
    val negSums = new ListBuffer[|LAYER_UC|_Expression]()

    def toExpr(sum : ListBuffer[|LAYER_UC|_Expression]) = if (sum.length == 1) sum.head else new |LAYER_UC|_Addition(sum)
    for ((value : Long, (pSums : ListBuffer[|LAYER_UC|_Expression], nSums : ListBuffer[|LAYER_UC|_Expression])) <- reverse.toSeq.sortBy(t => t._1).reverse)
      if (value == 1L) {
        posSums ++= pSums
        negSums ++= nSums
      } else {
        if (pSums.isEmpty)
          negSums += |LAYER_UC|_Multiplication(|LAYER_UC|_IntegerConstant(value), toExpr(nSums))
        else if (nSums.isEmpty)
          posSums += |LAYER_UC|_Multiplication(|LAYER_UC|_IntegerConstant(value), toExpr(pSums))
        else
          posSums += |LAYER_UC|_Multiplication(|LAYER_UC|_IntegerConstant(value), |LAYER_UC|_Subtraction(toExpr(pSums), toExpr(nSums)))
      }

    if (const > 0L)
      posSums += |LAYER_UC|_IntegerConstant(const)
    else if (const < 0L)
      negSums += |LAYER_UC|_IntegerConstant(-const)

    if (posSums.isEmpty)
      |LAYER_UC|_Negative(toExpr(negSums))
    else if (negSums.isEmpty)
      toExpr(posSums)
    else
      |LAYER_UC|_Subtraction(toExpr(posSums), toExpr(negSums))
  }

  def simplifyIntegralExpr(expr : |LAYER_UC|_Expression) : |LAYER_UC|_Expression = {
    try {
      val res = |LAYER_UC|_ExpressionStatement(recreateExprFromIntSum(extractIntegralSum(expr)))
      |LAYER_UC|_GeneralSimplify.doUntilDoneStandalone(res)
      res.expression
    } catch {
      case ex : EvaluationException =>
        throw EvaluationException(ex.msg + ";  in " + expr.prettyprint(), ex)
    }
  }

  object SimplifyIndices extends QuietDefaultStrategy("Simplify indices") {
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
  def extractFloatingSum(expr : |LAYER_UC|_Expression) : HashMap[|LAYER_UC|_Expression, Double] = {
    extractFloatingSumRec(expr)
  }

  private def extractFloatingSumRec(expr : |LAYER_UC|_Expression) : HashMap[|LAYER_UC|_Expression, Double] = {

    var res : HashMap[|LAYER_UC|_Expression, Double] = null

    expr match {

      case |LAYER_UC|_IntegerConstant(i) =>
        res = new HashMap[|LAYER_UC|_Expression, Double]()
        res(constName) = i

      case |LAYER_UC|_RealConstant(d) =>
        res = new HashMap[|LAYER_UC|_Expression, Double]()
        res(constName) = d

      case fia : |LAYER_UC|_FieldIteratorAccess =>
        res = new HashMap[|LAYER_UC|_Expression, Double]()
        res(fia) = 1d // preserve datatype if some

      case |LAYER_UC|_VariableAccess(varName, dt) =>
        res = new HashMap[|LAYER_UC|_Expression, Double]()
        res(|LAYER_UC|_VariableAccess(varName, dt)) = 1d // preserve datatype if some

      case call : |LAYER_UC|_FunctionCall =>
        if (call.name.contains("std::rand")) // HACK
          throw EvaluationException("don't optimize code containing a call to std::rand")
        def simplifyFloatingArgs(pars : Seq[|LAYER_UC|_Datatype]) : Unit = {
          call.arguments =
            pars.view.zip(call.arguments).map {
              case (|LAYER_UC|_RealDatatype, arg) =>
                simplifyFloatingExpr(arg)
              case (_, arg)               =>
                arg
            }.to[ListBuffer]
        }
        if (|LAYER_UC|_MathFunctions.signatures.contains(call.name))
          simplifyFloatingArgs(|LAYER_UC|_MathFunctions.signatures(call.name)._1)
        res = new HashMap[|LAYER_UC|_Expression, Double]()
        res(call) = 1d

      case |LAYER_UC|_Negative(neg) =>
        res = extractFloatingSumRec(neg)
        for ((name : |LAYER_UC|_Expression, value : Double) <- res)
          res(name) = -value

      case |LAYER_UC|_Addition(summands) =>
        res = new HashMap[|LAYER_UC|_Expression, Double]()
        for (s <- summands)
          for ((name : |LAYER_UC|_Expression, value : Double) <- extractFloatingSumRec(s))
            res(name) = res.getOrElse(name, 0d) + value

      case |LAYER_UC|_Subtraction(l, r) =>
        res = extractFloatingSumRec(l)
        for ((name : |LAYER_UC|_Expression, value : Double) <- extractFloatingSumRec(r))
          res(name) = res.getOrElse(name, 0d) - value

      case |LAYER_UC|_Multiplication(facs) =>
        var coeff : Double = 1d
        val nonCst = new ListBuffer[HashMap[|LAYER_UC|_Expression, Double]]()
        for (f <- facs) {
          val map = extractFloatingSumRec(f)
          if (map.size == 1 && map.contains(constName) || map.isEmpty)
            coeff *= map.getOrElse(constName, 0d)
          else
            nonCst += map
        }
        res = new HashMap[|LAYER_UC|_Expression, Double]()
        if (nonCst.isEmpty)
          res(constName) = coeff
        else if (nonCst.length == 1)
          for ((name : |LAYER_UC|_Expression, value : Double) <- nonCst.head)
            res(name) = value * coeff
        else {
          for (nC <- nonCst) {
            val it = nC.view.filter(_._1 != constName).map(_._2).iterator
            val v : Double = math.abs(it.next())
            var allId : Boolean = true
            while (it.hasNext && allId)
              allId = math.abs(it.next()) == v
            if (allId) {
              coeff *= v
              nC.transform((expr, d) => d / v)
            }
          }
          res(|LAYER_UC|_Multiplication(nonCst.map { sum => recreateExprFromFloatSum(sum) })) = coeff
        }

      case |LAYER_UC|_Division(l, r) =>
        val mapL = extractFloatingSumRec(l)
        val mapR = extractFloatingSumRec(r)
        if (mapR.size == 1 && mapR.contains(constName)) {
          val div : Double = mapR(constName)
          res = mapL
          for ((name : |LAYER_UC|_Expression, value : Double) <- res)
            res(name) = value / div
        } else {
          var coefL : Double = 1d
          var exprL = recreateExprFromFloatSum(mapL)
          var coefR : Double = 1d
          var exprR = recreateExprFromFloatSum(mapR)
          exprL match {
            case |LAYER_UC|_Multiplication(ListBuffer(|LAYER_UC|_RealConstant(coef), inner)) =>
              coefL = coef
              exprL = inner
            case |LAYER_UC|_RealConstant(coef)                                       =>
              coefL = coef
              exprL = |LAYER_UC|_RealConstant(1d)
            case _                                                           =>
          }
          exprR match {
            case |LAYER_UC|_Multiplication(ListBuffer(|LAYER_UC|_RealConstant(coef), inner)) =>
              coefR = coef
              exprR = inner
            case |LAYER_UC|_RealConstant(coef)                                       =>
              coefR = coef
              exprR = |LAYER_UC|_RealConstant(1d)
            case _                                                           =>
          }
          res = new HashMap[|LAYER_UC|_Expression, Double]()
          val div = |LAYER_UC|_Division(exprL, exprR)
          res(div) = coefL / coefR
        }

      case |LAYER_UC|_Modulo(l, r) =>
        val mapR = extractFloatingSumRec(r)
        if (!(mapR.size == 1 && mapR.contains(constName)))
          throw EvaluationException("only constant divisor allowed yet:  " + l.prettyprint() + "  %  " + r.prettyprint())
        val mod : Double = mapR(constName)
        res = extractFloatingSumRec(l)
        for ((name : |LAYER_UC|_Expression, value : Double) <- res)
          res(name) = value % mod

      case |LAYER_UC|_Minimum(args : ListBuffer[|LAYER_UC|_Expression]) =>
        val exprs = new ListBuffer[|LAYER_UC|_Expression]
        var min : java.lang.Double = null
        for (arg <- args) simplifyFloatingExpr(arg) match {
          case |LAYER_UC|_RealConstant(c)      => min = if (min == null || min > c) c else min
          case e if !exprs.contains(e) => exprs += e
          case _                       => // we already found a (syntactically) identical expression, so skip this one
        }
        res = new HashMap[|LAYER_UC|_Expression, Double]()
        if (exprs.isEmpty)
          res(constName) = min
        else {
          if (min != null)
            exprs += |LAYER_UC|_RealConstant(min)
          res(|LAYER_UC|_Minimum(exprs)) = 1d
        }

      case |LAYER_UC|_Maximum(args : ListBuffer[|LAYER_UC|_Expression]) =>
        val exprs = new ListBuffer[|LAYER_UC|_Expression]
        var max : java.lang.Double = null
        for (arg <- args) simplifyFloatingExpr(arg) match {
          case |LAYER_UC|_RealConstant(c)      => max = if (max == null || max < c) c else max
          case e if !exprs.contains(e) => exprs += e
          case _                       => // we already found a (syntactically) identical expression, so skip this one
        }
        res = new HashMap[|LAYER_UC|_Expression, Double]()
        if (exprs.isEmpty)
          res(constName) = max
        else {
          if (max != null)
            exprs += |LAYER_UC|_RealConstant(max)
          res(|LAYER_UC|_Maximum(exprs)) = 1d
        }

      case |LAYER_UC|_Power(l, r) =>
        val mapL = extractFloatingSumRec(l)
        val mapR = extractFloatingSumRec(r)
        // trivial solution, if no better one is found later on
        res = new HashMap[|LAYER_UC|_Expression, Double]()
        res(|LAYER_UC|_Power(recreateExprFromFloatSum(mapL), recreateExprFromFloatSum(mapR))) = 1d
        val isLCst = mapL.size == 1 && mapL.contains(constName)
        val isRCst = mapR.size == 1 && mapR.contains(constName)
        if (isLCst && isRCst)
          mapL(constName) = math.pow(mapL(constName), mapR(constName))
        else if (isRCst) {
          val exp : Double = mapR(constName)
          val expL : Long = exp.toLong
          if (expL.toDouble == exp) expL match {
            case 0                           => res = HashMap(constName -> 1d)
            case 1                           => res = mapL
            case _ if expL >= 2 && expL <= 6 =>
              res = extractFloatingSumRec(|LAYER_UC|_Multiplication(ListBuffer.fill(expL.toInt)(Duplicate(l))))
            case _                           =>
          }
        }

      case _ =>
        throw EvaluationException("unknown expression type for evaluation: " + expr.getClass + " in " + expr.prettyprint())
    }

    res.filter(e => e._2 != 0d)
  }

  /**
    * Takes the output from extractFloatingSum(..) and recreates an AST for this sum.
    */
  def recreateExprFromFloatSum(sumMap : HashMap[|LAYER_UC|_Expression, Double]) : |LAYER_UC|_Expression = {

    val const : Double = sumMap.getOrElse(constName, 0d)

    val sumSeq = sumMap.view.filter(s => s._1 != constName && s._2 != 0d).toSeq.sortWith({
      case ((e1, _), (e2, _)) => e1.prettyprint() < e2.prettyprint()
    })

    if (sumSeq.isEmpty)
      return |LAYER_UC|_RealConstant(const)

    // use distributive property
    val reverse = new HashMap[Double, (ListBuffer[|LAYER_UC|_Expression], ListBuffer[|LAYER_UC|_Expression])]()
    def empty = (new ListBuffer[|LAYER_UC|_Expression](), new ListBuffer[|LAYER_UC|_Expression]())
    for ((njuExpr : |LAYER_UC|_Expression, value : Double) <- sumSeq)
      if (value > 0d)
        reverse.getOrElseUpdate(value, empty)._1 += njuExpr
      else
        reverse.getOrElseUpdate(-value, empty)._2 += njuExpr

    val posSums = new ListBuffer[|LAYER_UC|_Expression]()
    val negSums = new ListBuffer[|LAYER_UC|_Expression]()

    def toExpr(sum : ListBuffer[|LAYER_UC|_Expression]) = if (sum.length == 1) sum.head else new |LAYER_UC|_Addition(sum)
    for ((value : Double, (pSums : ListBuffer[|LAYER_UC|_Expression], nSums : ListBuffer[|LAYER_UC|_Expression])) <- reverse.toSeq.sortBy(t => t._1).reverse)
      if (value == 1d) {
        posSums ++= pSums
        negSums ++= nSums
      } else {
        if (pSums.isEmpty)
          negSums += |LAYER_UC|_Multiplication(|LAYER_UC|_RealConstant(value), toExpr(nSums))
        else if (nSums.isEmpty)
          posSums += |LAYER_UC|_Multiplication(|LAYER_UC|_RealConstant(value), toExpr(pSums))
        else
          posSums += |LAYER_UC|_Multiplication(|LAYER_UC|_RealConstant(value), |LAYER_UC|_Subtraction(toExpr(pSums), toExpr(nSums)))
      }

    if (const > 0d)
      posSums += |LAYER_UC|_RealConstant(const)
    else if (const < 0d)
      negSums += |LAYER_UC|_RealConstant(-const)

    if (posSums.isEmpty)
      |LAYER_UC|_Negative(toExpr(negSums))
    else if (negSums.isEmpty)
      toExpr(posSums)
    else
      |LAYER_UC|_Subtraction(toExpr(posSums), toExpr(negSums))
  }

  def simplifyFloatingExpr(expr : |LAYER_UC|_Expression) : |LAYER_UC|_Expression = {
    try {
      val res = |LAYER_UC|_ExpressionStatement(recreateExprFromFloatSum(extractFloatingSum(expr)))
      |LAYER_UC|_GeneralSimplify.doUntilDoneStandalone(res)
      res.expression
    } catch {
      case ex : EvaluationException =>
        throw EvaluationException(ex.msg + ";  in " + expr.prettyprint(), ex)
    }
  }
}

/// EvaluationException

// TODO: move somewhere more reasonable
case class EvaluationException(msg : String, cause : Throwable = null) extends Exception(msg, cause) {}
"""
  }
}
