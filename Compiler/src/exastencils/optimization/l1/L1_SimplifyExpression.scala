package exastencils.optimization.l1

import scala.collection._
import scala.collection.mutable.{ HashMap, ListBuffer }

import exastencils.base.l1._
import exastencils.core._
import exastencils.datastructures._
import exastencils.util.l1.L1_MathFunctions

/// L1_SimplifyExpression

// TODO: refactor -> less (convoluted) code
object L1_SimplifyExpression {

  /**
    * Completely evaluates an integral expression.
    * Only IntegerConstants are allowed!
    * Other scalar constants or variable accesses lead to an EvaluationException.
    */
  def evalIntegral(expr : L1_Expression) : Long = expr match {
    case L1_IntegerConstant(v)                                => v
    case L1_Addition(sums : ListBuffer[L1_Expression])        => sums.view.map(s => evalIntegral(s)).sum
    case L1_Subtraction(l : L1_Expression, r : L1_Expression) => evalIntegral(l) - evalIntegral(r)
    case L1_Multiplication(facs : ListBuffer[L1_Expression])  => facs.view.map(s => evalIntegral(s)).product
    case L1_Division(l : L1_Expression, r : L1_Expression)    => evalIntegral(l) / evalIntegral(r)
    case L1_Modulo(l : L1_Expression, r : L1_Expression)      => evalIntegral(l) % evalIntegral(r)
    case L1_Minimum(l : ListBuffer[L1_Expression])            => l.view.map(e => evalIntegral(e)).min
    case L1_Maximum(l : ListBuffer[L1_Expression])            => l.view.map(e => evalIntegral(e)).max
    case _                                                    =>
      throw EvaluationException("unknown expression type for evaluation: " + expr.getClass)
  }

  final val EXTREMA_MAP : String = "extremaMap" // associated value must be HashMap[String, (Long,Long)]

  def evalIntegralExtrema(expr : L1_Expression) : (Long, Long) = {
    evalIntegralExtrema(expr, mutable.Map[String, (Long, Long)]())
  }

  /**
    * Completely evaluates an integral expression and computes a lower bound and an upper bound
    * for its value depending on the minOffset and maxOffset in potential OffsetIndex nodes.
    * Only IntegerConstants are allowed! (Except for the offset field in OffsetIndex, which is not evaluated at all.)
    * Other scalar constants or variable accesses lead to an EvaluationException.
    */
  def evalIntegralExtrema(expr : L1_Expression, extremaLookup : Map[String, (Long, Long)]) : (Long, Long) = expr match {
    case L1_IntegerConstant(v) =>
      (v, v)

    case L1_StringConstant(value) if extremaLookup.contains(value) =>
      extremaLookup(value)

    case L1_StringLiteral(value) if extremaLookup.contains(value) =>
      extremaLookup(value)

    case L1_Addition(sums : ListBuffer[L1_Expression]) =>
      sums.view.map(s => evalIntegralExtrema(s, extremaLookup)).reduce { (x, y) =>
        (x._1 + y._1, x._2 + y._2)
      }

    case L1_Subtraction(l : L1_Expression, r : L1_Expression) =>
      val x = evalIntegralExtrema(l, extremaLookup)
      val y = evalIntegralExtrema(r, extremaLookup)
      (x._1 - y._2, x._2 - y._1)

    case L1_Multiplication(facs : ListBuffer[L1_Expression]) =>
      facs.view.map(s => evalIntegralExtrema(s, extremaLookup)).reduce { (x, y) =>
        val a = x._1 * y._1
        val b = x._1 * y._2
        val c = x._2 * y._1
        val d = x._2 * y._2
        (a min b min c min d, a max b max c max d)
      }

    case L1_Division(l : L1_Expression, r : L1_Expression) =>
      val x = evalIntegralExtrema(l, extremaLookup)
      val y = evalIntegralExtrema(r, extremaLookup)
      val a = x._1 / y._1
      val b = x._1 / y._2
      val c = x._2 / y._1
      val d = x._2 / y._2
      (a min b min c min d, a max b max c max d)

    case L1_Modulo(l : L1_Expression, r : L1_Expression) =>
      val x = evalIntegralExtrema(l, extremaLookup)
      val y = evalIntegralExtrema(r, extremaLookup)
      val a = x._1 % y._1
      val b = x._1 % y._2
      val c = x._2 % y._1
      val d = x._2 % y._2
      (a min b min c min d, a max b max c max d)

    case L1_Minimum(l : ListBuffer[L1_Expression]) =>
      l.view.map(e => evalIntegralExtrema(e, extremaLookup)).reduce { (x, y) =>
        (x._1 min y._1, x._2 min y._2)
      }

    case L1_Maximum(l : ListBuffer[L1_Expression]) =>
      l.view.map(e => evalIntegralExtrema(e, extremaLookup)).reduce { (x, y) =>
        (x._1 max y._1, x._2 max y._2)
      }

    case L1_Negative(left : L1_Expression) =>
      val (min, max) = evalIntegralExtrema(left, extremaLookup)
      (-max, -min)

    case L1_FunctionCall(function, ListBuffer(l : L1_Expression, r : L1_Expression)) if "floord" == function.name =>
      evalIntegralExtrema(L1_Division(l, r), extremaLookup)

    case _ =>
      throw EvaluationException("unknown expression type for evaluation: " + expr.getClass)
  }

  /**
    * Completely evaluates an floating expression.
    * Only FloatConstants are allowed!
    * Other scalar constants or variable accesses lead to an EvaluationException.
    */
  def evalFloating(expr : L1_Expression) : Double = expr match {
    case L1_RealConstant(v)                                   => v
    case L1_Addition(sums : ListBuffer[L1_Expression])        => sums.view.map(s => evalFloating(s)).sum
    case L1_Subtraction(l : L1_Expression, r : L1_Expression) => evalFloating(l) - evalFloating(r)
    case L1_Multiplication(facs : ListBuffer[L1_Expression])  => facs.view.map(s => evalFloating(s)).product
    case L1_Division(l : L1_Expression, r : L1_Expression)    => evalFloating(l) / evalFloating(r)
    case L1_Minimum(l : ListBuffer[L1_Expression])            => l.view.map(e => evalFloating(e)).min
    case L1_Maximum(l : ListBuffer[L1_Expression])            => l.view.map(e => evalFloating(e)).max
    case _                                                    =>
      throw EvaluationException("unknown expression type for evaluation: " + expr.getClass)
  }

  /**
    * Constant string that is used to hold the additive constant of an affine expression in the result map of
    * evalIntegralAffine(Expression).
    */
  final val constName : L1_Expression = L1_NullExpression

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
  def extractIntegralSum(expr : L1_Expression) : HashMap[L1_Expression, Long] = {
    extractIntegralSumRec(expr)
  }

  private def extractIntegralSumDivision(l : L1_Expression, r : L1_Expression, floor : Boolean) : HashMap[L1_Expression, Long] = {
    var tmp = extractIntegralSumRec(r)
    if (tmp.isEmpty)
      throw EvaluationException("BOOM! (divide by zero)")
    if (!(tmp.size == 1 && tmp.contains(constName)))
      throw EvaluationException("only constant divisor allowed yet")
    val divs : Long = tmp(constName)
    tmp.clear()
    val res = new HashMap[L1_Expression, Long]()
    val mapL = extractIntegralSumRec(l)
    if (floor) { // do only remove parts of the dividend when the rounding dl2ection is "uniform" (truncate rounds towards 0)
      for ((name : L1_Expression, value : Long) <- mapL)
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
    val (name, update) : (L1_Expression, Long) = dividend match {
      case L1_IntegerConstant(x) =>
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

      case L1_Division(x, L1_IntegerConstant(divs2)) if !floor                                                     =>
        (L1_Division(x, L1_IntegerConstant(divs * divs2)), 1L)
      case L1_Addition(ListBuffer(L1_Division(x, L1_IntegerConstant(divs2)), L1_IntegerConstant(const))) if !floor =>
        (simplifyIntegralExpr(L1_Division(x + L1_IntegerConstant(const * divs2), L1_IntegerConstant(divs * divs2))), 1L)
      case L1_Addition(ListBuffer(L1_IntegerConstant(const), L1_Division(x, L1_IntegerConstant(divs2)))) if !floor =>
        (simplifyIntegralExpr(L1_Division(x + L1_IntegerConstant(const * divs2), L1_IntegerConstant(divs * divs2))), 1L)

      case L1_FunctionCall(function, ListBuffer(x, L1_IntegerConstant(divs2))) if floor && "floord" == function.name                                                     =>
        (L1_FunctionCall(L1_InternalFunctionReference.floord, ListBuffer(x, L1_IntegerConstant(divs * divs2))), 1L)
      case L1_Addition(ListBuffer(L1_FunctionCall(function, ListBuffer(x, L1_IntegerConstant(divs2))), L1_IntegerConstant(const))) if floor && "floord" == function.name =>
        (simplifyIntegralExpr(L1_FunctionCall(L1_InternalFunctionReference.floord, x + L1_IntegerConstant(const * divs2), L1_IntegerConstant(divs * divs2))), 1L)
      case L1_Addition(ListBuffer(L1_IntegerConstant(const), L1_FunctionCall(function, ListBuffer(x, L1_IntegerConstant(divs2))))) if floor && "floord" == function.name =>
        (simplifyIntegralExpr(L1_FunctionCall(L1_InternalFunctionReference.floord, x + L1_IntegerConstant(const * divs2), L1_IntegerConstant(divs * divs2))), 1L)
      case divd                                                                                                                                                          =>
        if (floor)
          (L1_FunctionCall(L1_InternalFunctionReference.floord, divd, L1_IntegerConstant(divs)), 1L)
        else
          (L1_Division(divd, L1_IntegerConstant(divs)), 1L)
    }
    res(name) = res.getOrElse(name, 0L) + update
    res
  }

  private def extractIntegralSumRec(expr : L1_Expression) : HashMap[L1_Expression, Long] = {

    var res : HashMap[L1_Expression, Long] = null

    expr match {

      case L1_IntegerConstant(i) =>
        res = new HashMap[L1_Expression, Long]()
        res(constName) = i

      case va : L1_VariableAccess =>
        res = new HashMap[L1_Expression, Long]()
        res(va) = 1l // preserve datatype if some

      case L1_Negative(neg) =>
        res = extractIntegralSumRec(neg)
        for ((name : L1_Expression, value : Long) <- res)
          res(name) = -value

      case L1_Addition(summands) =>
        res = new HashMap[L1_Expression, Long]()
        for (s <- summands)
          for ((name : L1_Expression, value : Long) <- extractIntegralSumRec(s))
            res(name) = res.getOrElse(name, 0L) + value
        // opt:  (x/2) + (x%2)  ==>  (x+1)/2
        val toOpt = new HashMap[L1_Expression, (L1_Division, L1_Modulo, Long)]()
        for ((ex, coeff) <- res) ex match {
          case divd @ L1_Division(x, L1_IntegerConstant(2)) =>
            toOpt.get(x) match {
              case None                             => toOpt(x) = (divd, null, coeff)
              case Some((_, modd, c)) if c == coeff => toOpt(x) = (divd, modd, coeff)
              case Some(_)                          => toOpt -= x // coefficient is not matching...
            }
          case modd @ L1_Modulo(x, L1_IntegerConstant(2))   =>
            toOpt.get(x) match {
              case None                             => toOpt(x) = (null, modd, coeff)
              case Some((divd, _, c)) if c == coeff => toOpt(x) = (divd, modd, coeff)
              case Some(_)                          => toOpt -= x // coefficient is not matching...
            }
          case _                                            =>
        }
        for ((x, (div, mod, coeff)) <- toOpt) if (div != null && mod != null) {
          // ensure resulting map only contains normalized version created by recreateExprFromIntSum
          val nju = recreateExprFromIntSum(extractIntegralSumRec((x + L1_IntegerConstant(1L)) / L1_IntegerConstant(2L)))
          res -= div -= mod
          res(nju) = coeff + res.getOrElse(nju, 0L)
        }

      case L1_Subtraction(l, r) =>
        res = extractIntegralSumRec(l)
        for ((name : L1_Expression, value : Long) <- extractIntegralSumRec(r))
          res(name) = res.getOrElse(name, 0L) - value

      case L1_Multiplication(facs) =>
        var coeff : Long = 1L
        val nonCst = new ListBuffer[L1_Expression]()
        var nonCstMap : HashMap[L1_Expression, Long] = null
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
        res = new HashMap[L1_Expression, Long]()
        if (nonCst.isEmpty)
          res(constName) = coeff
        else if (nonCst.length == 1)
          for ((name : L1_Expression, value : Long) <- nonCstMap)
            res(name) = value * coeff
        else
          res(L1_Multiplication(nonCst.sortBy(_.prettyprint()))) = coeff

      case L1_Division(l, r) =>
        res = extractIntegralSumDivision(l, r, false)

      case L1_FunctionCall(function, ListBuffer(l, r)) if "floord" == function.name =>
        res = extractIntegralSumDivision(l, r, true)

      case L1_Modulo(l, r) =>
        val tmp = extractIntegralSumRec(r)
        if (tmp.isEmpty)
          throw EvaluationException("BOOM! (divide by zero)")
        if (!(tmp.size == 1 && tmp.contains(constName)))
          throw EvaluationException("only constant divisor allowed")
        val mod : Long = tmp(constName)
        res = new HashMap[L1_Expression, Long]()
        val dividendMap : HashMap[L1_Expression, Long] = extractIntegralSumRec(l)
        val dividend : L1_Expression = recreateExprFromIntSum(dividendMap)
        dividend match {
          case L1_IntegerConstant(x) => res(constName) = x % mod
          case _                     => res(L1_Modulo(dividend, L1_IntegerConstant(mod))) = 1L
        }

      case L1_Minimum(args : ListBuffer[L1_Expression]) =>
        val exprs = new ListBuffer[L1_Expression]
        var min : java.lang.Long = null
        for (arg <- args) simplifyIntegralExpr(arg) match {
          case L1_IntegerConstant(c)   => min = if (min == null || min > c) c else min
          case e if !exprs.contains(e) => exprs += e
          case _                       => // we already found a (syntactically) identical expression, so skip this one
        }
        res = new HashMap[L1_Expression, Long]()
        if (exprs.isEmpty)
          res(constName) = min
        else {
          if (min != null)
            exprs += L1_IntegerConstant(min)
          res(L1_Minimum(exprs)) = 1L
        }

      case L1_Maximum(args : ListBuffer[L1_Expression]) =>
        val exprs = new ListBuffer[L1_Expression]
        var max : java.lang.Long = null
        for (arg <- args) simplifyIntegralExpr(arg) match {
          case L1_IntegerConstant(c)   => max = if (max == null || max < c) c else max
          case e if !exprs.contains(e) => exprs += e
          case _                       => // we already found a (syntactically) identical expression, so skip this one
        }
        res = new HashMap[L1_Expression, Long]()
        if (exprs.isEmpty)
          res(constName) = max
        else {
          if (max != null)
            exprs += L1_IntegerConstant(max)
          res(L1_Minimum(exprs)) = 1L
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
  def recreateExprFromIntSum(sumMap : HashMap[L1_Expression, Long]) : L1_Expression = {

    val const : Long = sumMap.getOrElse(constName, 0L)

    val sumSeq = sumMap.view.filter(s => s._1 != constName && s._2 != 0L).toSeq.sortWith({
      case ((e1, _), (e2, _)) => e1.prettyprint() < e2.prettyprint()
    })

    if (sumSeq.isEmpty)
      return L1_IntegerConstant(const)

    // use distributive property
    val reverse = new HashMap[Long, (ListBuffer[L1_Expression], ListBuffer[L1_Expression])]()

    def empty = (new ListBuffer[L1_Expression](), new ListBuffer[L1_Expression]())

    for ((njuExpr : L1_Expression, value : Long) <- sumSeq)
      if (value > 0L)
        reverse.getOrElseUpdate(value, empty)._1 += njuExpr
      else
        reverse.getOrElseUpdate(-value, empty)._2 += njuExpr

    val posSums = new ListBuffer[L1_Expression]()
    val negSums = new ListBuffer[L1_Expression]()

    def toExpr(sum : ListBuffer[L1_Expression]) = if (sum.length == 1) sum.head else new L1_Addition(sum)

    for ((value : Long, (pSums : ListBuffer[L1_Expression], nSums : ListBuffer[L1_Expression])) <- reverse.toSeq.sortBy(t => t._1).reverse)
      if (value == 1L) {
        posSums ++= pSums
        negSums ++= nSums
      } else {
        if (pSums.isEmpty)
          negSums += L1_Multiplication(L1_IntegerConstant(value), toExpr(nSums))
        else if (nSums.isEmpty)
          posSums += L1_Multiplication(L1_IntegerConstant(value), toExpr(pSums))
        else
          posSums += L1_Multiplication(L1_IntegerConstant(value), L1_Subtraction(toExpr(pSums), toExpr(nSums)))
      }

    if (const > 0L)
      posSums += L1_IntegerConstant(const)
    else if (const < 0L)
      negSums += L1_IntegerConstant(-const)

    if (posSums.isEmpty)
      L1_Negative(toExpr(negSums))
    else if (negSums.isEmpty)
      toExpr(posSums)
    else
      L1_Subtraction(toExpr(posSums), toExpr(negSums))
  }

  def simplifyIntegralExpr(expr : L1_Expression) : L1_Expression = {
    try {
      val res = L1_ExpressionStatement(recreateExprFromIntSum(extractIntegralSum(expr)))
      L1_GeneralSimplify.doUntilDoneStandalone(res)
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
  def extractFloatingSum(expr : L1_Expression) : HashMap[L1_Expression, Double] = {
    extractFloatingSumRec(expr)
  }

  private def extractFloatingSumRec(expr : L1_Expression) : HashMap[L1_Expression, Double] = {

    var res : HashMap[L1_Expression, Double] = null

    expr match {

      case L1_IntegerConstant(i) =>
        res = new HashMap[L1_Expression, Double]()
        res(constName) = i

      case L1_RealConstant(d) =>
        res = new HashMap[L1_Expression, Double]()
        res(constName) = d

      case va : L1_VariableAccess =>
        res = new HashMap[L1_Expression, Double]()
        res(va) = 1d // preserve datatype if some

      case call : L1_FunctionCall =>
        if (call.name.contains("std::rand")) // HACK
          throw EvaluationException("don't optimize code containing a call to std::rand")

        def simplifyFloatingArgs(pars : Seq[L1_Datatype]) : Unit = {
          call.arguments =
            pars.view.zip(call.arguments).map {
              case (L1_RealDatatype, arg) =>
                simplifyFloatingExpr(arg)
              case (_, arg)               =>
                arg
            }.to[ListBuffer]
        }

        if (L1_MathFunctions.signatures.contains(call.name))
          simplifyFloatingArgs(L1_MathFunctions.signatures(call.name)._1)
        res = new HashMap[L1_Expression, Double]()
        res(call) = 1d

      case L1_Negative(neg) =>
        res = extractFloatingSumRec(neg)
        for ((name : L1_Expression, value : Double) <- res)
          res(name) = -value

      case L1_Addition(summands) =>
        res = new HashMap[L1_Expression, Double]()
        for (s <- summands)
          for ((name : L1_Expression, value : Double) <- extractFloatingSumRec(s))
            res(name) = res.getOrElse(name, 0d) + value

      case L1_Subtraction(l, r) =>
        res = extractFloatingSumRec(l)
        for ((name : L1_Expression, value : Double) <- extractFloatingSumRec(r))
          res(name) = res.getOrElse(name, 0d) - value

      case L1_Multiplication(facs) =>
        var coeff : Double = 1d
        val nonCst = new ListBuffer[HashMap[L1_Expression, Double]]()
        for (f <- facs) {
          val map = extractFloatingSumRec(f)
          if (map.size == 1 && map.contains(constName) || map.isEmpty)
            coeff *= map.getOrElse(constName, 0d)
          else
            nonCst += map
        }
        res = new HashMap[L1_Expression, Double]()
        if (nonCst.isEmpty)
          res(constName) = coeff
        else if (nonCst.length == 1)
          for ((name : L1_Expression, value : Double) <- nonCst.head)
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
          res(L1_Multiplication(nonCst.map { sum => recreateExprFromFloatSum(sum) })) = coeff
        }

      case L1_Division(l, r) =>
        val mapL = extractFloatingSumRec(l)
        val mapR = extractFloatingSumRec(r)
        if (mapR.size == 1 && mapR.contains(constName)) {
          val div : Double = mapR(constName)
          res = mapL
          for ((name : L1_Expression, value : Double) <- res)
            res(name) = value / div
        } else {
          var coefL : Double = 1d
          var exprL = recreateExprFromFloatSum(mapL)
          var coefR : Double = 1d
          var exprR = recreateExprFromFloatSum(mapR)
          exprL match {
            case L1_Multiplication(ListBuffer(L1_RealConstant(coef), inner)) =>
              coefL = coef
              exprL = inner
            case L1_RealConstant(coef)                                       =>
              coefL = coef
              exprL = L1_RealConstant(1d)
            case _                                                           =>
          }
          exprR match {
            case L1_Multiplication(ListBuffer(L1_RealConstant(coef), inner)) =>
              coefR = coef
              exprR = inner
            case L1_RealConstant(coef)                                       =>
              coefR = coef
              exprR = L1_RealConstant(1d)
            case _                                                           =>
          }
          res = new HashMap[L1_Expression, Double]()
          val div = L1_Division(exprL, exprR)
          res(div) = coefL / coefR
        }

      case L1_Modulo(l, r) =>
        val mapR = extractFloatingSumRec(r)
        if (!(mapR.size == 1 && mapR.contains(constName)))
          throw EvaluationException("only constant divisor allowed yet:  " + l.prettyprint() + "  %  " + r.prettyprint())
        val mod : Double = mapR(constName)
        res = extractFloatingSumRec(l)
        for ((name : L1_Expression, value : Double) <- res)
          res(name) = value % mod

      case L1_Minimum(args : ListBuffer[L1_Expression]) =>
        val exprs = new ListBuffer[L1_Expression]
        var min : java.lang.Double = null
        for (arg <- args) simplifyFloatingExpr(arg) match {
          case L1_RealConstant(c)      => min = if (min == null || min > c) c else min
          case e if !exprs.contains(e) => exprs += e
          case _                       => // we already found a (syntactically) identical expression, so skip this one
        }
        res = new HashMap[L1_Expression, Double]()
        if (exprs.isEmpty)
          res(constName) = min
        else {
          if (min != null)
            exprs += L1_RealConstant(min)
          res(L1_Minimum(exprs)) = 1d
        }

      case L1_Maximum(args : ListBuffer[L1_Expression]) =>
        val exprs = new ListBuffer[L1_Expression]
        var max : java.lang.Double = null
        for (arg <- args) simplifyFloatingExpr(arg) match {
          case L1_RealConstant(c)      => max = if (max == null || max < c) c else max
          case e if !exprs.contains(e) => exprs += e
          case _                       => // we already found a (syntactically) identical expression, so skip this one
        }
        res = new HashMap[L1_Expression, Double]()
        if (exprs.isEmpty)
          res(constName) = max
        else {
          if (max != null)
            exprs += L1_RealConstant(max)
          res(L1_Maximum(exprs)) = 1d
        }

      case L1_Power(l, r) =>
        val mapL = extractFloatingSumRec(l)
        val mapR = extractFloatingSumRec(r)
        // trivial solution, if no better one is found later on
        res = new HashMap[L1_Expression, Double]()
        res(L1_Power(recreateExprFromFloatSum(mapL), recreateExprFromFloatSum(mapR))) = 1d
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
              res = extractFloatingSumRec(L1_Multiplication(ListBuffer.fill(expL.toInt)(Duplicate(l))))
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
  def recreateExprFromFloatSum(sumMap : HashMap[L1_Expression, Double]) : L1_Expression = {

    val const : Double = sumMap.getOrElse(constName, 0d)

    val sumSeq = sumMap.view.filter(s => s._1 != constName && s._2 != 0d).toSeq.sortWith({
      case ((e1, _), (e2, _)) => e1.prettyprint() < e2.prettyprint()
    })

    if (sumSeq.isEmpty)
      return L1_RealConstant(const)

    // use distributive property
    val reverse = new HashMap[Double, (ListBuffer[L1_Expression], ListBuffer[L1_Expression])]()

    def empty = (new ListBuffer[L1_Expression](), new ListBuffer[L1_Expression]())

    for ((njuExpr : L1_Expression, value : Double) <- sumSeq)
      if (value > 0d)
        reverse.getOrElseUpdate(value, empty)._1 += njuExpr
      else
        reverse.getOrElseUpdate(-value, empty)._2 += njuExpr

    val posSums = new ListBuffer[L1_Expression]()
    val negSums = new ListBuffer[L1_Expression]()

    def toExpr(sum : ListBuffer[L1_Expression]) = if (sum.length == 1) sum.head else new L1_Addition(sum)

    for ((value : Double, (pSums : ListBuffer[L1_Expression], nSums : ListBuffer[L1_Expression])) <- reverse.toSeq.sortBy(t => t._1).reverse)
      if (value == 1d) {
        posSums ++= pSums
        negSums ++= nSums
      } else {
        if (pSums.isEmpty)
          negSums += L1_Multiplication(L1_RealConstant(value), toExpr(nSums))
        else if (nSums.isEmpty)
          posSums += L1_Multiplication(L1_RealConstant(value), toExpr(pSums))
        else
          posSums += L1_Multiplication(L1_RealConstant(value), L1_Subtraction(toExpr(pSums), toExpr(nSums)))
      }

    if (const > 0d)
      posSums += L1_RealConstant(const)
    else if (const < 0d)
      negSums += L1_RealConstant(-const)

    if (posSums.isEmpty)
      L1_Negative(toExpr(negSums))
    else if (negSums.isEmpty)
      toExpr(posSums)
    else
      L1_Subtraction(toExpr(posSums), toExpr(negSums))
  }

  def simplifyFloatingExpr(expr : L1_Expression) : L1_Expression = {
    try {
      val res = L1_ExpressionStatement(recreateExprFromFloatSum(extractFloatingSum(expr)))
      L1_GeneralSimplify.doUntilDoneStandalone(res)
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
