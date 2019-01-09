package exastencils.optimization.l4

import scala.collection._
import scala.collection.mutable.{ HashMap, ListBuffer }

import exastencils.base.l4._
import exastencils.core._
import exastencils.util.MathHelper
import exastencils.util.l4.L4_MathFunctions

/// EvaluationException

/** Exception used in L4_SimplifyExpression to signal that the given expression cannot be processed/evaluated. */
case class EvaluationException(msg : String, cause : Throwable = null) extends Exception(msg, cause) {}

/// L4_SimplifyExpression

object L4_SimplifyExpression {

  /**
    * Completely evaluates an integral expression.
    * Only IntegerConstants are allowed!
    * Other scalar constants or variable accesses lead to an EvaluationException.
    */
  def evalIntegral(expr : L4_Expression) : Long = expr match {
    case L4_IntegerConstant(v)                                => v
    case L4_Addition(sums : ListBuffer[L4_Expression])        => sums.view.map(s => evalIntegral(s)).sum
    case L4_Subtraction(l : L4_Expression, r : L4_Expression) => evalIntegral(l) - evalIntegral(r)
    case L4_Multiplication(facs : ListBuffer[L4_Expression])  => facs.view.map(s => evalIntegral(s)).product
    case L4_Division(l : L4_Expression, r : L4_Expression)    => evalIntegral(l) / evalIntegral(r)
    case L4_Modulo(l : L4_Expression, r : L4_Expression)      => evalIntegral(l) % evalIntegral(r)
    case L4_Minimum(l : ListBuffer[L4_Expression])            => l.view.map(e => evalIntegral(e)).min
    case L4_Maximum(l : ListBuffer[L4_Expression])            => l.view.map(e => evalIntegral(e)).max
    case _                                                    =>
      throw EvaluationException("unknown expression type for evaluation: " + expr.getClass)
  }

  final val EXTREMA_MAP : String = "extremaMap" // associated value must be HashMap[String, (Long,Long)]

  var extremaMap : mutable.HashMap[String, (Long, Long)] = null

  def evalIntegralExtrema(expr : L4_Expression) : (Long, Long) = {
    evalIntegralExtrema(expr, mutable.Map[String, (Long, Long)]())
  }

  /**
    * Completely evaluates an integral expression and computes a lower bound and an upper bound
    * for its value depending on the minOffset and maxOffset in potential OffsetIndex nodes.
    * Only IntegerConstants are allowed! (Except for the offset field in OffsetIndex, which is not evaluated at all.)
    * Other scalar constants or variable accesses lead to an EvaluationException.
    */
  def evalIntegralExtrema(expr : L4_Expression, extremaLookup : Map[String, (Long, Long)]) : (Long, Long) = expr match {
    case L4_IntegerConstant(v) =>
      (v, v)

    case L4_StringConstant(value) if extremaLookup.contains(value) =>
      extremaLookup(value)

    case L4_StringLiteral(value) if extremaLookup.contains(value) =>
      extremaLookup(value)

    case va : L4_VariableAccess if extremaLookup.contains(va.name) =>
      extremaLookup(va.name)

    case L4_Addition(sums : ListBuffer[L4_Expression]) =>
      sums.view.map(s => evalIntegralExtrema(s, extremaLookup)).reduce { (x, y) =>
        (x._1 + y._1, x._2 + y._2)
      }

    case L4_Subtraction(l : L4_Expression, r : L4_Expression) =>
      val x = evalIntegralExtrema(l, extremaLookup)
      val y = evalIntegralExtrema(r, extremaLookup)
      (x._1 - y._2, x._2 - y._1)

    case L4_Multiplication(facs : ListBuffer[L4_Expression]) =>
      facs.view.map(s => evalIntegralExtrema(s, extremaLookup)).reduce { (x, y) =>
        val a = x._1 * y._1
        val b = x._1 * y._2
        val c = x._2 * y._1
        val d = x._2 * y._2
        (a min b min c min d, a max b max c max d)
      }

    case L4_Division(l : L4_Expression, r : L4_Expression) =>
      val x = evalIntegralExtrema(l, extremaLookup)
      val y = evalIntegralExtrema(r, extremaLookup)
      val a = x._1 / y._1
      val b = x._1 / y._2
      val c = x._2 / y._1
      val d = x._2 / y._2
      (a min b min c min d, a max b max c max d)

    case L4_Modulo(_ : L4_Expression, L4_IntegerConstant(den)) =>
      (0, den - 1)

    // may lead to incorrect results (e.g. for x % 2 and x has extrema 0 and 2)
    //    case L4_Modulo(l : L4_Expression, r : L4_Expression) =>
    //      val x = evalIntegralExtrema(l, extremaLookup)
    //      val y = evalIntegralExtrema(r, extremaLookup)
    //      val a = x._1 % y._1
    //      val b = x._1 % y._2
    //      val c = x._2 % y._1
    //      val d = x._2 % y._2
    //      (a min b min c min d, a max b max c max d)

    case L4_Minimum(l : ListBuffer[L4_Expression]) =>
      l.view.map(e => evalIntegralExtrema(e, extremaLookup)).reduce { (x, y) =>
        (x._1 min y._1, x._2 min y._2)
      }

    case L4_Maximum(l : ListBuffer[L4_Expression]) =>
      l.view.map(e => evalIntegralExtrema(e, extremaLookup)).reduce { (x, y) =>
        (x._1 max y._1, x._2 max y._2)
      }

    case L4_Negative(left : L4_Expression) =>
      val (min, max) = evalIntegralExtrema(left, extremaLookup)
      (-max, -min)

    case L4_FunctionCall(function, ListBuffer(l : L4_Expression, r : L4_Expression)) if "floord" == function.name =>
      evalIntegralExtrema(L4_Division(l, r), extremaLookup)

    case _ =>
      throw EvaluationException("unknown expression type for evaluation: " + expr.getClass)
  }

  /**
    * Completely evaluates an floating expression.
    * Only FloatConstants are allowed!
    * Other scalar constants or variable accesses lead to an EvaluationException.
    */
  def evalFloating(expr : L4_Expression) : Double = expr match {
    case L4_RealConstant(v)                                   => v
    case L4_Addition(sums : ListBuffer[L4_Expression])        => sums.view.map(s => evalFloating(s)).sum
    case L4_Subtraction(l : L4_Expression, r : L4_Expression) => evalFloating(l) - evalFloating(r)
    case L4_Multiplication(facs : ListBuffer[L4_Expression])  => facs.view.map(s => evalFloating(s)).product
    case L4_Division(l : L4_Expression, r : L4_Expression)    => evalFloating(l) / evalFloating(r)
    case L4_Minimum(l : ListBuffer[L4_Expression])            => l.view.map(e => evalFloating(e)).min
    case L4_Maximum(l : ListBuffer[L4_Expression])            => l.view.map(e => evalFloating(e)).max
    case _                                                    =>
      throw EvaluationException("unknown expression type for evaluation: " + expr.getClass)
  }

  /**
    * Constant string that is used to hold the additive constant of an affine expression in the result map of
    * evalIntegralAffine(Expression).
    */
  final val constName : L4_Expression = L4_NullExpression

  /**
    * Evaluates and (implicitly) simplifies an integral expression.
    * No float or boolean constants are allowed.
    * (Otherwise an EvaluationException is thrown.)
    *
    * Returns a map from all present summands to their corresponding coefficients.
    * The additive constant is stored beside the key SimplifyExpression.constName.
    * The given expression is equivalent to: map(constName) + \sum_{n \in names} map(n) * n
    *
    * Only VariableAccess nodes are used as keys. (NO StringLiteral)
    */
  def extractIntegralSum(expr : L4_Expression) : HashMap[L4_Expression, Long] = {

    var res : HashMap[L4_Expression, Long] = null

    expr match {

      case L4_IntegerConstant(i) =>
        res = new HashMap[L4_Expression, Long]()
        res(constName) = i

      case L4_PlainVariableAccess(varName, _, isConst) =>
        res = new HashMap[L4_Expression, Long]()
        res(L4_PlainVariableAccess(varName, L4_IntegerDatatype, isConst)) = 1L

      case L4_LeveledVariableAccess(varName, level, _, isConst) =>
        res = new HashMap[L4_Expression, Long]()
        res(L4_LeveledVariableAccess(varName, level, L4_IntegerDatatype, isConst)) = 1L

      case L4_StringLiteral(varName) =>
        res = new HashMap[L4_Expression, Long]()
        res(L4_PlainVariableAccess(varName, L4_IntegerDatatype, false)) = 1L // ONLY VariableAccess in res keys, NO StringLiteral

      case L4_Negative(neg) =>
        res = extractIntegralSum(neg)
        for ((name : L4_Expression, value : Long) <- res)
          res(name) = -value

      case L4_Addition(summands) =>
        res = new HashMap[L4_Expression, Long]()
        for (s <- summands)
          for ((name : L4_Expression, value : Long) <- extractIntegralSum(s))
            res(name) = res.getOrElse(name, 0L) + value
        // opt:  (x/2) + (x%2)  ==>  (x+1)/2
        val toOpt = new HashMap[L4_Expression, (L4_Division, L4_Modulo, Long)]()
        for ((ex, coeff) <- res) ex match {
          case divd @ L4_Division(x, L4_IntegerConstant(2)) =>
            toOpt.get(x) match {
              case None                             => toOpt(x) = (divd, null, coeff)
              case Some((_, modd, c)) if c == coeff => toOpt(x) = (divd, modd, coeff)
              case Some(_)                          => toOpt -= x // coefficient is not matching...
            }
          case modd @ L4_Modulo(x, L4_IntegerConstant(2))   =>
            toOpt.get(x) match {
              case None                             => toOpt(x) = (null, modd, coeff)
              case Some((divd, _, c)) if c == coeff => toOpt(x) = (divd, modd, coeff)
              case Some(_)                          => toOpt -= x // coefficient is not matching...
            }
          case _                                            =>
        }
        for ((x, (div, mod, coeff)) <- toOpt) if (div != null && mod != null) {
          // ensure resulting map only contains normalized version created by recreateExprFromIntSum
          val nju = recreateExprFromIntSum(extractIntegralSum((x + L4_IntegerConstant(1L)) / L4_IntegerConstant(2L)))
          res -= div -= mod
          res(nju) = coeff + res.getOrElse(nju, 0L)
        }

      case L4_Subtraction(l, r) =>
        res = extractIntegralSum(l)
        for ((name : L4_Expression, value : Long) <- extractIntegralSum(r))
          res(name) = res.getOrElse(name, 0L) - value

      case L4_Multiplication(facs) =>
        var coeff : Long = 1L
        val nonCst = new ListBuffer[HashMap[L4_Expression, Long]]()
        for (f <- facs) {
          val map = extractIntegralSum(f)
          if (map.size == 1 && map.contains(constName) || map.isEmpty) {
            coeff *= map.getOrElse(constName, 0L)
          } else {
            var gcdL : Long = math.abs(map.head._2)
            for ((_, c) <- map)
              gcdL = MathHelper.gcd(c, gcdL)
            for ((e, c) <- map)
              map(e) = c / gcdL
            coeff *= gcdL
            nonCst += map
          }
        }
        res = new HashMap[L4_Expression, Long]()
        if (nonCst.isEmpty)
          res(constName) = coeff
        else if (nonCst.length == 1)
          for ((name : L4_Expression, value : Long) <- nonCst.head)
            res(name) = value * coeff
        else
          res(L4_Multiplication(nonCst.map(recreateExprFromIntSum).sortBy(_.prettyprint()))) = coeff

      case L4_Division(l, r) =>
        res = extractIntegralSumDivision(l, r, false)

      case L4_FunctionCall(function, ListBuffer(l, r)) if "floord" == function.name =>
        res = extractIntegralSumDivision(l, r, true)

      case L4_Modulo(l, r) =>
        val tmp = extractIntegralSum(r)
        if (tmp.isEmpty)
          throw EvaluationException("BOOM! (divide by zero)")
        if (!(tmp.size == 1 && tmp.contains(constName)))
          throw EvaluationException("only constant divisor allowed")
        val mod : Long = tmp(constName)
        res = new HashMap[L4_Expression, Long]()
        val dividendMap : HashMap[L4_Expression, Long] = extractIntegralSum(l)
        val dividend : L4_Expression = recreateExprFromIntSum(dividendMap)
        dividend match {
          case L4_IntegerConstant(x) => res(constName) = x % mod
          case _                     => res(L4_Modulo(dividend, L4_IntegerConstant(mod))) = 1L
        }

      case L4_Minimum(args : ListBuffer[L4_Expression]) =>
        val exprs = new ListBuffer[L4_Expression]
        var min : java.lang.Long = null
        for (arg <- args) simplifyIntegralExpr(arg) match {
          case L4_IntegerConstant(c) => min = if (min == null || min > c) c else min
          case e                     => if (!exprs.contains(e)) exprs += e
        }
        res = new HashMap[L4_Expression, Long]()
        if (exprs.isEmpty)
          res(constName) = min
        else {
          if (min != null)
            exprs += L4_IntegerConstant(min)
          res(L4_Minimum(exprs)) = 1L
        }

      case L4_Maximum(args : ListBuffer[L4_Expression]) =>
        val exprs = new ListBuffer[L4_Expression]
        var max : java.lang.Long = null
        for (arg <- args) simplifyIntegralExpr(arg) match {
          case L4_IntegerConstant(c) => max = if (max == null || max < c) c else max
          case e                     => if (!exprs.contains(e)) exprs += e
        }
        res = new HashMap[L4_Expression, Long]()
        if (exprs.isEmpty)
          res(constName) = max
        else {
          if (max != null)
            exprs += L4_IntegerConstant(max)
          res(L4_Maximum(exprs)) = 1L
        }

      case _ =>
        throw EvaluationException("unknown expression type for evaluation: " + expr.getClass)
    }

    res.filter(e => e._2 != 0L)
  }

  private def extractIntegralSumDivision(l : L4_Expression, r : L4_Expression, floor : Boolean) : HashMap[L4_Expression, Long] = {
    var tmp = extractIntegralSum(r)
    if (tmp.isEmpty)
      throw EvaluationException("BOOM! (divide by zero)")
    if (!(tmp.size == 1 && tmp.contains(constName)))
      throw EvaluationException("only constant divisor allowed yet")
    val divs : Long = tmp(constName)
    tmp.clear()
    // optimization below is allowed if division is floord, or if the sign of l does not change, both before and after the optimization
    var changeSign : Boolean = true
    var sign : Long = 0
    if (extremaMap != null) {
      try {
        val (lo : Long, up : Long) = evalIntegralExtrema(l, extremaMap)
        changeSign = lo < 0 && up > 0
        sign = if (lo != 0) lo else up // only the sign is relevant, ignore 0
      } catch {
        case _ : EvaluationException =>
      }
    }
    val res = new HashMap[L4_Expression, Long]()
    val mapL = extractIntegralSum(l)
    if (floor || !changeSign) { // do only remove parts of the dividend when the rounding direction is "uniform" (truncate rounds towards 0)
      for ((name : L4_Expression, value : Long) <- mapL)
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
      // check if optimization was allowed (sign of new min and max is identical, ignoring 0)
      if (!floor)
        try {
          val (lo : Long, up : Long) = evalIntegralExtrema(recreateExprFromIntSum(tmp), extremaMap)
          val newSign : Long = if (lo != 0) lo else up // only the sign is relevant, ignore 0
          if (lo < 0 && up > 0) { // optimization was NOT allowed, revert
            tmp = mapL
            res.clear()
            // check if optimization moved range from positive to negative, or vice versa: if so, adapt rounding direction
          } else if (sign < 0 && newSign > 0) // previous rounding direction: towards +inf
            tmp(constName) = tmp.getOrElse(constName, 0L) + (divs - 1)
          else if (newSign < 0 && sign > 0) // previous rounding direction: towards -inf
            tmp(constName) = tmp.getOrElse(constName, 0L) - (divs - 1)
        } catch {
          case _ : EvaluationException =>
        }
    } else
      tmp = mapL
    val dividend = recreateExprFromIntSum(tmp)
    val (name, update) : (L4_Expression, Long) = dividend match {
      case L4_IntegerConstant(x) =>
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

      case L4_Division(x, L4_IntegerConstant(divs2)) if !floor                                                     =>
        (L4_Division(x, L4_IntegerConstant(divs * divs2)), 1L)
      case L4_Addition(ListBuffer(L4_Division(x, L4_IntegerConstant(divs2)), L4_IntegerConstant(const))) if !floor =>
        (simplifyIntegralExpr(L4_Division(x + L4_IntegerConstant(const * divs2), L4_IntegerConstant(divs * divs2))), 1L)
      case L4_Addition(ListBuffer(L4_IntegerConstant(const), L4_Division(x, L4_IntegerConstant(divs2)))) if !floor =>
        (simplifyIntegralExpr(L4_Division(x + L4_IntegerConstant(const * divs2), L4_IntegerConstant(divs * divs2))), 1L)

      case L4_FunctionCall(function, ListBuffer(x, L4_IntegerConstant(divs2))) if floor && "floord" == function.name                                                     =>
        (L4_FunctionCall(L4_InternalFunctionReference.floord, ListBuffer(x, L4_IntegerConstant(divs * divs2))), 1L)
      case L4_Addition(ListBuffer(L4_FunctionCall(function, ListBuffer(x, L4_IntegerConstant(divs2))), L4_IntegerConstant(const))) if floor && "floord" == function.name =>
        (simplifyIntegralExpr(L4_FunctionCall(L4_InternalFunctionReference.floord, x + L4_IntegerConstant(const * divs2), L4_IntegerConstant(divs * divs2))), 1L)
      case L4_Addition(ListBuffer(L4_IntegerConstant(const), L4_FunctionCall(function, ListBuffer(x, L4_IntegerConstant(divs2))))) if floor && "floord" == function.name =>
        (simplifyIntegralExpr(L4_FunctionCall(L4_InternalFunctionReference.floord, x + L4_IntegerConstant(const * divs2), L4_IntegerConstant(divs * divs2))), 1L)
      case divd                                                                                                                                                          =>
        if (floor)
          (L4_FunctionCall(L4_InternalFunctionReference.floord, divd, L4_IntegerConstant(divs)), 1L)
        else
          (L4_Division(divd, L4_IntegerConstant(divs)), 1L)
    }
    res(name) = res.getOrElse(name, 0L) + update
    res
  }

  /**
    * Takes the output from extractIntegralSum(..) and recreates an AST for this sum.
    */
  def recreateExprFromIntSum(sumMap : HashMap[L4_Expression, Long]) : L4_Expression = {

    val const : Long = sumMap.getOrElse(constName, 0L)

    val sumSeq = sumMap.view.filter(s => s._1 != constName && s._2 != 0L).toSeq.sortWith({
      case ((v1 : L4_VariableAccess, _), (v2 : L4_VariableAccess, _)) => v1.name < v2.name
      case ((_ : L4_VariableAccess, _), _)                                => true
      case (_, (_ : L4_VariableAccess, _))                                => false
      case ((e1, _), (e2, _))                                             =>
        val (e1PP, e2PP) = (e1.prettyprint(), e2.prettyprint())
        if (e1PP == e2PP) {
          // Logger.warn(e1PP)
          e1.toString < e2.toString
        } else {
          e1PP < e2PP
        }
    })

    if (sumSeq.isEmpty)
      return L4_IntegerConstant(const)

    // use distributive property
    val reverse = new HashMap[Long, (ListBuffer[L4_Expression], ListBuffer[L4_Expression])]()

    def empty = (new ListBuffer[L4_Expression](), new ListBuffer[L4_Expression]())

    for ((njuExpr : L4_Expression, value : Long) <- sumSeq)
      if (value > 0L)
        reverse.getOrElseUpdate(value, empty)._1 += njuExpr
      else
        reverse.getOrElseUpdate(-value, empty)._2 += njuExpr

    val posSums = new ListBuffer[L4_Expression]()
    val negSums = new ListBuffer[L4_Expression]()

    def toExpr(sum : ListBuffer[L4_Expression]) = if (sum.length == 1) sum.head else new L4_Addition(sum)

    for ((value : Long, (pSums : ListBuffer[L4_Expression], nSums : ListBuffer[L4_Expression])) <- reverse.toSeq.sortBy(t => t._1).reverse)
      if (value == 1L) {
        posSums ++= pSums
        negSums ++= nSums
      } else {
        if (pSums.isEmpty)
          negSums += L4_Multiplication(L4_IntegerConstant(value), toExpr(nSums))
        else if (nSums.isEmpty)
          posSums += L4_Multiplication(L4_IntegerConstant(value), toExpr(pSums))
        else
          posSums += L4_Multiplication(L4_IntegerConstant(value), L4_Subtraction(toExpr(pSums), toExpr(nSums)))
      }

    if (const > 0L)
      posSums += L4_IntegerConstant(const)
    else if (const < 0L)
      negSums += L4_IntegerConstant(-const)

    if (posSums.isEmpty)
      L4_Negative(toExpr(negSums))
    else if (negSums.isEmpty)
      toExpr(posSums)
    else
      L4_Subtraction(toExpr(posSums), toExpr(negSums))
  }

  def simplifyIntegralExpr(expr : L4_Expression, extrMap : mutable.HashMap[String, (Long, Long)] = null) : L4_Expression = {
    extremaMap = extrMap
    try {
      val res = L4_ExpressionStatement(recreateExprFromIntSum(extractIntegralSum(expr)))
      extremaMap = null
      L4_GeneralSimplify.doUntilDoneStandalone(res)
      res.expression
    } catch {
      case ex : EvaluationException =>
        extremaMap = null
        throw EvaluationException(ex.msg + ";  in " + expr.prettyprint(), ex)
    }
  }

  /**
    * Evaluates and (implicitly) simplifies a floating-point expression.
    * No boolean constants are allowed.
    * (Otherwise an EvaluationException is thrown.)
    *
    * Returns a map from all present summands to their corresponding coefficients.
    * The additive constant is stored beside the key SimplifyExpression.constName.
    * The given expression is equivalent to: map(constName) + \sum_{n \in names} map(n) * n
    *
    * Only VariableAccess and ArrayAccess nodes are used as keys. (NO StringLiteral)
    */
  def extractFloatingSum(expr : L4_Expression) : HashMap[L4_Expression, Double] = {

    var res : HashMap[L4_Expression, Double] = null

    expr match {

      case L4_IntegerConstant(i) =>
        res = new HashMap[L4_Expression, Double]()
        res(constName) = i

      case L4_RealConstant(d) =>
        res = new HashMap[L4_Expression, Double]()
        res(constName) = d

      case va : L4_VariableAccess =>
        res = new HashMap[L4_Expression, Double]()
        res(va) = 1d

      case L4_StringLiteral(varName) =>
        if (varName.contains("std::rand")) // HACK
          throw EvaluationException("don't optimze code containing a call to std::rand")
        res = new HashMap[L4_Expression, Double]()
        res(L4_PlainVariableAccess(varName, L4_RealDatatype, false)) = 1d // ONLY VariableAccess in res keys, NO StringLiteral

      case call : L4_FunctionCall =>
        if (call.name.contains("std::rand")) // HACK
          throw EvaluationException("don't optimize code containing a call to std::rand")

        def simplifyFloatingArgs(pars : Seq[L4_Datatype]) : Unit = {
          call.arguments =
            pars.view.zip(call.arguments).map {
              case (L4_RealDatatype, arg) =>
                simplifyFloatingExpr(arg)
              case (_, arg)               =>
                arg
            }.to[ListBuffer]
        }

        if (L4_MathFunctions.signatures.contains(call.name))
          simplifyFloatingArgs(L4_MathFunctions.signatures(call.name)._1)
        else for (func <- StateManager.findFirst({ f : L4_Function => f.name == call.name }))
          simplifyFloatingArgs(func.parameters.view.map(_.datatype))
        res = new HashMap[L4_Expression, Double]()
        res(call) = 1d

      case L4_Negative(neg) =>
        res = extractFloatingSum(neg)
        for ((name : L4_Expression, value : Double) <- res)
          res(name) = -value

      case L4_Addition(summands) =>
        res = new HashMap[L4_Expression, Double]()
        for (s <- summands)
          for ((name : L4_Expression, value : Double) <- extractFloatingSum(s))
            res(name) = res.getOrElse(name, 0d) + value

      case L4_Subtraction(l, r) =>
        res = extractFloatingSum(l)
        for ((name : L4_Expression, value : Double) <- extractFloatingSum(r))
          res(name) = res.getOrElse(name, 0d) - value

      case L4_Multiplication(facs) =>
        var coeff : Double = 1d
        val nonCst = new ListBuffer[HashMap[L4_Expression, Double]]()
        for (f <- facs) {
          val map = extractFloatingSum(f)
          if (map.size == 1 && map.contains(constName) || map.isEmpty)
            coeff *= map.getOrElse(constName, 0d)
          else
            nonCst += map
        }
        res = new HashMap[L4_Expression, Double]()
        if (nonCst.isEmpty)
          res(constName) = coeff
        else if (nonCst.length == 1)
          for ((name : L4_Expression, value : Double) <- nonCst.head)
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
              nC.transform((_, d) => d / v)
            }
          }
          res(L4_Multiplication(nonCst.map(recreateExprFromFloatSum).sortBy(_.prettyprint()))) = coeff
        }

      case L4_Division(l, r) =>
        val mapL = extractFloatingSum(l)
        val mapR = extractFloatingSum(r)
        if (mapR.size == 1 && mapR.contains(constName)) {
          val div : Double = mapR(constName)
          res = mapL.transform { (_ : L4_Expression, value : Double) => value / div }
        } else {
          var coefL : Double = 1d
          var exprL = recreateExprFromFloatSum(mapL)
          var coefR : Double = 1d
          var exprR = recreateExprFromFloatSum(mapR)
          exprL match {
            case L4_Multiplication(ListBuffer(L4_RealConstant(coef), inner)) =>
              coefL = coef
              exprL = inner
            case L4_RealConstant(coef)                                       =>
              coefL = coef
              exprL = L4_RealConstant(1d)
            case _                                                           =>
          }
          exprR match {
            case L4_Multiplication(ListBuffer(L4_RealConstant(coef), inner)) =>
              coefR = coef
              exprR = inner
            // case L4_RealConstant(coef) => // cannot happen, see other branch of surrounding if
            case _ =>
          }
          res = new HashMap[L4_Expression, Double]()
          val div = L4_Division(exprL, exprR)
          res(div) = coefL / coefR
        }

      case L4_Modulo(l, r) =>
        val mapR = extractFloatingSum(r)
        if (!(mapR.size == 1 && mapR.contains(constName)))
          throw EvaluationException("only constant divisor allowed yet:  " + l.prettyprint() + "  %  " + r.prettyprint())
        val mod : Double = mapR(constName)
        res = extractFloatingSum(l)
        for ((name : L4_Expression, value : Double) <- res)
          res(name) = value % mod

      case L4_Minimum(args : ListBuffer[L4_Expression]) =>
        val exprs = new ListBuffer[L4_Expression]
        var min : java.lang.Double = null
        for (arg <- args) simplifyFloatingExpr(arg) match {
          case L4_RealConstant(c) => min = if (min == null || min > c) c else min
          case e                  => if (!exprs.contains(e)) exprs += e
        }
        res = new HashMap[L4_Expression, Double]()
        if (exprs.isEmpty)
          res(constName) = min
        else {
          if (min != null)
            exprs += L4_RealConstant(min)
          res(L4_Minimum(exprs)) = 1d
        }

      case L4_Maximum(args : ListBuffer[L4_Expression]) =>
        val exprs = new ListBuffer[L4_Expression]
        var max : java.lang.Double = null
        for (arg <- args) simplifyFloatingExpr(arg) match {
          case L4_RealConstant(c) => max = if (max == null || max < c) c else max
          case e                  => if (!exprs.contains(e)) exprs += e
        }
        res = new HashMap[L4_Expression, Double]()
        if (exprs.isEmpty)
          res(constName) = max
        else {
          if (max != null)
            exprs += L4_RealConstant(max)
          res(L4_Maximum(exprs)) = 1d
        }

      case L4_Power(l, r) =>
        val mapL = extractFloatingSum(l)
        val mapR = extractFloatingSum(r)
        val isLCst = mapL.size == 1 && mapL.contains(constName)
        val isRCst = mapR.size == 1 && mapR.contains(constName)
        if (isLCst && isRCst) {
          res = HashMap(constName -> math.pow(mapL(constName), mapR(constName)))
        } else if (isRCst) {
          val exp : Double = mapR(constName)
          val expI : Int = exp.toInt
          if (expI.toDouble == exp) expI match {
            case 0                           => res = HashMap(constName -> 1d)
            case 1                           => res = mapL
            case _ if expI >= 2 && expI <= 6 =>
              res = extractFloatingSum(L4_Multiplication(ListBuffer.fill(expI)(Duplicate(l))))
            case _                           =>
          }
        }
        if (res == null) // no better solution found, use trivial one
          res = HashMap(L4_Power(recreateExprFromFloatSum(mapL), recreateExprFromFloatSum(mapR)) -> 1d)

      case _ =>
        throw EvaluationException("unknown expression type for evaluation: " + expr.getClass + " in " + expr.prettyprint())
    }

    res.filter(e => e._2 != 0d)
  }

  /**
    * Takes the output from extractFloatingSum(..) and recreates an AST for this sum.
    */
  def recreateExprFromFloatSum(sumMap : HashMap[L4_Expression, Double]) : L4_Expression = {

    val const : Double = sumMap.getOrElse(constName, 0d)

    val sumSeq = sumMap.view.filter(s => s._1 != constName && s._2 != 0d).toSeq.sortWith({
      case ((v1 : L4_VariableAccess, _), (v2 : L4_VariableAccess, _)) => v1.name < v2.name
      case ((_ : L4_VariableAccess, _), _)                                => true
      case (_, (_ : L4_VariableAccess, _))                                => false
      case ((e1, _), (e2, _))                                             =>
        val (e1PP, e2PP) = (e1.prettyprint(), e2.prettyprint())
        if (e1PP == e2PP) {
          // Logger.warn(e1PP)
          e1.toString < e2.toString
        } else {
          e1PP < e2PP
        }
    })

    if (sumSeq.isEmpty)
      return L4_RealConstant(const)

    // use distributive property
    val reverse = new HashMap[Double, (ListBuffer[L4_Expression], ListBuffer[L4_Expression])]()

    def empty = (new ListBuffer[L4_Expression](), new ListBuffer[L4_Expression]())

    for ((njuExpr : L4_Expression, value : Double) <- sumSeq)
      if (value > 0d)
        reverse.getOrElseUpdate(value, empty)._1 += njuExpr
      else
        reverse.getOrElseUpdate(-value, empty)._2 += njuExpr

    val posSums = new ListBuffer[L4_Expression]()
    val negSums = new ListBuffer[L4_Expression]()

    def toExpr(sum : ListBuffer[L4_Expression]) = if (sum.length == 1) sum.head else new L4_Addition(sum)

    for ((value : Double, (pSums : ListBuffer[L4_Expression], nSums : ListBuffer[L4_Expression])) <- reverse.toSeq.sortBy(t => t._1).reverse)
      if (value == 1d) {
        posSums ++= pSums
        negSums ++= nSums
      } else {
        if (pSums.isEmpty)
          negSums += L4_Multiplication(L4_RealConstant(value), toExpr(nSums))
        else if (nSums.isEmpty)
          posSums += L4_Multiplication(L4_RealConstant(value), toExpr(pSums))
        else
          posSums += L4_Multiplication(L4_RealConstant(value), L4_Subtraction(toExpr(pSums), toExpr(nSums)))
      }

    if (const > 0d)
      posSums += L4_RealConstant(const)
    else if (const < 0d)
      negSums += L4_RealConstant(-const)

    if (posSums.isEmpty)
      L4_Negative(toExpr(negSums))
    else if (negSums.isEmpty)
      toExpr(posSums)
    else
      L4_Subtraction(toExpr(posSums), toExpr(negSums))
  }

  def simplifyFloatingExpr(expr : L4_Expression) : L4_Expression = {
    try {
      val res = L4_ExpressionStatement(recreateExprFromFloatSum(extractFloatingSum(expr)))
      L4_GeneralSimplify.doUntilDoneStandalone(res)
      res.expression
    } catch {
      case ex : EvaluationException =>
        throw EvaluationException(ex.msg + ";  in " + expr.prettyprint(), ex)
    }
  }
}
