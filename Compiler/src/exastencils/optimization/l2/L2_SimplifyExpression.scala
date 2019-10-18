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

package exastencils.optimization.l2

import scala.collection._
import scala.collection.mutable.{ HashMap, ListBuffer }

import exastencils.base.l2._
import exastencils.baseExt.l2._
import exastencils.core._
import exastencils.datastructures._
import exastencils.util.MathHelper
import exastencils.util.l2.L2_MathFunctions

/// EvaluationException

/** Exception used in L2_SimplifyExpression to signal that the given expression cannot be processed/evaluated. */
case class EvaluationException(msg : String, cause : Throwable = null) extends Exception(msg, cause) {}

/// L2_SimplifyExpression

object L2_SimplifyExpression {

  /**
    * Completely evaluates an integral expression.
    * Only IntegerConstants are allowed!
    * Other scalar constants or variable accesses lead to an EvaluationException.
    */
  def evalIntegral(expr : L2_Expression) : Long = expr match {
    case L2_IntegerConstant(v)                                => v
    case L2_Addition(sums : ListBuffer[L2_Expression])        => sums.view.map(s => evalIntegral(s)).sum
    case L2_Subtraction(l : L2_Expression, r : L2_Expression) => evalIntegral(l) - evalIntegral(r)
    case L2_Multiplication(facs : ListBuffer[L2_Expression])  => facs.view.map(s => evalIntegral(s)).product
    case L2_Division(l : L2_Expression, r : L2_Expression)    => evalIntegral(l) / evalIntegral(r)
    case L2_Modulo(l : L2_Expression, r : L2_Expression)      => evalIntegral(l) % evalIntegral(r)
    case L2_Minimum(l : ListBuffer[L2_Expression])            => l.view.map(e => evalIntegral(e)).min
    case L2_Maximum(l : ListBuffer[L2_Expression])            => l.view.map(e => evalIntegral(e)).max
    case _                                                    =>
      throw EvaluationException("unknown expression type for evaluation: " + expr.getClass)
  }

  final val EXTREMA_MAP : String = "extremaMap" // associated value must be HashMap[String, (Long,Long)]

  var extremaMap : mutable.HashMap[String, (Long, Long)] = null

  def evalIntegralExtrema(expr : L2_Expression) : (Long, Long) = {
    evalIntegralExtrema(expr, mutable.Map[String, (Long, Long)]())
  }

  /**
    * Completely evaluates an integral expression and computes a lower bound and an upper bound
    * for its value depending on the minOffset and maxOffset in potential OffsetIndex nodes.
    * Only IntegerConstants are allowed! (Except for the offset field in OffsetIndex, which is not evaluated at all.)
    * Other scalar constants or variable accesses lead to an EvaluationException.
    */
  def evalIntegralExtrema(expr : L2_Expression, extremaLookup : Map[String, (Long, Long)]) : (Long, Long) = expr match {
    case L2_IntegerConstant(v) =>
      (v, v)

    case L2_StringConstant(value) if extremaLookup.contains(value) =>
      extremaLookup(value)

    case L2_StringLiteral(value) if extremaLookup.contains(value) =>
      extremaLookup(value)

    case va : L2_VariableAccess if extremaLookup.contains(va.name) =>
      extremaLookup(va.name)

    case L2_Addition(sums : ListBuffer[L2_Expression]) =>
      sums.view.map(s => evalIntegralExtrema(s, extremaLookup)).reduce { (x, y) =>
        (x._1 + y._1, x._2 + y._2)
      }

    case L2_Subtraction(l : L2_Expression, r : L2_Expression) =>
      val x = evalIntegralExtrema(l, extremaLookup)
      val y = evalIntegralExtrema(r, extremaLookup)
      (x._1 - y._2, x._2 - y._1)

    case L2_Multiplication(facs : ListBuffer[L2_Expression]) =>
      facs.view.map(s => evalIntegralExtrema(s, extremaLookup)).reduce { (x, y) =>
        val a = x._1 * y._1
        val b = x._1 * y._2
        val c = x._2 * y._1
        val d = x._2 * y._2
        (a min b min c min d, a max b max c max d)
      }

    case L2_Division(l : L2_Expression, r : L2_Expression) =>
      val x = evalIntegralExtrema(l, extremaLookup)
      val y = evalIntegralExtrema(r, extremaLookup)
      val a = x._1 / y._1
      val b = x._1 / y._2
      val c = x._2 / y._1
      val d = x._2 / y._2
      (a min b min c min d, a max b max c max d)

    case L2_Modulo(_ : L2_Expression, L2_IntegerConstant(den)) =>
      (0, den - 1)

    // may lead to incorrect results (e.g. for x % 2 and x has extrema 0 and 2)
    //    case L2_Modulo(l : L2_Expression, r : L2_Expression) =>
    //      val x = evalIntegralExtrema(l, extremaLookup)
    //      val y = evalIntegralExtrema(r, extremaLookup)
    //      val a = x._1 % y._1
    //      val b = x._1 % y._2
    //      val c = x._2 % y._1
    //      val d = x._2 % y._2
    //      (a min b min c min d, a max b max c max d)

    case L2_Minimum(l : ListBuffer[L2_Expression]) =>
      l.view.map(e => evalIntegralExtrema(e, extremaLookup)).reduce { (x, y) =>
        (x._1 min y._1, x._2 min y._2)
      }

    case L2_Maximum(l : ListBuffer[L2_Expression]) =>
      l.view.map(e => evalIntegralExtrema(e, extremaLookup)).reduce { (x, y) =>
        (x._1 max y._1, x._2 max y._2)
      }

    case L2_Negative(left : L2_Expression) =>
      val (min, max) = evalIntegralExtrema(left, extremaLookup)
      (-max, -min)

    case L2_FunctionCall(function, ListBuffer(l : L2_Expression, r : L2_Expression)) if "floord" == function.name =>
      evalIntegralExtrema(L2_Division(l, r), extremaLookup)

    case _ =>
      throw EvaluationException("unknown expression type for evaluation: " + expr.getClass)
  }

  /**
    * Completely evaluates an floating expression.
    * Only FloatConstants are allowed!
    * Other scalar constants or variable accesses lead to an EvaluationException.
    */
  def evalFloating(expr : L2_Expression) : Double = expr match {
    case L2_RealConstant(v)                                   => v
    case L2_Addition(sums : ListBuffer[L2_Expression])        => sums.view.map(s => evalFloating(s)).sum
    case L2_Subtraction(l : L2_Expression, r : L2_Expression) => evalFloating(l) - evalFloating(r)
    case L2_Multiplication(facs : ListBuffer[L2_Expression])  => facs.view.map(s => evalFloating(s)).product
    case L2_Division(l : L2_Expression, r : L2_Expression)    => evalFloating(l) / evalFloating(r)
    case L2_Minimum(l : ListBuffer[L2_Expression])            => l.view.map(e => evalFloating(e)).min
    case L2_Maximum(l : ListBuffer[L2_Expression])            => l.view.map(e => evalFloating(e)).max
    case _                                                    =>
      throw EvaluationException("unknown expression type for evaluation: " + expr.getClass)
  }

  /**
    * Constant string that is used to hold the additive constant of an affine expression in the result map of
    * evalIntegralAffine(Expression).
    */
  final val constName : L2_Expression = L2_NullExpression

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
  def extractIntegralSum(expr : L2_Expression) : HashMap[L2_Expression, Long] = {

    var res : HashMap[L2_Expression, Long] = null

    expr match {

      case L2_IntegerConstant(i) =>
        res = new HashMap[L2_Expression, Long]()
        res(constName) = i

      case fia : L2_FieldIteratorAccess =>
        res = new HashMap[L2_Expression, Long]()
        res(fia) = 1L

      case va : L2_VariableAccess =>
        res = new HashMap[L2_Expression, Long]()
        res(va) = 1L

      case L2_Negative(neg) =>
        res = extractIntegralSum(neg)
        for ((name : L2_Expression, value : Long) <- res)
          res(name) = -value

      case L2_Addition(summands) =>
        res = new HashMap[L2_Expression, Long]()
        for (s <- summands)
          for ((name : L2_Expression, value : Long) <- extractIntegralSum(s))
            res(name) = res.getOrElse(name, 0L) + value
        // opt:  (x/2) + (x%2)  ==>  (x+1)/2
        val toOpt = new HashMap[L2_Expression, (L2_Division, L2_Modulo, Long)]()
        for ((ex, coeff) <- res) ex match {
          case divd @ L2_Division(x, L2_IntegerConstant(2)) =>
            toOpt.get(x) match {
              case None                             => toOpt(x) = (divd, null, coeff)
              case Some((_, modd, c)) if c == coeff => toOpt(x) = (divd, modd, coeff)
              case Some(_)                          => toOpt -= x // coefficient is not matching...
            }
          case modd @ L2_Modulo(x, L2_IntegerConstant(2))   =>
            toOpt.get(x) match {
              case None                             => toOpt(x) = (null, modd, coeff)
              case Some((divd, _, c)) if c == coeff => toOpt(x) = (divd, modd, coeff)
              case Some(_)                          => toOpt -= x // coefficient is not matching...
            }
          case _                                            =>
        }
        for ((x, (div, mod, coeff)) <- toOpt) if (div != null && mod != null) {
          // ensure resulting map only contains normalized version created by recreateExprFromIntSum
          val nju = recreateExprFromIntSum(extractIntegralSum((x + L2_IntegerConstant(1L)) / L2_IntegerConstant(2L)))
          res -= div -= mod
          res(nju) = coeff + res.getOrElse(nju, 0L)
        }

      case L2_Subtraction(l, r) =>
        res = extractIntegralSum(l)
        for ((name : L2_Expression, value : Long) <- extractIntegralSum(r))
          res(name) = res.getOrElse(name, 0L) - value

      case L2_Multiplication(facs) =>
        var coeff : Long = 1L
        val nonCst = new ListBuffer[HashMap[L2_Expression, Long]]()
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
        res = new HashMap[L2_Expression, Long]()
        if (nonCst.isEmpty)
          res(constName) = coeff
        else if (nonCst.length == 1)
          for ((name : L2_Expression, value : Long) <- nonCst.head)
            res(name) = value * coeff
        else
          res(L2_Multiplication(nonCst.map(recreateExprFromIntSum).sortBy(_.prettyprint()))) = coeff

      case L2_Division(l, r) =>
        res = extractIntegralSumDivision(l, r, false)

      case L2_FunctionCall(function, ListBuffer(l, r)) if "floord" == function.name =>
        res = extractIntegralSumDivision(l, r, true)

      case L2_Modulo(l, r) =>
        val tmp = extractIntegralSum(r)
        if (tmp.isEmpty)
          throw EvaluationException("BOOM! (divide by zero)")
        if (!(tmp.size == 1 && tmp.contains(constName)))
          throw EvaluationException("only constant divisor allowed")
        val mod : Long = tmp(constName)
        res = new HashMap[L2_Expression, Long]()
        val dividendMap : HashMap[L2_Expression, Long] = extractIntegralSum(l)
        val dividend : L2_Expression = recreateExprFromIntSum(dividendMap)
        dividend match {
          case L2_IntegerConstant(x) => res(constName) = x % mod
          case _                     => res(L2_Modulo(dividend, L2_IntegerConstant(mod))) = 1L
        }

      case L2_Minimum(args : ListBuffer[L2_Expression]) =>
        val exprs = new ListBuffer[L2_Expression]
        var min : java.lang.Long = null
        for (arg <- args) simplifyIntegralExpr(arg) match {
          case L2_IntegerConstant(c) => min = if (min == null || min > c) c else min
          case e                     => if (!exprs.contains(e)) exprs += e
        }
        res = new HashMap[L2_Expression, Long]()
        if (exprs.isEmpty)
          res(constName) = min
        else {
          if (min != null)
            exprs += L2_IntegerConstant(min)
          res(L2_Minimum(exprs)) = 1L
        }

      case L2_Maximum(args : ListBuffer[L2_Expression]) =>
        val exprs = new ListBuffer[L2_Expression]
        var max : java.lang.Long = null
        for (arg <- args) simplifyIntegralExpr(arg) match {
          case L2_IntegerConstant(c) => max = if (max == null || max < c) c else max
          case e                     => if (!exprs.contains(e)) exprs += e
        }
        res = new HashMap[L2_Expression, Long]()
        if (exprs.isEmpty)
          res(constName) = max
        else {
          if (max != null)
            exprs += L2_IntegerConstant(max)
          res(L2_Maximum(exprs)) = 1L
        }

      case _ =>
        throw EvaluationException("unknown expression type for evaluation: " + expr.getClass)
    }

    res.filter(e => e._2 != 0L)
  }

  private def extractIntegralSumDivision(l : L2_Expression, r : L2_Expression, floor : Boolean) : HashMap[L2_Expression, Long] = {
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
    val res = new HashMap[L2_Expression, Long]()
    val mapL = extractIntegralSum(l)
    if (floor || !changeSign) { // do only remove parts of the dividend when the rounding direction is "uniform" (truncate rounds towards 0)
      for ((name : L2_Expression, value : Long) <- mapL)
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
    val (name, update) : (L2_Expression, Long) = dividend match {
      case L2_IntegerConstant(x) =>
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

      case L2_Division(x, L2_IntegerConstant(divs2)) if !floor                                                     =>
        (L2_Division(x, L2_IntegerConstant(divs * divs2)), 1L)
      case L2_Addition(ListBuffer(L2_Division(x, L2_IntegerConstant(divs2)), L2_IntegerConstant(const))) if !floor =>
        (simplifyIntegralExpr(L2_Division(x + L2_IntegerConstant(const * divs2), L2_IntegerConstant(divs * divs2))), 1L)
      case L2_Addition(ListBuffer(L2_IntegerConstant(const), L2_Division(x, L2_IntegerConstant(divs2)))) if !floor =>
        (simplifyIntegralExpr(L2_Division(x + L2_IntegerConstant(const * divs2), L2_IntegerConstant(divs * divs2))), 1L)

      case L2_FunctionCall(function, ListBuffer(x, L2_IntegerConstant(divs2))) if floor && "floord" == function.name                                                     =>
        (L2_FunctionCall(L2_InternalFunctionReference.floord, ListBuffer(x, L2_IntegerConstant(divs * divs2))), 1L)
      case L2_Addition(ListBuffer(L2_FunctionCall(function, ListBuffer(x, L2_IntegerConstant(divs2))), L2_IntegerConstant(const))) if floor && "floord" == function.name =>
        (simplifyIntegralExpr(L2_FunctionCall(L2_InternalFunctionReference.floord, x + L2_IntegerConstant(const * divs2), L2_IntegerConstant(divs * divs2))), 1L)
      case L2_Addition(ListBuffer(L2_IntegerConstant(const), L2_FunctionCall(function, ListBuffer(x, L2_IntegerConstant(divs2))))) if floor && "floord" == function.name =>
        (simplifyIntegralExpr(L2_FunctionCall(L2_InternalFunctionReference.floord, x + L2_IntegerConstant(const * divs2), L2_IntegerConstant(divs * divs2))), 1L)
      case divd                                                                                                                                                          =>
        if (floor)
          (L2_FunctionCall(L2_InternalFunctionReference.floord, divd, L2_IntegerConstant(divs)), 1L)
        else
          (L2_Division(divd, L2_IntegerConstant(divs)), 1L)
    }
    res(name) = res.getOrElse(name, 0L) + update
    res
  }

  /**
    * Takes the output from extractIntegralSum(..) and recreates an AST for this sum.
    */
  def recreateExprFromIntSum(sumMap : HashMap[L2_Expression, Long]) : L2_Expression = {

    val const : Long = sumMap.getOrElse(constName, 0L)

    val sumSeq = sumMap.view.filter(s => s._1 != constName && s._2 != 0L).toSeq.sortWith({
      case ((v1 : L2_VariableAccess, _), (v2 : L2_VariableAccess, _)) => v1.name < v2.name
      case ((_ : L2_VariableAccess, _), _)                                => true
      case (_, (_ : L2_VariableAccess, _))                                => false
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
      return L2_IntegerConstant(const)

    // use distributive property
    val reverse = new HashMap[Long, (ListBuffer[L2_Expression], ListBuffer[L2_Expression])]()

    def empty = (new ListBuffer[L2_Expression](), new ListBuffer[L2_Expression]())

    for ((njuExpr : L2_Expression, value : Long) <- sumSeq)
      if (value > 0L)
        reverse.getOrElseUpdate(value, empty)._1 += njuExpr
      else
        reverse.getOrElseUpdate(-value, empty)._2 += njuExpr

    val posSums = new ListBuffer[L2_Expression]()
    val negSums = new ListBuffer[L2_Expression]()

    def toExpr(sum : ListBuffer[L2_Expression]) = if (sum.length == 1) sum.head else new L2_Addition(sum)

    for ((value : Long, (pSums : ListBuffer[L2_Expression], nSums : ListBuffer[L2_Expression])) <- reverse.toSeq.sortBy(t => t._1).reverse)
      if (value == 1L) {
        posSums ++= pSums
        negSums ++= nSums
      } else {
        if (pSums.isEmpty)
          negSums += L2_Multiplication(L2_IntegerConstant(value), toExpr(nSums))
        else if (nSums.isEmpty)
          posSums += L2_Multiplication(L2_IntegerConstant(value), toExpr(pSums))
        else
          posSums += L2_Multiplication(L2_IntegerConstant(value), L2_Subtraction(toExpr(pSums), toExpr(nSums)))
      }

    if (const > 0L)
      posSums += L2_IntegerConstant(const)
    else if (const < 0L)
      negSums += L2_IntegerConstant(-const)

    if (posSums.isEmpty)
      L2_Negative(toExpr(negSums))
    else if (negSums.isEmpty)
      toExpr(posSums)
    else
      L2_Subtraction(toExpr(posSums), toExpr(negSums))
  }

  def simplifyIntegralExpr(expr : L2_Expression, extrMap : mutable.HashMap[String, (Long, Long)] = null) : L2_Expression = {
    extremaMap = extrMap
    try {
      val res = L2_ExpressionStatement(recreateExprFromIntSum(extractIntegralSum(expr)))
      extremaMap = null
      L2_GeneralSimplify.doUntilDoneStandalone(res)
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
  def extractFloatingSum(expr : L2_Expression) : HashMap[L2_Expression, Double] = {

    var res : HashMap[L2_Expression, Double] = null

    expr match {

      case L2_IntegerConstant(i) =>
        res = new HashMap[L2_Expression, Double]()
        res(constName) = i

      case L2_RealConstant(d) =>
        res = new HashMap[L2_Expression, Double]()
        res(constName) = d

      case fia : L2_FieldIteratorAccess =>
        res = new HashMap[L2_Expression, Double]()
        res(fia) = 1d

      case va : L2_VariableAccess =>
        res = new HashMap[L2_Expression, Double]()
        res(va) = 1d

      case call : L2_FunctionCall =>
        if (call.name.contains("std::rand")) // HACK
          throw EvaluationException("don't optimize code containing a call to std::rand")

        def simplifyFloatingArgs(pars : Seq[L2_Datatype]) : Unit = {
          call.arguments =
            pars.view.zip(call.arguments).map {
              case (L2_RealDatatype, arg) =>
                simplifyFloatingExpr(arg)
              case (_, arg)               =>
                arg
            }.to[ListBuffer]
        }

        if (L2_MathFunctions.signatures.contains(call.name))
          simplifyFloatingArgs(L2_MathFunctions.signatures(call.name)._1)
        // TODO: no L2_Function, search for other class instead?
        // else for (func <- StateManager.findFirst({ f : L2_Function => f.name == call.name }))
        //   simplifyFloatingArgs(func.parameters.view.map(_.datatype))
        res = new HashMap[L2_Expression, Double]()
        res(call) = 1d

      case L2_Negative(neg) =>
        res = extractFloatingSum(neg)
        for ((name : L2_Expression, value : Double) <- res)
          res(name) = -value

      case L2_Addition(summands) =>
        res = new HashMap[L2_Expression, Double]()
        for (s <- summands)
          for ((name : L2_Expression, value : Double) <- extractFloatingSum(s))
            res(name) = res.getOrElse(name, 0d) + value

      case L2_Subtraction(l, r) =>
        res = extractFloatingSum(l)
        for ((name : L2_Expression, value : Double) <- extractFloatingSum(r))
          res(name) = res.getOrElse(name, 0d) - value

      case L2_Multiplication(facs) =>
        var coeff : Double = 1d
        val nonCst = new ListBuffer[HashMap[L2_Expression, Double]]()
        for (f <- facs) {
          val map = extractFloatingSum(f)
          if (map.size == 1 && map.contains(constName) || map.isEmpty)
            coeff *= map.getOrElse(constName, 0d)
          else
            nonCst += map
        }
        res = new HashMap[L2_Expression, Double]()
        if (nonCst.isEmpty)
          res(constName) = coeff
        else if (nonCst.length == 1)
          for ((name : L2_Expression, value : Double) <- nonCst.head)
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
          res(L2_Multiplication(nonCst.map(recreateExprFromFloatSum).sortBy(_.prettyprint()))) = coeff
        }

      case L2_Division(l, r) =>
        val mapL = extractFloatingSum(l)
        val mapR = extractFloatingSum(r)
        if (mapR.size == 1 && mapR.contains(constName)) {
          val div : Double = mapR(constName)
          res = mapL.transform { (_ : L2_Expression, value : Double) => value / div }
        } else {
          var coefL : Double = 1d
          var exprL = recreateExprFromFloatSum(mapL)
          var coefR : Double = 1d
          var exprR = recreateExprFromFloatSum(mapR)
          exprL match {
            case L2_Multiplication(ListBuffer(L2_RealConstant(coef), inner)) =>
              coefL = coef
              exprL = inner
            case L2_RealConstant(coef)                                       =>
              coefL = coef
              exprL = L2_RealConstant(1d)
            case _                                                           =>
          }
          exprR match {
            case L2_Multiplication(ListBuffer(L2_RealConstant(coef), inner)) =>
              coefR = coef
              exprR = inner
            // case L2_RealConstant(coef) => // cannot happen, see other branch of surrounding if
            case _ =>
          }
          res = new HashMap[L2_Expression, Double]()
          val div = L2_Division(exprL, exprR)
          res(div) = coefL / coefR
        }

      case L2_Modulo(l, r) =>
        val mapR = extractFloatingSum(r)
        if (!(mapR.size == 1 && mapR.contains(constName)))
          throw EvaluationException("only constant divisor allowed yet:  " + l.prettyprint() + "  %  " + r.prettyprint())
        val mod : Double = mapR(constName)
        res = extractFloatingSum(l)
        for ((name : L2_Expression, value : Double) <- res)
          res(name) = value % mod

      case L2_Minimum(args : ListBuffer[L2_Expression]) =>
        val exprs = new ListBuffer[L2_Expression]
        var min : java.lang.Double = null
        for (arg <- args) simplifyFloatingExpr(arg) match {
          case L2_RealConstant(c) => min = if (min == null || min > c) c else min
          case e                  => if (!exprs.contains(e)) exprs += e
        }
        res = new HashMap[L2_Expression, Double]()
        if (exprs.isEmpty)
          res(constName) = min
        else {
          if (min != null)
            exprs += L2_RealConstant(min)
          res(L2_Minimum(exprs)) = 1d
        }

      case L2_Maximum(args : ListBuffer[L2_Expression]) =>
        val exprs = new ListBuffer[L2_Expression]
        var max : java.lang.Double = null
        for (arg <- args) simplifyFloatingExpr(arg) match {
          case L2_RealConstant(c) => max = if (max == null || max < c) c else max
          case e                  => if (!exprs.contains(e)) exprs += e
        }
        res = new HashMap[L2_Expression, Double]()
        if (exprs.isEmpty)
          res(constName) = max
        else {
          if (max != null)
            exprs += L2_RealConstant(max)
          res(L2_Maximum(exprs)) = 1d
        }

      case L2_Power(l, r) =>
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
              res = extractFloatingSum(L2_Multiplication(ListBuffer.fill(expI)(Duplicate(l))))
            case _                           =>
          }
        }
        if (res == null) // no better solution found, use trivial one
          res = HashMap(L2_Power(recreateExprFromFloatSum(mapL), recreateExprFromFloatSum(mapR)) -> 1d)

      case _ =>
        throw EvaluationException("unknown expression type for evaluation: " + expr.getClass + " in " + expr.prettyprint())
    }

    res.filter(e => e._2 != 0d)
  }

  /**
    * Takes the output from extractFloatingSum(..) and recreates an AST for this sum.
    */
  def recreateExprFromFloatSum(sumMap : HashMap[L2_Expression, Double]) : L2_Expression = {

    val const : Double = sumMap.getOrElse(constName, 0d)

    val sumSeq = sumMap.view.filter(s => s._1 != constName && s._2 != 0d).toSeq.sortWith({
      case ((v1 : L2_VariableAccess, _), (v2 : L2_VariableAccess, _)) => v1.name < v2.name
      case ((_ : L2_VariableAccess, _), _)                                => true
      case (_, (_ : L2_VariableAccess, _))                                => false
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
      return L2_RealConstant(const)

    // use distributive property
    val reverse = new HashMap[Double, (ListBuffer[L2_Expression], ListBuffer[L2_Expression])]()

    def empty = (new ListBuffer[L2_Expression](), new ListBuffer[L2_Expression]())

    for ((njuExpr : L2_Expression, value : Double) <- sumSeq)
      if (value > 0d)
        reverse.getOrElseUpdate(value, empty)._1 += njuExpr
      else
        reverse.getOrElseUpdate(-value, empty)._2 += njuExpr

    val posSums = new ListBuffer[L2_Expression]()
    val negSums = new ListBuffer[L2_Expression]()

    def toExpr(sum : ListBuffer[L2_Expression]) = if (sum.length == 1) sum.head else new L2_Addition(sum)

    for ((value : Double, (pSums : ListBuffer[L2_Expression], nSums : ListBuffer[L2_Expression])) <- reverse.toSeq.sortBy(t => t._1).reverse)
      if (value == 1d) {
        posSums ++= pSums
        negSums ++= nSums
      } else {
        if (pSums.isEmpty)
          negSums += L2_Multiplication(L2_RealConstant(value), toExpr(nSums))
        else if (nSums.isEmpty)
          posSums += L2_Multiplication(L2_RealConstant(value), toExpr(pSums))
        else
          posSums += L2_Multiplication(L2_RealConstant(value), L2_Subtraction(toExpr(pSums), toExpr(nSums)))
      }

    if (const > 0d)
      posSums += L2_RealConstant(const)
    else if (const < 0d)
      negSums += L2_RealConstant(-const)

    if (posSums.isEmpty)
      L2_Negative(toExpr(negSums))
    else if (negSums.isEmpty)
      toExpr(posSums)
    else
      L2_Subtraction(toExpr(posSums), toExpr(negSums))
  }

  def simplifyFloatingExpr(expr : L2_Expression) : L2_Expression = {
    try {
      val res = L2_ExpressionStatement(recreateExprFromFloatSum(extractFloatingSum(expr)))
      L2_GeneralSimplify.doUntilDoneStandalone(res)
      res.expression
    } catch {
      case ex : EvaluationException =>
        throw EvaluationException(ex.msg + ";  in " + expr.prettyprint(), ex)
    }
  }
}
