package exastencils.util

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.logger._

object SimplifyExpression {

  /**
    * Completely evaluates an integral expression.
    * Only IntegerConstants are allowed!
    * Other scalar constants or variable accesses lead to an EvaluationException.
    */
  def evalIntegral(expr : Expression) : Long = expr match {
    case IntegerConstant(v)                                      => v
    case AdditionExpression(sums : ListBuffer[Expression])       => sums.view.map(s => evalIntegral(s)).reduce(_ + _)
    case SubtractionExpression(l : Expression, r : Expression)   => evalIntegral(l) - evalIntegral(r)
    case MultiplicationExpression(facs : ListBuffer[Expression]) => facs.view.map(s => evalIntegral(s)).reduce(_ * _)
    case DivisionExpression(l : Expression, r : Expression)      => evalIntegral(l) / evalIntegral(r)
    case ModuloExpression(l : Expression, r : Expression)        => evalIntegral(l) % evalIntegral(r)
    case MinimumExpression(l : ListBuffer[Expression])           => l.view.map(e => evalIntegral(e)).reduce(_ min _)
    case MaximumExpression(l : ListBuffer[Expression])           => l.view.map(e => evalIntegral(e)).reduce(_ max _)
    case _ =>
      throw new EvaluationException("unknown expression type for evaluation: " + expr.getClass())
  }

  /**
    * Completely evaluates an floating expression.
    * Only FloatConstants are allowed!
    * Other scalar constants or variable accesses lead to an EvaluationException.
    */
  def evalFloating(expr : Expression) : Double = expr match {
    case FloatConstant(v)                                        => v
    case AdditionExpression(sums : ListBuffer[Expression])       => sums.view.map(s => evalFloating(s)).reduce(_ + _)
    case SubtractionExpression(l : Expression, r : Expression)   => evalFloating(l) - evalFloating(r)
    case MultiplicationExpression(facs : ListBuffer[Expression]) => facs.view.map(s => evalIntegral(s)).reduce(_ * _)
    case DivisionExpression(l : Expression, r : Expression)      => evalFloating(l) / evalFloating(r)
    case MinimumExpression(l : ListBuffer[Expression])           => l.view.map(e => evalFloating(e)).reduce(_ min _)
    case MaximumExpression(l : ListBuffer[Expression])           => l.view.map(e => evalFloating(e)).reduce(_ max _)
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
    return extractIntegralSumRec(expr)
  }

  private def extractIntegralSumRec(expr : Expression) : HashMap[Expression, Long] = {

    var res : HashMap[Expression, Long] = null

    expr match {

      case IntegerConstant(i) =>
        res = new HashMap[Expression, Long]()
        res(constName) = i

      case VariableAccess(varName, _) =>
        res = new HashMap[Expression, Long]()
        res(VariableAccess(varName, Some(IntegerDatatype))) = 1L

      case StringLiteral(varName) =>
        res = new HashMap[Expression, Long]()
        res(VariableAccess(varName, Some(IntegerDatatype))) = 1L // ONLY VariableAccess in res keys, NO StringConstant

      case acc : ArrayAccess =>
        res = new HashMap[Expression, Long]()
        res(acc) = 1L

      case NegativeExpression(expr) =>
        res = extractIntegralSumRec(expr)
        for ((name : Expression, value : Long) <- extractIntegralSumRec(expr))
          res(name) = -value

      case AdditionExpression(summands) =>
        res = new HashMap[Expression, Long]()
        for (s <- summands)
          for ((name : Expression, value : Long) <- extractIntegralSumRec(s))
            res(name) = res.getOrElse(name, 0L) + value
        // opt:  (x/2) + (x%2)  ==>  (x+1)/2
        val toOpt = new HashMap[Expression, (DivisionExpression, ModuloExpression, Long)]()
        for ((ex, coeff) <- res) ex match {
          case divd @ DivisionExpression(x, IntegerConstant(2)) =>
            toOpt.get(x) match {
              case None                               => toOpt(x) = (divd, null, coeff)
              case Some((_, modd, c)) if (c == coeff) => toOpt(x) = (divd, modd, coeff)
              case Some(_)                            => toOpt -= x // coefficient is not matching...
            }
          case modd @ ModuloExpression(x, IntegerConstant(2)) =>
            toOpt.get(x) match {
              case None                               => toOpt(x) = (null, modd, coeff)
              case Some((divd, _, c)) if (c == coeff) => toOpt(x) = (divd, modd, coeff)
              case Some(_)                            => toOpt -= x // coefficient is not matching...
            }
          case _ =>
        }
        for ((x, (div, mod, coeff)) <- toOpt) if (div != null && mod != null) {
          // ensure resulting map only contains normalized version created by recreateExprFromIntSum
          val nju = recreateExprFromIntSum(extractIntegralSumRec((x + IntegerConstant(1L)) / IntegerConstant(2L)))
          res -= div -= mod
          res(nju) = coeff + res.getOrElse(nju, 0L)
        }

      case SubtractionExpression(l, r) =>
        res = extractIntegralSumRec(l)
        for ((name : Expression, value : Long) <- extractIntegralSumRec(r))
          res(name) = res.getOrElse(name, 0L) - value

      case MultiplicationExpression(facs) =>
        var coeff : Long = 1L
        val nonCst = new ListBuffer[Expression]()
        var nonCstMap : HashMap[Expression, Long] = null
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
        res = new HashMap[Expression, Long]()
        if (nonCst.isEmpty)
          res(constName) = coeff
        else if (nonCst.length == 1)
          for ((name : Expression, value : Long) <- nonCstMap)
            res(name) = value * coeff
        else
          res(new MultiplicationExpression(nonCst.sortBy(_.prettyprint()))) = coeff

      //      case MultiplicationExpression(l, r) =>
      //        val mapL = extractIntegralSumRec(l)
      //        val mapR = extractIntegralSumRec(r)
      //        var coeff : Long = 1L
      //        if ((mapL.size == 1 && mapL.contains(constName)) || mapL.isEmpty) {
      //          coeff = mapL.getOrElse(constName, 0L)
      //          res = mapR
      //        } else if ((mapR.size == 1 && mapR.contains(constName)) || mapR.isEmpty) {
      //          coeff = mapR.getOrElse(constName, 0L)
      //          res = mapL
      //        } else {
      //          var gcdL : Long = math.abs(mapL.head._2)
      //          for ((_, c) <- mapL)
      //            gcdL = gcd(c, gcdL)
      //          var gcdR : Long = math.abs(mapR.head._2)
      //          for ((_, c) <- mapR)
      //            gcdR = gcd(c, gcdR)
      //          for ((e, c) <- mapL)
      //            mapL(e) = c / gcdL
      //          for ((e, c) <- mapR)
      //            mapR(e) = c / gcdR
      //          val exprL : Expression = recreateExprFromIntSum(mapL)
      //          val exprR : Expression = recreateExprFromIntSum(mapR)
      //          res = new HashMap[Expression, Long]()
      //          if (exprL.prettyprint() <= exprR.prettyprint())
      //            res(new MultiplicationExpression(exprL, exprR)) = gcdL * gcdR
      //          else
      //            res(new MultiplicationExpression(exprR, exprL)) = gcdL * gcdR
      //        }
      //        if (coeff == 0L)
      //          res.clear()
      //        else if (coeff != 1L)
      //          for ((name : Expression, value : Long) <- res)
      //            res(name) = value * coeff

      case DivisionExpression(l, r) =>
        val tmp = extractIntegralSumRec(r)
        if (tmp.isEmpty)
          throw new EvaluationException("BOOM! (divide by zero)")
        if (!(tmp.size == 1 && tmp.contains(constName)))
          throw new EvaluationException("only constant divisor allowed yet")
        val divs : Long = tmp(constName)
        tmp.clear()
        res = new HashMap[Expression, Long]()
        val mapL = extractIntegralSumRec(l)
        for ((name : Expression, value : Long) <- mapL)
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
        val dividend = recreateExprFromIntSum(tmp)
        val (name, update) : (Expression, Long) = dividend match {
          case IntegerConstant(x)                            => (constName, x / divs)
          case DivisionExpression(x, IntegerConstant(divs2)) => (DivisionExpression(x, IntegerConstant(divs * divs2)), 1L)
          case AdditionExpression(ListBuffer(DivisionExpression(x, IntegerConstant(divs2)), IntegerConstant(const))) =>
            (simplifyIntegralExpr(DivisionExpression(x + IntegerConstant(const * divs2), IntegerConstant(divs * divs2))), 1L)
          case AdditionExpression(ListBuffer(IntegerConstant(const), DivisionExpression(x, IntegerConstant(divs2)))) =>
            (simplifyIntegralExpr(DivisionExpression(x + IntegerConstant(const * divs2), IntegerConstant(divs * divs2))), 1L)
          case divd => (DivisionExpression(divd, IntegerConstant(divs)), 1L)
        }
        res(name) = res.get(name).getOrElse(0L) + update

      case FunctionCallExpression("floord", ListBuffer(l, r)) =>
        val tmp = extractIntegralSumRec(r)
        if (tmp.isEmpty)
          throw new EvaluationException("BOOM! (divide by zero)")
        if (!(tmp.size == 1 && tmp.contains(constName)))
          throw new EvaluationException("only constant divisor allowed yet")
        val divs : Long = tmp(constName)
        tmp.clear()
        res = new HashMap[Expression, Long]()
        val mapL = extractIntegralSumRec(l)
        for ((name : Expression, value : Long) <- mapL)
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
        val dividend = recreateExprFromIntSum(tmp)
        val (name, update) : (Expression, Long) = dividend match {
          case IntegerConstant(x) => (constName, if (x >= 0) x / divs else ((x - divs + 1) / divs))
          case FunctionCallExpression("floord", ListBuffer(x, IntegerConstant(divs2))) =>
            (FunctionCallExpression("floord", ListBuffer(x, IntegerConstant(divs * divs2))), 1L)
          case AdditionExpression(ListBuffer(FunctionCallExpression("floord", ListBuffer(x, IntegerConstant(divs2))), IntegerConstant(const))) =>
            (simplifyIntegralExpr(FunctionCallExpression("floord", ListBuffer(x + IntegerConstant(const * divs2), IntegerConstant(divs * divs2)))), 1L)
          case AdditionExpression(ListBuffer(IntegerConstant(const), FunctionCallExpression("floord", ListBuffer(x, IntegerConstant(divs2))))) =>
            (simplifyIntegralExpr(FunctionCallExpression("floord", ListBuffer(x + IntegerConstant(const * divs2), IntegerConstant(divs * divs2)))), 1L)
          case divd => (FunctionCallExpression("floord", ListBuffer(divd, IntegerConstant(divs))), 1L)
        }
        res(name) = res.get(name).getOrElse(0L) + update

      case ModuloExpression(l, r) =>
        val tmp = extractIntegralSumRec(r)
        if (tmp.isEmpty)
          throw new EvaluationException("BOOM! (divide by zero)")
        if (!(tmp.size == 1 && tmp.contains(constName)))
          throw new EvaluationException("only constant divisor allowed")
        val mod : Long = tmp(constName)
        res = new HashMap[Expression, Long]()
        val dividendMap : HashMap[Expression, Long] = extractIntegralSumRec(l).filter(elem => elem._2 % mod != 0L)
        val cstOpt = dividendMap.remove(constName) // const part can be reduced
        if (cstOpt.isDefined)
          dividendMap(constName) = (cstOpt.get % mod + mod) % mod // mathematical modulo
        val dividend : Expression = recreateExprFromIntSum(dividendMap)
        dividend match {
          case IntegerConstant(x) => res(constName) = x // const part is already result of modulo
          case _                  => res(ModuloExpression(dividend, IntegerConstant(mod))) = 1L
        }

      case MinimumExpression(args : ListBuffer[Expression]) =>
        val exprs = new ListBuffer[Expression]
        var min : java.lang.Long = null
        for (arg <- args) simplifyIntegralExpr(arg) match {
          case IntegerConstant(c)              => min = if (min == null || min > c) c else min
          case expr if (!exprs.contains(expr)) => exprs += expr
        }
        res = new HashMap[Expression, Long]()
        if (exprs.isEmpty)
          res(constName) = min
        else {
          exprs += IntegerConstant(min)
          res(MinimumExpression(exprs)) = 1L
        }

      case MaximumExpression(args : ListBuffer[Expression]) =>
        val exprs = new ListBuffer[Expression]
        var max : java.lang.Long = null
        for (arg <- args) simplifyIntegralExpr(arg) match {
          case IntegerConstant(c)              => max = if (max == null || max < c) c else max
          case expr if (!exprs.contains(expr)) => exprs += expr
        }
        res = new HashMap[Expression, Long]()
        if (exprs.isEmpty)
          res(constName) = max
        else {
          exprs += IntegerConstant(max)
          res(MinimumExpression(exprs)) = 1L
        }

      case scalarIV : iv.InternalVariable if scalarIV.resolveDataType.isInstanceOf[ScalarDatatype] =>
        res = new HashMap[Expression, Long]()
        res(scalarIV) = 1L

      case anyIV : iv.InternalVariable =>
        Logger.warn(s"Found non-scalar iv ${anyIV.prettyprint()} in extractIntegralSumRec")
        res = new HashMap[Expression, Long]()
        res(anyIV) = 1L

      case _ =>
        throw new EvaluationException("unknown expression type for evaluation: " + expr.getClass())
    }

    return res.filter(e => e._2 != 0L)
  }

  def gcd(x : Long, y : Long) : Long = {
    var a : Long = x
    var b : Long = y
    while (b != 0) {
      val h = a % b
      a = b
      b = h
    }
    return math.abs(a)
  }

  /**
    * Takes the output from extractIntegralSum(..) and recreates an AST for this sum.
    */
  def recreateExprFromIntSum(sumMap : HashMap[Expression, Long]) : Expression = {

    var res : Expression = null
    val const : Option[Long] = sumMap.get(constName)

    val sumSeq = sumMap.view.filter(s => s._1 != constName && s._2 != 0L).toSeq.sortWith({
      case ((VariableAccess(v1, _), _), (VariableAccess(v2, _), _)) => v1 < v2
      case ((v1 : VariableAccess, _), _)                            => true
      case (_, (v2 : VariableAccess, _))                            => false
      case ((e1, _), (e2, _))                                       => e1.prettyprint() < e2.prettyprint()
    })

    if (sumSeq.isEmpty)
      return IntegerConstant(const.getOrElse(0L))

    // use distributive property
    val reverse = new HashMap[Long, Expression]()
    for ((njuExpr : Expression, value : Long) <- sumSeq) {
      val expr : Option[Expression] = reverse.get(value)
      reverse(value) =
        if (expr.isDefined)
          new AdditionExpression(expr.get, njuExpr)
        else
          njuExpr
    }

    for ((value : Long, expr : Expression) <- reverse.toSeq.sortBy(t => t._1).reverse) {
      if (res == null) {
        res = value match {
          case 1L  => expr
          case -1L => NegativeExpression(expr)
          case _   => new MultiplicationExpression(IntegerConstant(value), expr)
        }
      } else {
        val (summand, negative) : (Expression, Boolean) =
          value match {
            case 1L  => (expr, false)
            case -1L => (expr, true)
            case _   => (new MultiplicationExpression(IntegerConstant(math.abs(value)), expr), value < 0L)
          }
        res =
          if (negative)
            new SubtractionExpression(res, summand)
          else
            new AdditionExpression(res, summand)
      }
    }

    if (const.isDefined && const.get != 0L)
      res =
        if (const.get > 0L)
          new AdditionExpression(res, IntegerConstant(const.get))
        else
          new SubtractionExpression(res, IntegerConstant(-const.get))

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

  object SimplifyIndices extends QuietDefaultStrategy("Simplify indices") {

    this += new Transformation("now", {
      case a : ArrayAccess =>
        a.index = SimplifyExpression.simplifyIntegralExpr(a.index)
        a

      case d : DirectFieldAccess =>
        for (i <- 0 until 4)
          if (d.index(i) != NullExpression)
            d.index(i) = SimplifyExpression.simplifyIntegralExpr(d.index(i))
        d

      case f : FieldAccess =>
        for (i <- 0 until 4)
          if (f.index(i) != NullExpression)
            f.index(i) = SimplifyExpression.simplifyIntegralExpr(f.index(i))
        f

      case f : ExternalFieldAccess =>
        for (i <- 0 until 4)
          if (f.index(i) != NullExpression)
            f.index(i) = SimplifyExpression.simplifyIntegralExpr(f.index(i))
        f
    })
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
    return extractFloatingSumRec(expr)
  }

  private def extractFloatingSumRec(expr : Expression) : HashMap[Expression, Double] = {

    var res : HashMap[Expression, Double] = null

    expr match {

      case IntegerConstant(i) =>
        res = new HashMap[Expression, Double]()
        res(constName) = i

      case FloatConstant(d) =>
        res = new HashMap[Expression, Double]()
        res(constName) = d

      case VariableAccess(varName, _) =>
        res = new HashMap[Expression, Double]()
        res(VariableAccess(varName, Some(RealDatatype))) = 1d

      case StringLiteral(varName) =>
        if (varName.contains("std::rand")) // HACK
          throw new EvaluationException("don't optimze code containing a call to std::rand")
        res = new HashMap[Expression, Double]()
        res(VariableAccess(varName, Some(RealDatatype))) = 1d // ONLY VariableAccess in res keys, NO StringConstant

      case aAcc : ArrayAccess =>
        res = new HashMap[Expression, Double]()
        res(aAcc) = 1d

      case NegativeExpression(expr) =>
        res = extractFloatingSumRec(expr)
        for ((name : Expression, value : Double) <- extractFloatingSumRec(expr))
          res(name) = -value

      case AdditionExpression(summands) =>
        for (s <- summands)
          if (res == null)
            res = extractFloatingSumRec(s)
          else
            for ((name : Expression, value : Double) <- extractFloatingSumRec(s))
              res(name) = res.getOrElse(name, 0d) + value

      case SubtractionExpression(l, r) =>
        res = extractFloatingSumRec(l)
        for ((name : Expression, value : Double) <- extractFloatingSumRec(r))
          res(name) = res.getOrElse(name, 0d) - value

      case MultiplicationExpression(facs) =>
        var coeff : Double = 1d
        val nonCst = new ListBuffer[HashMap[Expression, Double]]()
        for (f <- facs) {
          val map = extractFloatingSumRec(f)
          if (map.size == 1 && map.contains(constName) || map.isEmpty) {
            coeff *= map.getOrElse(constName, 0d)
          } else {
            nonCst += map
          }
        }
        res = new HashMap[Expression, Double]()
        if (nonCst.isEmpty)
          res(constName) = coeff
        else if (nonCst.length == 1)
          for ((name : Expression, value : Double) <- nonCst.head)
            res(name) = value * coeff
        else
          res(new MultiplicationExpression(nonCst.map { sum => recreateExprFromFloatSum(sum.map { nv => (nv._1, nv._2 * coeff) }) })) = 1d

      //      case MultiplicationExpression(l, r) =>
      //        val mapL = extractFloatingSumRec(l)
      //        val mapR = extractFloatingSumRec(r)
      //        var coeff : Double = 1d
      //        if (mapL.size == 1 && mapL.contains(constName)) {
      //          coeff = mapL(constName)
      //          res = mapR
      //        } else if (mapR.size == 1 && mapR.contains(constName)) {
      //          coeff = mapR(constName)
      //          res = mapL
      //        } else
      //          throw new EvaluationException("non-constant * non-constant is not yet implemented:  " +
      //            l.prettyprint() + "  *  " + r.prettyprint())
      //        if (coeff == 0d)
      //          res.clear()
      //        else
      //          for ((name : Expression, value : Double) <- res)
      //            res(name) = value * coeff

      case DivisionExpression(l, r) =>
        val mapR = extractFloatingSumRec(r)
        if (!(mapR.size == 1 && mapR.contains(constName)))
          throw new EvaluationException("only constant divisor allowed yet:  " + l.prettyprint() + "  /  " + r.prettyprint())
        val div : Double = mapR(constName)
        res = extractFloatingSumRec(l)
        for ((name : Expression, value : Double) <- res)
          res(name) = value / div

      case ModuloExpression(l, r) =>
        val mapR = extractFloatingSumRec(r)
        if (!(mapR.size == 1 && mapR.contains(constName)))
          throw new EvaluationException("only constant divisor allowed yet:  " + l.prettyprint() + "  %  " + r.prettyprint())
        val mod : Double = mapR(constName)
        res = extractFloatingSumRec(l)
        for ((name : Expression, value : Double) <- res)
          res(name) = value % mod

      case MinimumExpression(args : ListBuffer[Expression]) =>
        val exprs = new ListBuffer[Expression]
        var min : java.lang.Double = null
        for (arg <- args) simplifyIntegralExpr(arg) match {
          case IntegerConstant(c)              => min = if (min == null || min > c) c else min
          case expr if (!exprs.contains(expr)) => exprs += expr
        }
        res = new HashMap[Expression, Double]()
        if (exprs.isEmpty)
          res(constName) = min
        else {
          if (min != null)
            exprs += FloatConstant(min)
          res(MinimumExpression(exprs)) = 1L
        }

      case MaximumExpression(args : ListBuffer[Expression]) =>
        val exprs = new ListBuffer[Expression]
        var max : java.lang.Double = null
        for (arg <- args) simplifyIntegralExpr(arg) match {
          case IntegerConstant(c)              => max = if (max == null || max < c) c else max
          case expr if (!exprs.contains(expr)) => exprs += expr
        }
        res = new HashMap[Expression, Double]()
        if (exprs.isEmpty)
          res(constName) = max
        else {
          if (max != null)
            exprs += FloatConstant(max)
          res(MaximumExpression(exprs)) = 1L
        }

      case _ =>
        throw new EvaluationException("unknown expression type for evaluation: " + expr.getClass() + " in " + expr.prettyprint())
    }

    return res.filter(e => e._2 != 0.0)
  }

  /**
    * Takes the output from extractFloatingSum(..) and recreates an AST for this sum.
    */
  def recreateExprFromFloatSum(sumMap : HashMap[Expression, Double]) : Expression = {

    var res : Expression = null
    val const : Option[Double] = sumMap.get(constName)

    val sumSeq = sumMap.view.filter(s => s._1 != constName && s._2 != 0d).toSeq.sortWith({
      case ((VariableAccess(v1, _), _), (VariableAccess(v2, _), _)) => v1 < v2
      case ((v1 : VariableAccess, _), _)                            => true
      case (_, (v2 : VariableAccess, _))                            => false
      case ((e1, _), (e2, _))                                       => e1.prettyprint() < e2.prettyprint()
    })

    if (sumSeq.isEmpty)
      return FloatConstant(const.getOrElse(0d))

    // use distributive property
    val reverse = new HashMap[Double, Expression]()
    for ((njuExpr : Expression, value : Double) <- sumSeq) {
      val expr : Option[Expression] = reverse.get(value)
      reverse(value) =
        if (expr.isDefined)
          new AdditionExpression(expr.get, njuExpr)
        else
          njuExpr
    }

    for ((value : Double, expr : Expression) <- reverse.toSeq.sortBy(t => t._1).reverse) {
      if (res == null) {
        res = value match {
          case 1d  => expr
          case -1d => NegativeExpression(expr)
          case _   => new MultiplicationExpression(FloatConstant(value), expr)
        }
      } else {
        val (summand, negative) : (Expression, Boolean) =
          value match {
            case 1d  => (expr, false)
            case -1d => (expr, true)
            case _   => (new MultiplicationExpression(FloatConstant(math.abs(value)), expr), value < 0d)
          }
        res =
          if (negative)
            new SubtractionExpression(res, summand)
          else
            new AdditionExpression(res, summand)
      }
    }

    if (const.isDefined && const.get != 0d)
      res = new AdditionExpression(res, FloatConstant(const.get))

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
