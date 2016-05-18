package exastencils.util

import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.iv._
import exastencils.logger._
import exastencils.strategies._

import scala.collection._
import scala.collection.mutable.ListBuffer

object SimplifyExpression {

  /**
   * Completely evaluates an integral expression.
   * Only IntegerConstants are allowed!
   * Other scalar constants or variable accesses lead to an EvaluationException.
   */
  def evalIntegral(expr : Expression) : Long = expr match {
    case IntegerConstant(v) => v
    case AdditionExpression(sums : ListBuffer[Expression]) => sums.view.map(s => evalIntegral(s)).sum
    case SubtractionExpression(l : Expression, r : Expression) => evalIntegral(l) - evalIntegral(r)
    case MultiplicationExpression(facs : ListBuffer[Expression]) => facs.view.map(s => evalIntegral(s)).product
    case DivisionExpression(l : Expression, r : Expression) => evalIntegral(l) / evalIntegral(r)
    case ModuloExpression(l : Expression, r : Expression) => evalIntegral(l) % evalIntegral(r)
    case MinimumExpression(l : ListBuffer[Expression]) => l.view.map(e => evalIntegral(e)).min
    case MaximumExpression(l : ListBuffer[Expression]) => l.view.map(e => evalIntegral(e)).max
    case _ =>
      throw new EvaluationException("unknown expression type for evaluation: " + expr.getClass)
  }

  final val EXTREMA_ANNOT : String = "extrema" // associated value must be (Long, Long)

  /**
   * Completely evaluates an integral expression and computes a lower bound and an upper bound
   *   for its value depending on the minOffset and maxOffset in potential OffsetIndex nodes.
   * Only IntegerConstants are allowed! (Except for the offset field in OffsetIndex, which is not evaluated at all.)
   * Other scalar constants or variable accesses lead to an EvaluationException.
   */
  def evalIntegralExtrema(expr : Expression) : (Long, Long) = expr match {
    case IntegerConstant(v) =>
      (v, v)

    case AdditionExpression(sums : ListBuffer[Expression]) =>
      sums.view.map(s => evalIntegralExtrema(s)).reduce { (x, y) =>
        (x._1 + y._1, x._2 + y._2)
      }

    case SubtractionExpression(l : Expression, r : Expression) =>
      val x = evalIntegralExtrema(l)
      val y = evalIntegralExtrema(r)
      (x._1 - y._2, x._2 - y._1)

    case MultiplicationExpression(facs : ListBuffer[Expression]) =>
      facs.view.map(s => evalIntegralExtrema(s)).reduce { (x, y) =>
        val a = x._1 * y._1
        val b = x._1 * y._2
        val c = x._2 * y._1
        val d = x._2 * y._2
        (a min b min c min d, a max b max c max d)
      }

    case DivisionExpression(l : Expression, r : Expression) =>
      val x = evalIntegralExtrema(l)
      val y = evalIntegralExtrema(r)
      val a = x._1 / y._1
      val b = x._1 / y._2
      val c = x._2 / y._1
      val d = x._2 / y._2
      (a min b min c min d, a max b max c max d)

    case ModuloExpression(l : Expression, r : Expression) =>
      val x = evalIntegralExtrema(l)
      val y = evalIntegralExtrema(r)
      val a = x._1 % y._1
      val b = x._1 % y._2
      val c = x._2 % y._1
      val d = x._2 % y._2
      (a min b min c min d, a max b max c max d)

    case MinimumExpression(l : ListBuffer[Expression]) =>
      l.view.map(e => evalIntegralExtrema(e)).reduce { (x, y) =>
        (x._1 min y._1, x._2 min y._2)
      }

    case MaximumExpression(l : ListBuffer[Expression]) =>
      l.view.map(e => evalIntegralExtrema(e)).reduce { (x, y) =>
        (x._1 max y._1, x._2 max y._2)
      }

    case OffsetIndex(minOffset, maxOffset, index, _) =>
      val x = evalIntegralExtrema(index)
      (x._1 + minOffset, x._2 + maxOffset)

    case n if n.hasAnnotation(EXTREMA_ANNOT) =>
      n.getAnnotation(EXTREMA_ANNOT).get.asInstanceOf[(Long, Long)]

    case _ =>
      throw new EvaluationException("unknown expression type for evaluation: " + expr.getClass)
  }

  /**
   * Evaluates an expression as far as possible and computes a lower bound and an upper bound for its value.
   */
  def evalExpressionExtrema(expr : Expression) : (Expression, Expression) = expr match {
    case c : IntegerConstant =>
      (c, c)

    case s : StringLiteral =>
      (s, s)

    case a @ ArrayAccess(i : IterationOffsetBegin, c : IntegerConstant, _) =>
      (a, a)

    case a @ ArrayAccess(i : IterationOffsetEnd, c : IntegerConstant, _) =>
      (a, a)

    case AdditionExpression(sums : ListBuffer[Expression]) =>
      sums.view.map(s => evalExpressionExtrema(s)).reduce { (x, y) =>
        (new AdditionExpression(x._1, y._1), new AdditionExpression(x._2, y._2))
      }

    case SubtractionExpression(l : Expression, r : Expression) =>
      val x = evalExpressionExtrema(l)
      val y = evalExpressionExtrema(r)
      (new SubtractionExpression(x._1, y._2), new SubtractionExpression(x._2, y._1))

    case MultiplicationExpression(facs : ListBuffer[Expression]) =>
      facs.view.map(s => evalExpressionExtrema(s)).reduce { (x, y) =>
        val a = new MultiplicationExpression(x._1, y._1)
        val b = new MultiplicationExpression(x._1, y._2)
        val c = new MultiplicationExpression(x._2, y._1)
        val d = new MultiplicationExpression(x._2, y._2)
        (new MinimumExpression(a, b, c, d), new MaximumExpression(a, b, c, d))
      }

    case DivisionExpression(l : Expression, r : Expression) =>
      val x = evalExpressionExtrema(l)
      val y = evalExpressionExtrema(r)
      val a = new DivisionExpression(x._1, y._1)
      val b = new DivisionExpression(x._1, y._2)
      val c = new DivisionExpression(x._2, y._1)
      val d = new DivisionExpression(x._2, y._2)
      (new MinimumExpression(a, b, c, d), new MaximumExpression(a, b, c, d))

    case ModuloExpression(l : Expression, r : Expression) =>
      val x = evalExpressionExtrema(l)
      val y = evalExpressionExtrema(r)
      val a = new ModuloExpression(x._1, y._1)
      val b = new ModuloExpression(x._1, y._2)
      val c = new ModuloExpression(x._2, y._1)
      val d = new ModuloExpression(x._2, y._2)
      (new MinimumExpression(a, b, c, d), new MaximumExpression(a, b, c, d))

    case MinimumExpression(l : ListBuffer[Expression]) =>
      l.view.map(e => evalExpressionExtrema(e)).reduce { (x, y) =>
        (new MinimumExpression(x._1, y._1), new MinimumExpression(x._2, y._2))
      }

    case MaximumExpression(l : ListBuffer[Expression]) =>
      l.view.map(e => evalExpressionExtrema(e)).reduce { (x, y) =>
        (new MaximumExpression(x._1, y._1), new MaximumExpression(x._2, y._2))
      }

    case OffsetIndex(minOffset, maxOffset, index, _) =>
      val x = evalExpressionExtrema(index)
      (new AdditionExpression(x._1, new IntegerConstant(minOffset)), new AdditionExpression(x._2, new IntegerConstant(maxOffset)))

    case _ =>
      throw new EvaluationException("unknown expression type for evaluation: " + expr.getClass)
  }

  /**
   * Completely evaluates an floating expression.
   * Only FloatConstants are allowed!
   * Other scalar constants or variable accesses lead to an EvaluationException.
   */
  def evalFloating(expr : Expression) : Double = expr match {
    case FloatConstant(v) => v
    case AdditionExpression(sums : ListBuffer[Expression]) => sums.view.map(s => evalFloating(s)).sum
    case SubtractionExpression(l : Expression, r : Expression) => evalFloating(l) - evalFloating(r)
    case MultiplicationExpression(facs : ListBuffer[Expression]) => facs.view.map(s => evalFloating(s)).product
    case DivisionExpression(l : Expression, r : Expression) => evalFloating(l) / evalFloating(r)
    case MinimumExpression(l : ListBuffer[Expression]) => l.view.map(e => evalFloating(e)).min
    case MaximumExpression(l : ListBuffer[Expression]) => l.view.map(e => evalFloating(e)).max
    case _ =>
      throw new EvaluationException("unknown expression type for evaluation: " + expr.getClass)
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
  def extractIntegralSum(expr : Expression) : mutable.HashMap[Expression, Long] = {
    extractIntegralSumRec(expr)
  }

  private def extractIntegralSumRec(expr : Expression) : mutable.HashMap[Expression, Long] = {

    var res : mutable.HashMap[Expression, Long] = null

    expr match {

      case IntegerConstant(i) =>
        res = new mutable.HashMap[Expression, Long]()
        res(constName) = i

      case VariableAccess(varName, _) =>
        res = new mutable.HashMap[Expression, Long]()
        res(VariableAccess(varName, Some(IntegerDatatype))) = 1L

      case StringLiteral(varName) =>
        res = new mutable.HashMap[Expression, Long]()
        res(VariableAccess(varName, Some(IntegerDatatype))) = 1L // ONLY VariableAccess in res keys, NO StringConstant

      case acc : ArrayAccess =>
        res = new mutable.HashMap[Expression, Long]()
        res(acc) = 1L

      case NegativeExpression(neg) =>
        res = extractIntegralSumRec(neg)
        for ((name : Expression, value : Long) <- extractIntegralSumRec(neg))
          res(name) = -value

      case AdditionExpression(summands) =>
        res = new mutable.HashMap[Expression, Long]()
        for (s <- summands)
          for ((name : Expression, value : Long) <- extractIntegralSumRec(s))
            res(name) = res.getOrElse(name, 0L) + value
        // opt:  (x/2) + (x%2)  ==>  (x+1)/2
        val toOpt = new mutable.HashMap[Expression, (DivisionExpression, ModuloExpression, Long)]()
        for ((ex, coeff) <- res) ex match {
          case divd @ DivisionExpression(x, IntegerConstant(2)) =>
            toOpt.get(x) match {
              case None => toOpt(x) = (divd, null, coeff)
              case Some((_, modd, c)) if c == coeff => toOpt(x) = (divd, modd, coeff)
              case Some(_) => toOpt -= x // coefficient is not matching...
            }
          case modd @ ModuloExpression(x, IntegerConstant(2)) =>
            toOpt.get(x) match {
              case None => toOpt(x) = (null, modd, coeff)
              case Some((divd, _, c)) if c == coeff => toOpt(x) = (divd, modd, coeff)
              case Some(_) => toOpt -= x // coefficient is not matching...
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
        var nonCstMap : mutable.HashMap[Expression, Long] = null
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
        res = new mutable.HashMap[Expression, Long]()
        if (nonCst.isEmpty)
          res(constName) = coeff
        else if (nonCst.length == 1)
          for ((name : Expression, value : Long) <- nonCstMap)
            res(name) = value * coeff
        else
          res(new MultiplicationExpression(nonCst.sortBy(_.prettyprint()))) = coeff

      case DivisionExpression(l, r) =>
        val tmp = extractIntegralSumRec(r)
        if (tmp.isEmpty)
          throw new EvaluationException("BOOM! (divide by zero)")
        if (!(tmp.size == 1 && tmp.contains(constName)))
          throw new EvaluationException("only constant divisor allowed yet")
        val divs : Long = tmp(constName)
        tmp.clear()
        res = new mutable.HashMap[Expression, Long]()
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
          case IntegerConstant(x) => (constName, x / divs)
          case DivisionExpression(x, IntegerConstant(divs2)) => (DivisionExpression(x, IntegerConstant(divs * divs2)), 1L)
          case AdditionExpression(ListBuffer(DivisionExpression(x, IntegerConstant(divs2)), IntegerConstant(const))) =>
            (simplifyIntegralExpr(DivisionExpression(x + IntegerConstant(const * divs2), IntegerConstant(divs * divs2))), 1L)
          case AdditionExpression(ListBuffer(IntegerConstant(const), DivisionExpression(x, IntegerConstant(divs2)))) =>
            (simplifyIntegralExpr(DivisionExpression(x + IntegerConstant(const * divs2), IntegerConstant(divs * divs2))), 1L)
          case divd => (DivisionExpression(divd, IntegerConstant(divs)), 1L)
        }
        res(name) = res.getOrElse(name, 0L) + update

      case FunctionCallExpression("floord", ListBuffer(l, r)) =>
        val tmp = extractIntegralSumRec(r)
        if (tmp.isEmpty)
          throw new EvaluationException("BOOM! (divide by zero)")
        if (!(tmp.size == 1 && tmp.contains(constName)))
          throw new EvaluationException("only constant divisor allowed yet")
        val divs : Long = tmp(constName)
        tmp.clear()
        res = new mutable.HashMap[Expression, Long]()
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
          case IntegerConstant(x) => (constName, if (x >= 0) x / divs else (x - divs + 1) / divs)
          case FunctionCallExpression("floord", ListBuffer(x, IntegerConstant(divs2))) =>
            (FunctionCallExpression("floord", ListBuffer(x, IntegerConstant(divs * divs2))), 1L)
          case AdditionExpression(ListBuffer(FunctionCallExpression("floord", ListBuffer(x, IntegerConstant(divs2))), IntegerConstant(const))) =>
            (simplifyIntegralExpr(FunctionCallExpression("floord", ListBuffer(x + IntegerConstant(const * divs2), IntegerConstant(divs * divs2)))), 1L)
          case AdditionExpression(ListBuffer(IntegerConstant(const), FunctionCallExpression("floord", ListBuffer(x, IntegerConstant(divs2))))) =>
            (simplifyIntegralExpr(FunctionCallExpression("floord", ListBuffer(x + IntegerConstant(const * divs2), IntegerConstant(divs * divs2)))), 1L)
          case divd => (FunctionCallExpression("floord", ListBuffer(divd, IntegerConstant(divs))), 1L)
        }
        res(name) = res.getOrElse(name, 0L) + update

      case ModuloExpression(l, r) =>
        val tmp = extractIntegralSumRec(r)
        if (tmp.isEmpty)
          throw new EvaluationException("BOOM! (divide by zero)")
        if (!(tmp.size == 1 && tmp.contains(constName)))
          throw new EvaluationException("only constant divisor allowed")
        val mod : Long = tmp(constName)
        res = new mutable.HashMap[Expression, Long]()
        val dividendMap : mutable.HashMap[Expression, Long] = extractIntegralSumRec(l).filter(elem => elem._2 % mod != 0L)
        val cstOpt = dividendMap.remove(constName) // const part can be reduced
        if (cstOpt.isDefined)
          dividendMap(constName) = (cstOpt.get % mod + mod) % mod // mathematical modulo
        val dividend : Expression = recreateExprFromIntSum(dividendMap)
        dividend match {
          case IntegerConstant(x) => res(constName) = x // const part is already result of modulo
          case _ => res(ModuloExpression(dividend, IntegerConstant(mod))) = 1L
        }

      case MinimumExpression(args : ListBuffer[Expression]) =>
        val exprs = new ListBuffer[Expression]
        var min : java.lang.Long = null
        for (arg <- args) simplifyIntegralExpr(arg) match {
          case IntegerConstant(c) => min = if (min == null || min > c) c else min
          case e if !exprs.contains(e) => exprs += e
          case _ => // we already found a (syntactically) indentical expression, so skip this one
        }
        res = new mutable.HashMap[Expression, Long]()
        if (exprs.isEmpty)
          res(constName) = min
        else {
          if (min != null)
            exprs += IntegerConstant(min)
          res(MinimumExpression(exprs)) = 1L
        }

      case MaximumExpression(args : ListBuffer[Expression]) =>
        val exprs = new ListBuffer[Expression]
        var max : java.lang.Long = null
        for (arg <- args) simplifyIntegralExpr(arg) match {
          case IntegerConstant(c) => max = if (max == null || max < c) c else max
          case e if !exprs.contains(e) => exprs += e
          case _ => // we already found a (syntactically) indentical expression, so skip this one
        }
        res = new mutable.HashMap[Expression, Long]()
        if (exprs.isEmpty)
          res(constName) = max
        else {
          if (max != null)
            exprs += IntegerConstant(max)
          res(MinimumExpression(exprs)) = 1L
        }

      case scalarIV : iv.InternalVariable if scalarIV.resolveDataType.isInstanceOf[ScalarDatatype] =>
        res = new mutable.HashMap[Expression, Long]()
        res(scalarIV) = 1L

      case anyIV : iv.InternalVariable =>
        Logger.warn(s"Found non-scalar iv ${anyIV.prettyprint()} in extractIntegralSumRec")
        res = new mutable.HashMap[Expression, Long]()
        res(anyIV) = 1L

      case offInd : OffsetIndex =>
        res = new mutable.HashMap[Expression, Long]()
        res(offInd) = 1L

      case _ =>
        throw new EvaluationException("unknown expression type for evaluation: " + expr.getClass)
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
  def recreateExprFromIntSum(sumMap : mutable.HashMap[Expression, Long]) : Expression = {

    val const : Long = sumMap.getOrElse(constName, 0L)

    val sumSeq = sumMap.view.filter(s => s._1 != constName && s._2 != 0L).toSeq.sortWith({
      case ((VariableAccess(v1, _), _), (VariableAccess(v2, _), _)) => v1 < v2
      case ((v1 : VariableAccess, _), _) => true
      case (_, (v2 : VariableAccess, _)) => false
      case ((e1, _), (e2, _)) => e1.prettyprint() < e2.prettyprint()
    })

    if (sumSeq.isEmpty)
      return IntegerConstant(const)

    // use distributive property
    val reverse = new mutable.HashMap[Long, (ListBuffer[Expression], ListBuffer[Expression])]()
    def empty = (new ListBuffer[Expression](), new ListBuffer[Expression]())
    for ((njuExpr : Expression, value : Long) <- sumSeq)
      if (value > 0L)
        reverse.getOrElseUpdate(value, empty)._1 += njuExpr
      else
        reverse.getOrElseUpdate(-value, empty)._2 += njuExpr

    val posSums = new ListBuffer[Expression]()
    val negSums = new ListBuffer[Expression]()

    def toExpr(sum : ListBuffer[Expression]) = if (sum.length == 1) sum.head else new AdditionExpression(sum)
    for ((value : Long, (pSums : ListBuffer[Expression], nSums : ListBuffer[Expression])) <- reverse.toSeq.sortBy(t => t._1).reverse)
      if (value == 1L) {
        posSums ++= pSums
        negSums ++= nSums
      } else {
        if (pSums.isEmpty)
          negSums += new MultiplicationExpression(IntegerConstant(value), toExpr(nSums))
        else if (nSums.isEmpty)
          posSums += new MultiplicationExpression(IntegerConstant(value), toExpr(pSums))
        else
          posSums += new MultiplicationExpression(IntegerConstant(value), new SubtractionExpression(toExpr(pSums), toExpr(nSums)))
      }

    if (const > 0L)
      posSums += IntegerConstant(const)
    else if (const < 0L)
      negSums += IntegerConstant(-const)

    if (posSums.isEmpty)
      new NegativeExpression(toExpr(negSums))
    else if (negSums.isEmpty)
      toExpr(posSums)
    else
      new SubtractionExpression(toExpr(posSums), toExpr(negSums))
  }

  def simplifyIntegralExpr(expr : Expression) : Expression = {
    try {
      val res = ExpressionStatement(recreateExprFromIntSum(extractIntegralSum(expr)))
      SimplifyStrategy.doUntilDoneStandalone(res)
      res.expression
    } catch {
      case ex : EvaluationException =>
        throw new EvaluationException(ex.msg + ";  in " + expr.prettyprint(), ex)
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
  def extractFloatingSum(expr : Expression) : mutable.HashMap[Expression, Double] = {
    extractFloatingSumRec(expr)
  }

  private def extractFloatingSumRec(expr : Expression) : mutable.HashMap[Expression, Double] = {

    var res : mutable.HashMap[Expression, Double] = null

    expr match {

      case IntegerConstant(i) =>
        res = new mutable.HashMap[Expression, Double]()
        res(constName) = i

      case FloatConstant(d) =>
        res = new mutable.HashMap[Expression, Double]()
        res(constName) = d

      case VariableAccess(varName, dt) =>
        res = new mutable.HashMap[Expression, Double]()
        res(VariableAccess(varName, dt.orElse(Some(RealDatatype)))) = 1d // preserve datatype if some

      case StringLiteral(varName) =>
        if (varName.contains("std::rand")) // HACK
          throw new EvaluationException("don't optimze code containing a call to std::rand")
        res = new mutable.HashMap[Expression, Double]()
        res(VariableAccess(varName, Some(RealDatatype))) = 1d // ONLY VariableAccess in res keys, NO StringLiteral

      case aAcc : ArrayAccess =>
        res = new mutable.HashMap[Expression, Double]()
        res(aAcc) = 1d

      case mAcc : MemberAccess =>
        res = new mutable.HashMap[Expression, Double]()
        res(mAcc) = 1d

      case fAcc : FieldAccessLike =>
        res = new mutable.HashMap[Expression, Double]()
        res(fAcc) = 1d

      case tAcc : TempBufferAccess =>
        res = new mutable.HashMap[Expression, Double]()
        res(tAcc) = 1d

      case call : FunctionCallExpression =>
        if (call.name.contains("std::rand")) // HACK
          throw new EvaluationException("don't optimze code containing a call to std::rand")
        def simplifyFloatingArgs(pars : Seq[Datatype]) : Unit = {
          call.arguments =
            pars.view.zip(call.arguments).map {
              case (RealDatatype, arg) =>
                simplifyFloatingExpr(arg)
              case (_, arg) =>
                arg
            }.to[ListBuffer]
        }
        if (MathFunctions.signatures.contains(call.name))
          simplifyFloatingArgs(MathFunctions.signatures(call.name)._1)
        else for (func <- StateManager.findFirst({ f : FunctionStatement => f.name == call.name }))
          simplifyFloatingArgs(func.parameters.view.map(_.dType.orNull))
        res = new mutable.HashMap[Expression, Double]()
        res(call) = 1d

      case NegativeExpression(neg) =>
        res = extractFloatingSumRec(neg)
        for ((name : Expression, value : Double) <- extractFloatingSumRec(neg))
          res(name) = -value

      case AdditionExpression(summands) =>
        res = new mutable.HashMap[Expression, Double]()
        for (s <- summands)
          for ((name : Expression, value : Double) <- extractFloatingSumRec(s))
            res(name) = res.getOrElse(name, 0d) + value

      case SubtractionExpression(l, r) =>
        res = extractFloatingSumRec(l)
        for ((name : Expression, value : Double) <- extractFloatingSumRec(r))
          res(name) = res.getOrElse(name, 0d) - value

      case MultiplicationExpression(facs) =>
        var coeff : Double = 1d
        val nonCst = new ListBuffer[mutable.HashMap[Expression, Double]]()
        for (f <- facs) {
          val map = extractFloatingSumRec(f)
          if (map.size == 1 && map.contains(constName) || map.isEmpty)
            coeff *= map.getOrElse(constName, 0d)
          else
            nonCst += map
        }
        res = new mutable.HashMap[Expression, Double]()
        if (nonCst.isEmpty)
          res(constName) = coeff
        else if (nonCst.length == 1)
          for ((name : Expression, value : Double) <- nonCst.head)
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
          res(new MultiplicationExpression(nonCst.map { sum => recreateExprFromFloatSum(sum) })) = coeff
        }

      case DivisionExpression(l, r) =>
        val mapL = extractFloatingSumRec(l)
        val mapR = extractFloatingSumRec(r)
        if (mapR.size == 1 && mapR.contains(constName)) {
          val div : Double = mapR(constName)
          res = mapL
          for ((name : Expression, value : Double) <- res)
            res(name) = value / div
        } else {
          var coefL : Double = 1d
          var exprL = recreateExprFromFloatSum(mapL)
          var coefR : Double = 1d
          var exprR = recreateExprFromFloatSum(mapR)
          exprL match {
            case MultiplicationExpression(ListBuffer(FloatConstant(coef), inner)) =>
              coefL = coef
              exprL = inner
            case FloatConstant(coef) =>
              coefL = coef
              exprL = new FloatConstant(1d)
            case _ =>
          }
          exprR match {
            case MultiplicationExpression(ListBuffer(FloatConstant(coef), inner)) =>
              coefR = coef
              exprR = inner
            case FloatConstant(coef) =>
              coefR = coef
              exprR = new FloatConstant(1d)
            case _ =>
          }
          res = new mutable.HashMap[Expression, Double]()
          val div = new DivisionExpression(exprL, exprR)
          res(div) = coefL / coefR
        }

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
        for (arg <- args) simplifyFloatingExpr(arg) match {
          case FloatConstant(c) => min = if (min == null || min > c) c else min
          case e if !exprs.contains(e) => exprs += e
          case _ => // we already found a (syntactically) indentical expression, so skip this one
        }
        res = new mutable.HashMap[Expression, Double]()
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
        for (arg <- args) simplifyFloatingExpr(arg) match {
          case FloatConstant(c) => max = if (max == null || max < c) c else max
          case e if !exprs.contains(e) => exprs += e
          case _ => // we already found a (syntactically) indentical expression, so skip this one
        }
        res = new mutable.HashMap[Expression, Double]()
        if (exprs.isEmpty)
          res(constName) = max
        else {
          if (max != null)
            exprs += FloatConstant(max)
          res(MaximumExpression(exprs)) = 1L
        }

      case _ =>
        throw new EvaluationException("unknown expression type for evaluation: " + expr.getClass + " in " + expr.prettyprint())
    }

    res.filter(e => e._2 != 0.0)
  }

  /**
   * Takes the output from extractFloatingSum(..) and recreates an AST for this sum.
   */
  def recreateExprFromFloatSum(sumMap : mutable.HashMap[Expression, Double]) : Expression = {

    val const : Double = sumMap.getOrElse(constName, 0d)

    val sumSeq = sumMap.view.filter(s => s._1 != constName && s._2 != 0d).toSeq.sortWith({
      case ((VariableAccess(v1, _), _), (VariableAccess(v2, _), _)) => v1 < v2
      case ((v1 : VariableAccess, _), _) => true
      case (_, (v2 : VariableAccess, _)) => false
      case ((e1, _), (e2, _)) => e1.prettyprint() < e2.prettyprint()
    })

    if (sumSeq.isEmpty)
      return FloatConstant(const)

    // use distributive property
    val reverse = new mutable.HashMap[Double, (ListBuffer[Expression], ListBuffer[Expression])]()
    def empty = (new ListBuffer[Expression](), new ListBuffer[Expression]())
    for ((njuExpr : Expression, value : Double) <- sumSeq)
      if (value > 0d)
        reverse.getOrElseUpdate(value, empty)._1 += njuExpr
      else
        reverse.getOrElseUpdate(-value, empty)._2 += njuExpr

    val posSums = new ListBuffer[Expression]()
    val negSums = new ListBuffer[Expression]()

    def toExpr(sum : ListBuffer[Expression]) = if (sum.length == 1) sum.head else new AdditionExpression(sum)
    for ((value : Double, (pSums : ListBuffer[Expression], nSums : ListBuffer[Expression])) <- reverse.toSeq.sortBy(t => t._1).reverse)
      if (value == 1d) {
        posSums ++= pSums
        negSums ++= nSums
      } else {
        if (pSums.isEmpty)
          negSums += new MultiplicationExpression(FloatConstant(value), toExpr(nSums))
        else if (nSums.isEmpty)
          posSums += new MultiplicationExpression(FloatConstant(value), toExpr(pSums))
        else
          posSums += new MultiplicationExpression(FloatConstant(value), new SubtractionExpression(toExpr(pSums), toExpr(nSums)))
      }

    if (const > 0d)
      posSums += FloatConstant(const)
    else if (const < 0d)
      negSums += FloatConstant(-const)

    if (posSums.isEmpty)
      new NegativeExpression(toExpr(negSums))
    else if (negSums.isEmpty)
      toExpr(posSums)
    else
      new SubtractionExpression(toExpr(posSums), toExpr(negSums))
  }

  def simplifyFloatingExpr(expr : Expression) : Expression = {
    try {
      val res = ExpressionStatement(recreateExprFromFloatSum(extractFloatingSum(expr)))
      SimplifyStrategy.doUntilDoneStandalone(res)
      res.expression
    } catch {
      case ex : EvaluationException =>
        throw new EvaluationException(ex.msg + ";  in " + expr.prettyprint(), ex)
    }
  }
}

case class EvaluationException(msg : String, cause : Throwable = null) extends Exception(msg, cause) {}
