package exastencils.strategies

import scala.collection.mutable.{ ArrayBuffer, HashMap, ListBuffer, Queue }

import exastencils.base.ir._
import exastencils.core._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.prettyprinting._

object PrintStrategy extends DefaultStrategy("Pretty-Print") {
  this += new Transformation("Pretty-Print", {
    case printable : FilePrettyPrintable =>
      printable.printToFile
      printable
  })
}

object ReplaceStringConstantsStrategy extends QuietDefaultStrategy("Replace something with something else") {
  var toReplace : String = ""
  var replacement : Node = LoopOverDimensions.defIt(Knowledge.dimensionality) // to be overwritten

  this += new Transformation("SearchAndReplace", {
    case StringLiteral(s) if s == toReplace => Duplicate(replacement)
  }, false)
}

object ExpandStrategy extends DefaultStrategy("Expanding") {
  def doUntilDone(node : Option[Node] = None) = {
    do { apply(node) }
    while (results.last._2.matches > 0) // FIXME: cleaner code
  }

  def doUntilDoneStandalone(node : Node) = {
    do { applyStandalone(node) }
    while (results.last._2.matches > 0) // FIXME: cleaner code
  }

  this += new Transformation("Hoho, expanding all day...", {
    case expandable : Expandable => expandable.expand
  })
}

object ExpandOnePassStrategy extends DefaultStrategy("Expanding") { // TODO: this strategy becomes somewhat obsolete as soon as trafos implement the required behavior directly
  this += new Transformation("Hoho, expanding all day...", {
    case expandable : Expandable => {
      var nodes : ListBuffer[Node] = ListBuffer()
      nodes += expandable
      var expandedSth = false
      do {
        expandedSth = false
        for (n <- 0 until nodes.length) {
          if (!expandedSth) {
            nodes(n) match {
              case expandable : Expandable =>
                val output = expandable.expand
                output.inner match {
                  case single : Node   => nodes.update(n, single)
                  case list : NodeList => {
                    val split = nodes.splitAt(n)
                    split._2.remove(0)
                    nodes = split._1 ++ list.nodes ++ split._2
                  }
                }
                expandedSth = true
              case _                       =>
            }
          }
        }
      } while (expandedSth)

      if (nodes.length == 1)
        nodes(0)
      else
        nodes
    }
  })
}

object SimplifyStrategy extends DefaultStrategy("Simplifying") {
  // hack: since AdditionExpression and MultiplicationExpression lead always to a match, we don't count these if nothing was changed
  private var negMatches : Int = 0
  private var compactAST : Boolean = false

  def doUntilDone(node : Option[Node] = None) = {
    do {
      negMatches = 0
      apply(node)
    } while (results.last._2.matches - negMatches > 0) // FIXME: cleaner code
  }

  def doUntilDoneStandalone(node : Node, compactAST : Boolean = false) = {
    this.compactAST = compactAST
    val oldLvl = Logger.getLevel
    Logger.setLevel(Logger.WARNING)
    do {
      negMatches = 0
      applyStandalone(node)
    } while (results.last._2.matches - negMatches > 0) // FIXME: cleaner code
    Logger.setLevel(oldLvl)
    this.compactAST = false
  }

  this += new Transformation("Improving the quality of some horrid code...", {

    case add : IR_AdditionExpression =>
      val nju = simplifyAdd(add.summands)
      if (nju == add)
        negMatches += 1
      nju

    case sub : IR_SubtractionExpression =>
      val nju = simplifyAdd(List(sub))
      if (nju == sub)
        negMatches += 1
      nju

    case mult : IR_MultiplicationExpression =>
      val nju = simplifyMult(mult.factors)
      if (nju == mult)
        negMatches += 1
      nju

    case old @ IR_NegativeExpression(IR_MultiplicationExpression(facs)) =>
      val nju = simplifyMult(facs.clone() += IntegerConstant(-1L))
      if (nju == old)
        negMatches += 1
      nju

    // deal with constants
    case IR_NegativeExpression(IntegerConstant(value)) => IntegerConstant(-value)
    case IR_NegativeExpression(FloatConstant(value))   => FloatConstant(-value)

    case IR_DivisionExpression(IntegerConstant(left), IntegerConstant(right)) => IntegerConstant(left / right)
    case IR_DivisionExpression(IntegerConstant(left), FloatConstant(right))   => FloatConstant(left / right)
    case IR_DivisionExpression(FloatConstant(left), IntegerConstant(right))   => FloatConstant(left / right)
    case IR_DivisionExpression(FloatConstant(left), FloatConstant(right))     => FloatConstant(left / right)

    case IR_DivisionExpression(left : IR_Expression, IntegerConstant(1))  => left
    case IR_DivisionExpression(left : IR_Expression, FloatConstant(f))    => IR_MultiplicationExpression(left, FloatConstant(1.0 / f))
    case IR_DivisionExpression(FloatConstant(0.0), right : IR_Expression) => FloatConstant(0.0)
    case IR_DivisionExpression(IntegerConstant(0), right : IR_Expression) => IntegerConstant(0)

    case IR_ModuloExpression(IntegerConstant(left), IntegerConstant(right)) => IntegerConstant(left % right)

    case IR_PowerExpression(IntegerConstant(base), IntegerConstant(exp)) => IntegerConstant(pow(base, exp))
    case IR_PowerExpression(FloatConstant(base), IntegerConstant(exp))   => FloatConstant(pow(base, exp))
    case IR_PowerExpression(IntegerConstant(base), FloatConstant(exp))   => FloatConstant(math.pow(base, exp))
    case IR_PowerExpression(FloatConstant(base), FloatConstant(exp))     => FloatConstant(math.pow(base, exp))

    case IR_PowerExpression(base, IntegerConstant(0))                        => IntegerConstant(1)
    case IR_PowerExpression(base, IntegerConstant(1))                        => base
    case IR_PowerExpression(base, IntegerConstant(e)) if (e >= 2 && e <= 6)  => IR_MultiplicationExpression(ListBuffer.fill(e.toInt)(Duplicate(base)))
    case IR_PowerExpression(b, FloatConstant(e)) if (e.toLong.toDouble == e) => IR_PowerExpression(b, IntegerConstant(e.toLong))

    // deal with negatives
    case IR_NegativeExpression(IR_NegativeExpression(expr))           => expr
    case IR_NegativeExpression(IR_AdditionExpression(sums))           => IR_AdditionExpression(sums.transform { s => IR_NegativeExpression(s) })
    case IR_NegativeExpression(IR_SubtractionExpression(left, right)) => IR_SubtractionExpression(right, left)

    case IR_DivisionExpression(IR_NegativeExpression(l), IR_NegativeExpression(r)) => IR_DivisionExpression(l, r)
    case IR_DivisionExpression(l, IR_NegativeExpression(r))                        => IR_NegativeExpression(IR_DivisionExpression(l, r))
    case IR_DivisionExpression(IR_NegativeExpression(l), r)                        => IR_NegativeExpression(IR_DivisionExpression(l, r))

    case IR_NegativeExpression(IR_MaximumExpression(exps)) => IR_MinimumExpression(exps.transform { s => IR_NegativeExpression(s) })
    case IR_NegativeExpression(IR_MinimumExpression(exps)) => IR_MaximumExpression(exps.transform { s => IR_NegativeExpression(s) })

    // Simplify vectors
    case IR_NegativeExpression(v : VectorExpression) =>
      VectorExpression(v.datatype, v.expressions.map { x => IR_NegativeExpression(x) }, v.rowVector)

    // Simplify matrices
    case IR_NegativeExpression(m : MatrixExpression) =>
      MatrixExpression(m.datatype, m.expressions.map { x => x.map { y => IR_NegativeExpression(y) : IR_Expression } })

    case Scope(ListBuffer(Scope(body))) => Scope(body)

    case ConditionStatement(cond, ListBuffer(Scope(trueBody)), falseBody)  => ConditionStatement(cond, trueBody, falseBody)
    case ConditionStatement(cond, trueBody, ListBuffer(Scope(falseBody)))  => ConditionStatement(cond, trueBody, falseBody)
    case l @ ForLoopStatement(beg, end, inc, ListBuffer(Scope(body)), red) =>
      l.body = body; l // preserve ForLoopStatement instance to ensure all traits are still present

    case IR_EqEqExpression(IntegerConstant(left), IntegerConstant(right))         => BooleanConstant(left == right)
    case IR_NeqExpression(IntegerConstant(left), IntegerConstant(right))          => BooleanConstant(left != right)
    case IR_LowerExpression(IntegerConstant(left), IntegerConstant(right))        => BooleanConstant(left < right)
    case IR_LowerEqualExpression(IntegerConstant(left), IntegerConstant(right))   => BooleanConstant(left <= right)
    case IR_GreaterExpression(IntegerConstant(left), IntegerConstant(right))      => BooleanConstant(left > right)
    case IR_GreaterEqualExpression(IntegerConstant(left), IntegerConstant(right)) => BooleanConstant(left >= right)

    case IR_NegationExpression(BooleanConstant(b)) => BooleanConstant(!b)

    case IR_NegationExpression(IR_EqEqExpression(left, right)) => IR_NeqExpression(left, right)
    case IR_NegationExpression(IR_NeqExpression(left, right))  => IR_EqEqExpression(left, right)

    case IR_NegationExpression(IR_LowerExpression(left, right))        => IR_GreaterEqualExpression(left, right)
    case IR_NegationExpression(IR_GreaterEqualExpression(left, right)) => IR_LowerExpression(left, right)
    case IR_NegationExpression(IR_LowerEqualExpression(left, right))   => IR_GreaterExpression(left, right)
    case IR_NegationExpression(IR_GreaterExpression(left, right))      => IR_LowerEqualExpression(left, right)

    case IR_NegationExpression(IR_AndAndExpression(left, right)) => IR_OrOrExpression(IR_NegationExpression(left), IR_NegationExpression(right))
    case IR_NegationExpression(IR_OrOrExpression(left, right))   => IR_AndAndExpression(IR_NegationExpression(left), IR_NegationExpression(right))

    case IR_AndAndExpression(BooleanConstant(true), expr : IR_Expression)  => expr
    case IR_AndAndExpression(expr : IR_Expression, BooleanConstant(true))  => expr
    case IR_AndAndExpression(BooleanConstant(false), expr : IR_Expression) => BooleanConstant(false)
    case IR_AndAndExpression(expr : IR_Expression, BooleanConstant(false)) => BooleanConstant(false)

    case IR_OrOrExpression(BooleanConstant(true), expr : IR_Expression)  => BooleanConstant(true)
    case IR_OrOrExpression(expr : IR_Expression, BooleanConstant(true))  => BooleanConstant(true)
    case IR_OrOrExpression(BooleanConstant(false), expr : IR_Expression) => expr
    case IR_OrOrExpression(expr : IR_Expression, BooleanConstant(false)) => expr

    case ConditionStatement(BooleanConstant(cond), tBranch, fBranch) => {
      if (cond) {
        if (tBranch.isEmpty) NullStatement else tBranch
      } else {
        if (fBranch.isEmpty) NullStatement else fBranch
      }
    }
  })

  private def simplifyAdd(sum : Seq[IR_Expression]) : IR_Expression = {
    var intCst : Long = 0L
    var floatCst : Double = 0d
    var vecExpr : VectorExpression = null
    var vecPos : Boolean = true
    val workQ = new Queue[(IR_Expression, Boolean)]()
    val posSums = new ListBuffer[IR_Expression]()
    val negSums = new ListBuffer[IR_Expression]()
    for (s <- sum) {
      workQ.enqueue((s, true)) // for nested AdditionExpressions; this allows in-order processing
      do {
        val (expr, pos) = workQ.dequeue()
        expr match {
          case IntegerConstant(i)                    => if (pos) intCst += i else intCst -= i
          case FloatConstant(f)                      => if (pos) floatCst += f else floatCst -= f
          case IR_AdditionExpression(sums)           => workQ.enqueue(sums.view.map { x => (x, pos) } : _*)
          case IR_NegativeExpression(e)              => workQ.enqueue((e, !pos))
          case IR_SubtractionExpression(left, right) =>
            workQ.enqueue((left, pos))
            workQ.enqueue((right, !pos))
          // if some more simplifications with vectors or matrices are required, a similar approach than for a
          //    MultiplicationExpression is possible here
          case v : VectorExpression =>
            if (vecExpr == null) {
              vecPos = pos
              vecExpr = v
            } else {
              if (vecExpr.rowVector.getOrElse(true) != v.rowVector.getOrElse(true))
                Logger.error("Vector types must match for addition")
              if (vecExpr.length != v.length)
                Logger.error("Vector sizes must match for addition")
              val vecExprsView = if (vecPos) vecExpr.expressions.view else vecExpr.expressions.view.map { x => IR_NegationExpression(x) }
              val vExprs = if (pos) v.expressions else v.expressions.view.map { x => IR_NegationExpression(x) }
              vecExpr =
                VectorExpression(GetResultingDatatype(vecExpr.datatype, v.datatype),
                  vecExprsView.zip(vExprs).map { x => x._1 + x._2 : IR_Expression }.to[ListBuffer],
                  if (vecExpr.rowVector.isDefined) vecExpr.rowVector else v.rowVector)
            }
          case e : IR_Expression    =>
            if (pos)
              posSums += e
            else
              negSums += e
        }
      } while (!workQ.isEmpty)
    }

    // add constant at last position
    if (floatCst != 0d) {
      val cst : Double = floatCst + intCst
      // if compactAST is set, no SubtractionExpression is created, so prevent creating a Neg(Const),
      //   which would lead to a non-terminating recursion
      // if posSums is empty we do not want to add the constant to the negSums, which would also result in a Neg(Const) -> non-terminating
      if (cst > 0.0 || compactAST || posSums.isEmpty)
        posSums += FloatConstant(cst)
      else
        negSums += FloatConstant(-cst)
    } else if (intCst != 0L)
    // if compactAST is set, no SubtractionExpression is created, so prevent creating a Neg(Const),
    //   which would lead to a non-terminating recursion
    // if posSums is empty we do not want to add the constant to the negSums, which would also result in a Neg(Const) -> non-terminating
      if (intCst > 0 || compactAST || posSums.isEmpty)
        posSums += IntegerConstant(intCst)
      else
        negSums += IntegerConstant(-intCst)

    if (vecExpr != null) {
      if (posSums.isEmpty && negSums.isEmpty)
        return vecExpr
      else
        Logger.error("Unable to add VectorExpression with other Expression types")

    } else if (posSums.length + negSums.length <= 1) { // result is only one summand (either a positive, or a negative, or 0)
      return (posSums ++= negSums.transform(x => IR_NegativeExpression(x)) += IntegerConstant(0L)).head

    } else if (posSums.length * negSums.length == 0 || compactAST) { // if compactAST is set do not create any SubtractionExpression
      return IR_AdditionExpression(posSums ++= negSums.transform(x => IR_NegativeExpression(x)))

    } else {
      val posExpr = if (posSums.length == 1) posSums.head else new IR_AdditionExpression(posSums)
      val negExpr = if (negSums.length == 1) negSums.head else new IR_AdditionExpression(negSums)
      return IR_SubtractionExpression(posExpr, negExpr)
    }
  }

  private def simplifyMult(facs : Seq[IR_Expression]) : IR_Expression = {
    var intCst : Long = 1L
    var floatCst : Double = 1d
    val workQ = new Queue[IR_Expression]()
    val remA = new ArrayBuffer[IR_Expression]() // use ArrayBuffer here for a more efficient access to the last element
    var div : IR_DivisionExpression = null
    for (f <- facs) {
      workQ.enqueue(f) // for nested MultiplicationExpression; this allows in-order processing
      do {
        val expr = workQ.dequeue()
        expr match {
          case IntegerConstant(iv)                             => intCst *= iv
          case FloatConstant(fv)                               => floatCst *= fv
          case IR_NegativeExpression(e)                        =>
            workQ.enqueue(e)
            intCst = -intCst
          case IR_MultiplicationExpression(iFacs)              =>
            workQ.enqueue(iFacs : _*)
          case d @ IR_DivisionExpression(FloatConstant(fv), _) =>
            floatCst *= fv
            d.left = FloatConstant(1.0)
            if (div == null)
              div = d
            remA += d
          case _ : VectorExpression | _ : MatrixExpression     =>
            if (remA.isEmpty)
              remA += expr
            else
            // merging with one previous only is sufficient, if simplifyMult only matches first arg with vect/mat types
              remA ++= simplifyBinMult(remA.last, expr)
          case r : IR_Expression                               =>
            remA += r
        }
      } while (!workQ.isEmpty)
    }
    val rem = remA.to[ListBuffer]
    var cstDt : Option[IR_Datatype] = None
    val negative : Boolean = floatCst * intCst < 0d
    floatCst = math.abs(floatCst)
    intCst = math.abs(intCst)
    if (floatCst * intCst == 0d) {
      rem.clear()
      rem += new IntegerConstant(0L) // TODO: fix type
    } else if (div != null) {
      div.left = FloatConstant(floatCst * intCst)
    } else if (floatCst != 1d) {
      FloatConstant(floatCst * intCst) +=: rem // add constant at first position (it is expected as rem.head later)
      cstDt = Some(IR_RealDatatype)
    } else if (intCst != 1L) {
      IntegerConstant(intCst) +=: rem // add constant at first position (it is expected as rem.head later)
      cstDt = Some(IR_IntegerDatatype)
    }

    var result : IR_Expression = null
    if (rem.isEmpty) {
      result = IntegerConstant(1L) // TODO: fix type

    } else if (rem.length == 1 || floatCst * intCst == 0d) {
      result = rem.head

    } else {
      if (cstDt.isDefined) {
        var found : Boolean = false
        val coeff : IR_Expression = rem.head // this must be the constant factor (as added a few lines above)
        rem.transform {
          case v : VectorExpression if (!found) =>
            found = true
            VectorExpression(GetResultingDatatype(cstDt, v.datatype), v.expressions.map(Duplicate(coeff) * _), v.rowVector)
          case m : MatrixExpression if (!found) =>
            found = true
            MatrixExpression(GetResultingDatatype(cstDt, m.datatype), m.expressions.map(_.map(Duplicate(coeff) * _ : IR_Expression)))
          case x                                => x
        }
        if (found)
          rem.remove(0)
      }
      result = IR_MultiplicationExpression(rem)
    }

    if (negative)
      result = IR_NegativeExpression(result)
    return result
  }

  private def simplifyBinMult(le : IR_Expression, ri : IR_Expression) : Seq[IR_Expression] = {
    (le, ri) match { // matching for constants is not required here (this is already handled by the caller)
      case (left : VectorExpression, right : VectorExpression) =>
        if (left.length != right.length) Logger.error("Vector sizes must match for multiplication")
        if (left.rowVector.getOrElse(true) != right.rowVector.getOrElse(true)) Logger.error("Vector types must match for multiplication")
        List(IR_AdditionExpression(left.expressions.view.zip(right.expressions).map { x => x._1 * x._2 : IR_Expression }.to[ListBuffer]))
      case (left, right)                                       =>
        List(left, right)
    }
  }

  private def pow(a : Long, b : Long) : Long = {
    if (b < 0)
      return 0
    var base : Long = a
    var exp : Long = b
    var res : Long = 1
    while (exp > 0) {
      if ((exp & 1) != 0)
        res *= base
      exp >>= 1
      base *= base
    }
    return res
  }

  private def pow(a : Double, b : Long) : Double = {
    if (b < 0)
      return 0
    var base : Double = a
    var exp : Long = b
    var res : Double = 1
    while (exp > 0) {
      if ((exp & 1) != 0)
        res *= base
      exp >>= 1
      base *= base
    }
    return res
  }
}

object CleanUnusedStuff extends DefaultStrategy("Cleaning up unused stuff") {
  // TODO: think about inlining
  // TODO: this currently disregards parameters

  var emptyFunctions = ListBuffer[String]()

  override def apply(node : Option[Node] = None) = {
    emptyFunctions.clear
    super.apply(node)
  }

  this += new Transformation("Looking for deletable objects", {
    case FunctionStatement(_, fName, _, ListBuffer(), _, _, _) => {
      emptyFunctions += fName
      List()
    }
  })

  this += new Transformation("Removing obsolete references", {
    case FunctionCallExpression(fName, _) if emptyFunctions.contains(fName) => NullExpression
  })

  this += new Transformation("Removing empty scopes", {
    case Scope(ListBuffer()) => None
  })

  //  this += new Transformation("Removing null-statements", {
  //    case ExpressionStatement(NullExpression) => List()
  //    case NullStatement                       => List()
  //  })
}

object UnifyInnerTypes extends DefaultStrategy("Unify inner types of (constant) vectors and matrices") {
  var vectors = ListBuffer[VectorExpression]()
  var matrices = ListBuffer[MatrixExpression]()

  override def apply(applyAtNode : Option[Node]) = {
    this.execute(new Transformation("Find vectors and matrices", {
      case x : VectorExpression =>
        vectors.+=(x); x
      case x : MatrixExpression => matrices.+=(x); x
    }))

    vectors.foreach(vector => {
      if (vector.isConstant) {
        val reals = vector.expressions.count(_.isInstanceOf[FloatConstant])
        val ints = vector.expressions.count(_.isInstanceOf[IntegerConstant])
        if (ints > 0 && reals > 0) {
          vector.expressions = vector.expressions.map(e => if (e.isInstanceOf[FloatConstant]) e; else FloatConstant(e.asInstanceOf[IntegerConstant].v))
        }
      }
    })

    matrices.foreach(matrix => {
      if (matrix.isConstant) {
        val reals = matrix.expressions.flatten[IR_Expression].count(_.isInstanceOf[FloatConstant])
        val ints = matrix.expressions.flatten[IR_Expression].count(_.isInstanceOf[IntegerConstant])
        if (ints > 0 && reals > 0) {
          matrix.expressions = matrix.expressions.map(_.map(e => if (e.isInstanceOf[FloatConstant]) e; else FloatConstant(e.asInstanceOf[IntegerConstant].v)))
        }
      }
    })
  }
}

object GatherFieldAccessOffsets extends QuietDefaultStrategy("Gathering field access offsets honoring reference offsets") {
  var accesses = HashMap[String, ListBuffer[MultiIndex]]()

  def addAccess(key : String, index : MultiIndex) = {
    if (!accesses.contains(key)) accesses.put(key, ListBuffer())
    accesses(key) += index
  }

  this += new Transformation("TODO", {
    case fa : FieldAccess        =>
      addAccess(fa.fieldSelection.field.codeName, fa.index - LoopOverDimensions.defIt(fa.index.length))
      fa
    case dfa : DirectFieldAccess =>
      addAccess(dfa.fieldSelection.field.codeName, dfa.index - dfa.fieldSelection.field.referenceOffset - LoopOverDimensions.defIt(dfa.index.length))
      dfa
  })
}
