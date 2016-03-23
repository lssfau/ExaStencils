package exastencils.strategies

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
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
            if (nodes(n).isInstanceOf[Expandable]) {
              var output = nodes(n).asInstanceOf[Expandable].expand
              output.inner match {
                case single : Node => nodes.update(n, single)
                case list : NodeList => {
                  val split = nodes.splitAt(n)
                  split._2.remove(0)
                  nodes = split._1 ++ list.nodes ++ split._2
                }
              }
              expandedSth = true
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
  var negMatches : Int = 0

  def doUntilDone(node : Option[Node] = None) = {
    do {
      negMatches = 0
      apply(node)
    } while (results.last._2.matches - negMatches > 0) // FIXME: cleaner code
  }

  def doUntilDoneStandalone(node : Node) = {
    val oldLvl = Logger.getLevel
    Logger.setLevel(Logger.WARNING)
    do {
      negMatches = 0
      applyStandalone(node)
    } while (results.last._2.matches - negMatches > 0) // FIXME: cleaner code
    Logger.setLevel(oldLvl)
  }

  this += new Transformation("Improving the quality of some horrid code...", {
    // FIXME: for re-runs only the number of replacements of the last trafo is checked, thus only one big trafo (should also improve performance)
    // TODO: extend for general data types; extend with missing cases; extend for left-right-switched cases

    case add : AdditionExpression => {
      // TODO: add distributive law?
      var intCst : Long = 0L
      var floatCst : Double = 0d
      var vecExpr : VectorExpression = null
      var vecPos : Boolean = true
      val workQ = new Queue[(Expression, Boolean)]()
      val posSums = new ListBuffer[Expression]()
      val negSums = new ListBuffer[Expression]()
      for (s <- add.summands) {
        workQ.enqueue((s, true)) // for nested AdditionExpressions; this allows in-order processing
        do {
          val (expr, pos) = workQ.dequeue()
          expr match {
            case IntegerConstant(i)       => if (pos) intCst += i else intCst -= i
            case FloatConstant(f)         => if (pos) floatCst += f else floatCst -= f
            case AdditionExpression(sums) => workQ.enqueue(sums.view.map { x => (x, pos) } : _*)
            case NegationExpression(e) =>
              workQ.enqueue((e, !pos))
            case SubtractionExpression(left, right) =>
              workQ.enqueue((left, pos))
              workQ.enqueue((right, !pos))
            // if some more simplifications with vectors or matrices are required, a similar approach than for a
            //    MultiplicationExpression is possible here
            case v : VectorExpression =>
              if (vecExpr == null) {
                vecPos = pos
                vecExpr = v
              } else {
                if (vecExpr.rowVector != v.rowVector)
                  Logger.error("Vector types must match for addition")
                if (vecExpr.length != v.length)
                  Logger.error("Vector sizes must match for addition")
                val vecExprsView = if (vecPos) vecExpr.expressions.view else vecExpr.expressions.view.map { x => NegationExpression(x) }
                val vExprs = if (pos) v.expressions else v.expressions.view.map { x => NegationExpression(x) }
                vecExpr =
                  VectorExpression(GetResultingDatatype("+", vecExpr.datatype, v.datatype),
                    vecExprsView.zip(vExprs).map { x => x._1 + x._2 : Expression }.to[ListBuffer],
                    vecExpr.rowVector)
              }
            case e : Expression =>
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
        if (cst > 0.0)
          posSums += FloatConstant(cst)
        else
          negSums += FloatConstant(-cst)
      } else if (intCst != 0L)
        if (intCst > 0)
          posSums += IntegerConstant(intCst)
        else
          negSums += IntegerConstant(-intCst)

      var result : Expression = null
      if (vecExpr != null) {
        if (posSums.isEmpty && negSums.isEmpty)
          result = vecExpr
        else
          Logger.error("Unable to add VectorExpression with other Expression types")

      } else if (posSums.length + negSums.length <= 1) { // result is only one summand (either a positive, or a negative, or 0)
        result = (posSums ++= negSums.transform(x => NegativeExpression(x)) += IntegerConstant(0L)).head

      } else if (posSums.length * negSums.length == 0) {
        result = AdditionExpression(posSums ++= negSums.transform(x => NegativeExpression(x)))

      } else {
        val posExpr = if (posSums.length == 1) posSums.head else new AdditionExpression(posSums)
        val negExpr = if (negSums.length == 1) negSums.head else new AdditionExpression(negSums)
        result = new SubtractionExpression(posExpr, negExpr)
      }

      if (result == add)
        negMatches += 1
      result
    }

    case mult : MultiplicationExpression => {
      var intCst : Long = 1L
      var floatCst : Double = 1d
      val workQ = new Queue[Expression]()
      val remA = new ArrayBuffer[Expression]() // use ArrayBuffer here for a more efficient access to the last element
      var div : DivisionExpression = null
      for (f <- mult.factors) {
        workQ.enqueue(f) // for nested MultiplicationExpression; this allows in-order processing
        do {
          val expr = workQ.dequeue()
          expr match {
            case IntegerConstant(i) => intCst *= i
            case FloatConstant(f)   => floatCst *= f
            case NegativeExpression(e) =>
              workQ.enqueue(e)
              intCst = -intCst
            case MultiplicationExpression(facs) =>
              workQ.enqueue(facs : _*)
            case d @ DivisionExpression(FloatConstant(f), _) =>
              floatCst *= f
              d.left = FloatConstant(1.0)
              if (div == null)
                div = d
              remA += d
            case _ : VectorExpression | _ : MatrixExpression =>
              if (remA.isEmpty)
                remA += expr
              else
                // merging with one previous only is sufficient, if simplifyMult only matches first arg with vect/mat types
                remA ++= simplifyMult(remA.last, expr)
            case r : Expression =>
              remA += r
          }
        } while (!workQ.isEmpty)
      }
      val rem = remA.to[ListBuffer]
      var cstDt : Option[Datatype] = None
      if (div != null)
        div.left = FloatConstant(floatCst * intCst)
      else if (floatCst != 1d) {
        FloatConstant(floatCst * intCst) +=: rem // add constant at first position (it is expected as rem.head later)
        cstDt = Some(RealDatatype)
      } else if (intCst != 1L) {
        IntegerConstant(intCst) +=: rem // add constant at first position (it is expected as rem.head later)
        cstDt = Some(IntegerDatatype)
      }

      var result : Expression = null
      if (rem.isEmpty) {
        result = IntegerConstant(1L)

      } else if (rem.length == 1 || floatCst * intCst == 0d) {
        result = rem.head

      } else {
        if (cstDt.isDefined) {
          var found : Boolean = false
          val coeff : Expression = rem.head // this must be the constant factor (as added a few lines above)
          rem.transform {
            case v : VectorExpression if (!found) =>
              found = true
              VectorExpression(GetResultingDatatype("*", cstDt, v.datatype), v.expressions.map(Duplicate(coeff) * _), v.rowVector)
            case m : MatrixExpression if (!found) =>
              found = true
              MatrixExpression(GetResultingDatatype("*", cstDt, m.datatype), m.expressions.map(_.map(Duplicate(coeff) * _ : Expression)))
            case x => x
          }
          if (found)
            rem.remove(0)
        }
        result = MultiplicationExpression(rem)
      }

      if (result == mult)
        negMatches += 1
      result
    }

    case NegativeExpression(IntegerConstant(value))                           => IntegerConstant(-value)
    case NegativeExpression(FloatConstant(value))                             => FloatConstant(-value)

    case NegativeExpression(NegativeExpression(expr))                         => expr
    case NegativeExpression(AdditionExpression(sums))                         => AdditionExpression(sums.map { s => NegativeExpression(s) })
    case NegativeExpression(SubtractionExpression(left, right))               => SubtractionExpression(right, left)
    case NegativeExpression(MultiplicationExpression(facs))                   => MultiplicationExpression(facs += IntegerConstant(-1))

    case SubtractionExpression(IntegerConstant(left), IntegerConstant(right)) => IntegerConstant(left - right)
    case SubtractionExpression(FloatConstant(left), FloatConstant(right))     => FloatConstant(left - right)
    case SubtractionExpression(IntegerConstant(left), FloatConstant(right))   => FloatConstant(left - right)
    case SubtractionExpression(FloatConstant(left), IntegerConstant(right))   => FloatConstant(left - right)

    case DivisionExpression(IntegerConstant(left), IntegerConstant(right))    => IntegerConstant(left / right)
    case DivisionExpression(IntegerConstant(left), FloatConstant(right))      => FloatConstant(left / right)
    case DivisionExpression(FloatConstant(left), IntegerConstant(right))      => FloatConstant(left / right)
    case DivisionExpression(FloatConstant(left), FloatConstant(right))        => FloatConstant(left / right)

    case SubtractionExpression(left : Expression, IntegerConstant(0))         => left
    case SubtractionExpression(left : Expression, FloatConstant(0))           => left
    case SubtractionExpression(IntegerConstant(0), right : Expression)        => NegativeExpression(right)
    case SubtractionExpression(FloatConstant(0), right : Expression)          => NegativeExpression(right)

    case DivisionExpression(left : Expression, IntegerConstant(1))            => left
    case DivisionExpression(left : Expression, FloatConstant(f))              => new MultiplicationExpression(left, FloatConstant(1.0 / f))
    case DivisionExpression(FloatConstant(0.0), right : Expression)           => FloatConstant(0.0)
    case DivisionExpression(IntegerConstant(0), right : Expression)           => IntegerConstant(0)

    case ModuloExpression(IntegerConstant(left), IntegerConstant(right))      => IntegerConstant(left % right)

    case SubtractionExpression(left, IntegerConstant(right)) if (right < 0)   => new AdditionExpression(left, IntegerConstant(-right))
    case SubtractionExpression(left, FloatConstant(right)) if (right < 0)     => new AdditionExpression(left, FloatConstant(-right))

    case SubtractionExpression(NegativeExpression(l), NegativeExpression(r))  => new SubtractionExpression(r, l)
    case SubtractionExpression(NegativeExpression(left), right)               => new AdditionExpression(NegativeExpression(left), NegativeExpression(right))
    case SubtractionExpression(left, NegativeExpression(right))               => new AdditionExpression(left, right)
    case SubtractionExpression(SubtractionExpression(pos, neg1), neg2)        => new SubtractionExpression(pos, new AdditionExpression(neg1, neg2))
    case SubtractionExpression(pos1, SubtractionExpression(neg, pos2)) =>
      new SubtractionExpression(new AdditionExpression(pos1, pos2), neg)

    // Simplify vectors
    case NegativeExpression(v : VectorExpression) =>
      VectorExpression(v.datatype, v.expressions.map { x => NegativeExpression(x) }, v.rowVector)
    //    case AdditionExpression(left : VectorExpression, right : VectorExpression) => {
    //      if (left.rowVector.getOrElse(true) != right.rowVector.getOrElse(true)) Logger.error("Vector types must match for addition")
    //      if (left.length != right.length) Logger.error("Vector sizes must match for addition")
    //      VectorExpression(GetResultingDatatype(left.datatype, right.datatype), (left.expressions, right.expressions).zipped.map(_ + _), left.rowVector.getOrElse(right.rowVector).asInstanceOf[Option[Boolean]])
    //    }
    case SubtractionExpression(left, right : VectorExpression) =>
      new AdditionExpression(left, NegativeExpression(right))
    //    case MultiplicationExpression(v : VectorExpression, c : IntegerConstant) => {
    //      VectorExpression(v.datatype, v.expressions.map(c * _), v.rowVector)
    //    }
    //    case MultiplicationExpression(v : VectorExpression, c : FloatConstant) => {
    //      VectorExpression(GetResultingDatatype(v.datatype, Some(RealDatatype)), v.expressions.map(c * _), v.rowVector)
    //    }
    //    case MultiplicationExpression(c : IntegerConstant, v : VectorExpression) => {
    //      VectorExpression(v.datatype, v.expressions.map(c * _), v.rowVector)
    //    }
    //    case MultiplicationExpression(c : FloatConstant, v : VectorExpression) => {
    //      VectorExpression(GetResultingDatatype(Some(RealDatatype), v.datatype), v.expressions.map(c * _), v.rowVector)
    //    }
    //    case MultiplicationExpression(left : VectorExpression, right : VectorExpression) => {
    //      if (left.length != right.length) Logger.error("Vector sizes must match for multiplication")
    //      if (left.rowVector.getOrElse(true) != right.rowVector.getOrElse(true)) Logger.error("Vector types must match for multiplication")
    //      val t = (left.expressions, right.expressions).zipped.map(_ * _)
    //      t.reduce((a : Expression, b : Expression) => a + b)
    //    }
    case x : FunctionCallExpression if (x.name == "transpose") => {
      x.arguments(0) match {
        case x : VectorExpression =>
          x.rowVector = !x.rowVector; x
        case x : MatrixExpression =>
          x.expressions.transpose; x
      }
      x
    }

    // Simplify matrices
    case NegativeExpression(m : MatrixExpression) =>
      MatrixExpression(m.datatype, m.expressions.map { x => x.map { y => NegativeExpression(y) : Expression } })
    case SubtractionExpression(left, right : MatrixExpression) =>
      new AdditionExpression(left, NegativeExpression(right))

    case Scope(ListBuffer(Scope(body)))                                        => Scope(body)
    case ConditionStatement(cond, ListBuffer(Scope(trueBody)), falseBody)      => ConditionStatement(cond, trueBody, falseBody)
    case ConditionStatement(cond, trueBody, ListBuffer(Scope(falseBody)))      => ConditionStatement(cond, trueBody, falseBody)
    case ForLoopStatement(beg, end, inc, ListBuffer(Scope(body)), red)         => ForLoopStatement(beg, end, inc, body, red)

    case EqEqExpression(IntegerConstant(left), IntegerConstant(right))         => BooleanConstant(left == right)
    case NeqExpression(IntegerConstant(left), IntegerConstant(right))          => BooleanConstant(left != right)
    case LowerExpression(IntegerConstant(left), IntegerConstant(right))        => BooleanConstant(left < right)
    case LowerEqualExpression(IntegerConstant(left), IntegerConstant(right))   => BooleanConstant(left <= right)
    case GreaterExpression(IntegerConstant(left), IntegerConstant(right))      => BooleanConstant(left > right)
    case GreaterEqualExpression(IntegerConstant(left), IntegerConstant(right)) => BooleanConstant(left >= right)

    case NegationExpression(BooleanConstant(b))                                => BooleanConstant(!b)

    case NegationExpression(EqEqExpression(left, right))                       => NeqExpression(left, right)
    case NegationExpression(NeqExpression(left, right))                        => EqEqExpression(left, right)

    case NegationExpression(LowerExpression(left, right))                      => GreaterEqualExpression(left, right)
    case NegationExpression(GreaterEqualExpression(left, right))               => LowerExpression(left, right)
    case NegationExpression(LowerEqualExpression(left, right))                 => GreaterExpression(left, right)
    case NegationExpression(GreaterExpression(left, right))                    => LowerEqualExpression(left, right)

    case NegationExpression(AndAndExpression(left, right))                     => OrOrExpression(NegationExpression(left), NegationExpression(right))
    case NegationExpression(OrOrExpression(left, right))                       => AndAndExpression(NegationExpression(left), NegationExpression(right))

    case AndAndExpression(BooleanConstant(true), expr : Expression)            => expr
    case AndAndExpression(expr : Expression, BooleanConstant(true))            => expr
    case AndAndExpression(BooleanConstant(false), expr : Expression)           => BooleanConstant(false)
    case AndAndExpression(expr : Expression, BooleanConstant(false))           => BooleanConstant(false)

    case OrOrExpression(BooleanConstant(true), expr : Expression)              => BooleanConstant(true)
    case OrOrExpression(expr : Expression, BooleanConstant(true))              => BooleanConstant(true)
    case OrOrExpression(BooleanConstant(false), expr : Expression)             => expr
    case OrOrExpression(expr : Expression, BooleanConstant(false))             => expr

    case ConditionStatement(BooleanConstant(cond), tBranch, fBranch) => {
      if (cond) {
        if (tBranch.isEmpty) NullStatement else tBranch
      } else {
        if (fBranch.isEmpty) NullStatement else fBranch
      }
    }
  })

  private def simplifyMult(le : Expression, ri : Expression) : Seq[Expression] = {
    (le, ri) match { // matching for constants is not required here (this is already handled by the caller)
      case (left : VectorExpression, right : VectorExpression) =>
        if (left.length != right.length) Logger.error("Vector sizes must match for multiplication")
        if (left.rowVector != right.rowVector) Logger.error("Vector types must match for multiplication")
        List(AdditionExpression(left.expressions.view.zip(right.expressions).map { x => x._1 * x._2 : Expression }.to[ListBuffer]))
      case (left, right) =>
        List(left, right)
    }
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
    case FunctionStatement(_, name, _, ListBuffer(), _, _, _) => {
      emptyFunctions += name
      List()
    }
  })

  this += new Transformation("Removing obsolete references", {
    case FunctionCallExpression(name, _) if emptyFunctions.contains(name) => NullExpression
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
        var reals = vector.expressions.filter(_.isInstanceOf[FloatConstant]).length
        var ints = vector.expressions.filter(_.isInstanceOf[IntegerConstant]).length
        if (ints > 0 && reals > 0) {
          vector.expressions = vector.expressions.map(e => if (e.isInstanceOf[FloatConstant]) e; else FloatConstant(e.asInstanceOf[IntegerConstant].v))
        }
      }
    })

    matrices.foreach(matrix => {
      if (matrix.isConstant) {
        var reals = matrix.expressions.flatten[Expression].filter(_.isInstanceOf[FloatConstant]).length
        var ints = matrix.expressions.flatten[Expression].filter(_.isInstanceOf[IntegerConstant]).length
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
    case fa : FieldAccess =>
      addAccess(fa.fieldSelection.field.codeName, fa.index - LoopOverDimensions.defIt(fa.index.length))
      fa
    case dfa : DirectFieldAccess =>
      addAccess(dfa.fieldSelection.field.codeName, dfa.index - dfa.fieldSelection.field.referenceOffset - LoopOverDimensions.defIt(dfa.index.length))
      dfa
  })
}
