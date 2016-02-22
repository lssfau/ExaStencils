package exastencils.strategies

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

    //this += new Transformation("Resolving unary operations", {
    case NegativeExpression(IntegerConstant(value)) =>
      IntegerConstant(-value.v)
    case NegativeExpression(FloatConstant(value)) =>
      FloatConstant(-value.v)
    //    case NegativeExpression(add : AdditionExpression) =>
    //      add.summands.transform { s => NegationExpression(s) }
    //      add
    //})

    case add : AdditionExpression =>
      // TODO: add distributive law?
      var change : Boolean = false
      var intCst : Long = 0
      var floatCst : Double = 0.0
      var vecExpr : VectorExpression = null
      val workQ = new Queue[Expression]()
      val rem = new ListBuffer[Expression]()
      for (s <- add.summands) {
        workQ.enqueue(s) // for nested AdditionExpressions; this allows in-order processing
        do {
          workQ.dequeue() match {
            case IntegerConstant(i) => intCst += i
            case FloatConstant(f)   => floatCst += f
            case AdditionExpression(sums) =>
              workQ.enqueue(sums : _*)
              change = true
            case v : VectorExpression =>
              if (vecExpr == null)
                vecExpr = v
              else {
                if (vecExpr.rowVector.getOrElse(true) != v.rowVector.getOrElse(true))
                  Logger.error("Vector types must match for addition")
                if (vecExpr.length != v.length)
                  Logger.error("Vector sizes must match for addition")
                vecExpr =
                  VectorExpression(GetResultingDatatype(vecExpr.datatype, v.datatype),
                    (vecExpr.expressions, v.expressions).zipped.map(_ + _),
                    if (vecExpr.rowVector.isDefined) vecExpr.rowVector else v.rowVector)
              }
            case r : Expression => rem += r
          }
        } while (!workQ.isEmpty)
      }
      if (floatCst != 0.0)
        new FloatConstant(floatCst + intCst) +=: rem
      else if (intCst != 0)
        new IntegerConstant(intCst) +=: rem

      if (vecExpr != null) {
        if (!rem.isEmpty)
          Logger.error("Unable to add VectorExpression with other Expression types")
        else
          vecExpr
      } else if (!change && rem.length == add.summands.length) {
        negMatches += 1
        add // return
      } else {
        new AdditionExpression(rem)
      }

    //this += new Transformation("Resolving operations with mixed data types", {
    //    case AdditionExpression(left : IntegerConstant, right : FloatConstant) =>
    //      FloatConstant(left.v + right.v)
    case SubtractionExpression(left : IntegerConstant, right : FloatConstant) =>
      FloatConstant(left.v - right.v)
    case MultiplicationExpression(left : IntegerConstant, right : FloatConstant) =>
      FloatConstant(left.v * right.v)
    case DivisionExpression(left : IntegerConstant, right : FloatConstant) =>
      FloatConstant(left.v / right.v)

    //    case AdditionExpression(left : FloatConstant, right : IntegerConstant) =>
    //      FloatConstant(left.v + right.v)
    case SubtractionExpression(left : FloatConstant, right : IntegerConstant) =>
      FloatConstant(left.v - right.v)
    case MultiplicationExpression(left : FloatConstant, right : IntegerConstant) =>
      FloatConstant(left.v * right.v)
    case DivisionExpression(left : FloatConstant, right : IntegerConstant) =>
      FloatConstant(left.v / right.v)
    //})

    //this += new Transformation("Permutating operations with constants on the 'wrong' side", {
    //    case AdditionExpression(left : IntegerConstant, right : Expression) if !right.isInstanceOf[IntegerConstant] =>
    //      right + left
    case MultiplicationExpression(left : IntegerConstant, right : Expression) if !right.isInstanceOf[IntegerConstant] =>
      right * left

    //    case AdditionExpression(left : FloatConstant, right : Expression) if !right.isInstanceOf[FloatConstant] =>
    //      right + left
    case MultiplicationExpression(left : FloatConstant, right : Expression) if !right.isInstanceOf[FloatConstant] =>
      right * left
    //})

    //this += new Transformation("Correcting signs", {
    //    case AdditionExpression(left, IntegerConstant(right)) if (right < 0) =>
    //      left - IntegerConstant(-right)
    case SubtractionExpression(left, IntegerConstant(right)) if (right < 0) =>
      left + IntegerConstant(-right)
    //    case AdditionExpression(left, FloatConstant(right)) if (right < 0) =>
    //      left - FloatConstant(-right)
    case SubtractionExpression(left, FloatConstant(right)) if (right < 0) =>
      left + FloatConstant(-right)
    //})

    //this += new Transformation("Resolving operations on constant integers", {
    //    case AdditionExpression(left : IntegerConstant, right : IntegerConstant) =>
    //      IntegerConstant(left.v + right.v)
    case SubtractionExpression(left : IntegerConstant, right : IntegerConstant) =>
      IntegerConstant(left.v - right.v)
    case MultiplicationExpression(left : IntegerConstant, right : IntegerConstant) =>
      IntegerConstant(left.v * right.v)
    case DivisionExpression(left : IntegerConstant, right : IntegerConstant) =>
      IntegerConstant(left.v / right.v)
    case ModuloExpression(left : IntegerConstant, right : IntegerConstant) =>
      IntegerConstant(left.v % right.v)

    //    case AdditionExpression(AdditionExpression(leftLeft, leftRight : IntegerConstant), right : IntegerConstant) =>
    //      (leftLeft + IntegerConstant(leftRight.v + right.v))
    //    case SubtractionExpression(AdditionExpression(leftLeft, leftRight : IntegerConstant), right : IntegerConstant) =>
    //      (leftLeft + IntegerConstant(leftRight.v - right.v))
    //    case AdditionExpression(SubtractionExpression(leftLeft, leftRight : IntegerConstant), right : IntegerConstant) =>
    //      (leftLeft + IntegerConstant(-leftRight.v + right.v))
    case SubtractionExpression(SubtractionExpression(leftLeft, leftRight : IntegerConstant), right : IntegerConstant) =>
      (leftLeft - IntegerConstant(leftRight.v + right.v))

    //    case AdditionExpression(left : FloatConstant, right : FloatConstant) =>
    //      FloatConstant(left.v + right.v)
    case SubtractionExpression(left : FloatConstant, right : FloatConstant) =>
      FloatConstant(left.v - right.v)
    case MultiplicationExpression(left : FloatConstant, right : FloatConstant) =>
      FloatConstant(left.v * right.v)
    case DivisionExpression(left : FloatConstant, right : FloatConstant) =>
      FloatConstant(left.v / right.v)

    //    case AdditionExpression(AdditionExpression(leftLeft, leftRight : FloatConstant), right : FloatConstant) =>
    //      (leftLeft + FloatConstant(leftRight.v + right.v))
    //    case SubtractionExpression(AdditionExpression(leftLeft, leftRight : FloatConstant), right : FloatConstant) =>
    //      (leftLeft + FloatConstant(leftRight.v - right.v))
    //    case AdditionExpression(SubtractionExpression(leftLeft, leftRight : FloatConstant), right : FloatConstant) =>
    //      (leftLeft + FloatConstant(-leftRight.v + right.v))
    case SubtractionExpression(SubtractionExpression(leftLeft, leftRight : FloatConstant), right : FloatConstant) =>
      (leftLeft - FloatConstant(leftRight.v + right.v))

    case MultiplicationExpression(MultiplicationExpression(leftLeft, leftRight : FloatConstant), right : FloatConstant) =>
      leftLeft * (leftRight.v * right.v)
    //})

    //this += new Transformation("Resolving operations with 0/1", {
    case MultiplicationExpression(left : Expression, IntegerConstant(0)) =>
      IntegerConstant(0)
    case MultiplicationExpression(left : Expression, FloatConstant(0.0)) =>
      FloatConstant(0.0)

    case MultiplicationExpression(left : Expression, IntegerConstant(1)) =>
      left
    case MultiplicationExpression(left : Expression, FloatConstant(1.0)) =>
      left

    case MultiplicationExpression(FloatConstant(0.0), right : Expression) => FloatConstant(0.0)
    case DivisionExpression(FloatConstant(0.0), right : Expression)       => FloatConstant(0.0)

    //    case AdditionExpression(left : Expression, IntegerConstant(0))        => left
    case SubtractionExpression(left : Expression, IntegerConstant(0))     => left
    //    case AdditionExpression(left : Expression, FloatConstant(0))          => left
    case SubtractionExpression(left : Expression, FloatConstant(0))       => left

    // Simplify vectors
    case NegativeExpression(v : VectorExpression)                         => VectorExpression(v.datatype, v.expressions.map(_ * (-1)), v.rowVector)
    //    case AdditionExpression(left : VectorExpression, right : VectorExpression) => {
    //      if (left.rowVector.getOrElse(true) != right.rowVector.getOrElse(true)) Logger.error("Vector types must match for addition")
    //      if (left.length != right.length) Logger.error("Vector sizes must match for addition")
    //      VectorExpression(GetResultingDatatype(left.datatype, right.datatype), (left.expressions, right.expressions).zipped.map(_ + _), left.rowVector.getOrElse(right.rowVector).asInstanceOf[Option[Boolean]])
    //    }
    case SubtractionExpression(left : VectorExpression, right : VectorExpression) => {
      if (left.rowVector.getOrElse(true) != right.rowVector.getOrElse(true)) Logger.error("Vector types must match for subtraction")
      if (left.length != right.length) Logger.error("Vector sizes must match for subtraction")
      VectorExpression(GetResultingDatatype(left.datatype, right.datatype), (left.expressions, right.expressions).zipped.map(_ - _), left.rowVector.getOrElse(right.rowVector).asInstanceOf[Option[Boolean]])
    }
    case MultiplicationExpression(v : VectorExpression, c : IntegerConstant) => {
      VectorExpression(v.datatype, v.expressions.map(c * _), v.rowVector)
    }
    case MultiplicationExpression(v : VectorExpression, c : FloatConstant) => {
      VectorExpression(GetResultingDatatype(v.datatype, Some(RealDatatype)), v.expressions.map(c * _), v.rowVector)
    }
    case MultiplicationExpression(c : IntegerConstant, v : VectorExpression) => {
      VectorExpression(v.datatype, v.expressions.map(c * _), v.rowVector)
    }
    case MultiplicationExpression(c : FloatConstant, v : VectorExpression) => {
      VectorExpression(GetResultingDatatype(Some(RealDatatype), v.datatype), v.expressions.map(c * _), v.rowVector)
    }
    case MultiplicationExpression(left : VectorExpression, right : VectorExpression) => {
      if (left.length != right.length) Logger.error("Vector sizes must match for multiplication")
      if (left.rowVector.getOrElse(true) != right.rowVector.getOrElse(true)) Logger.error("Vector types must match for multiplication")
      val t = (left.expressions, right.expressions).zipped.map(_ * _)
      t.reduce((a : Expression, b : Expression) => a + b)
    }

    // Simplify matrices
    case NegativeExpression(m : MatrixExpression) => MatrixExpression(m.datatype, m.expressions.map(_.map(_ * (-1)).map(_.asInstanceOf[Expression])))
    case MultiplicationExpression(c : IntegerConstant, m : MatrixExpression) =>
      MatrixExpression(m.datatype, m.expressions.map(_.map(c * _).map(_.asInstanceOf[Expression])))
    case MultiplicationExpression(c : FloatConstant, m : MatrixExpression) =>
      MatrixExpression(m.datatype, m.expressions.map(_.map(c * _).map(_.asInstanceOf[Expression])))
    case MultiplicationExpression(m : MatrixExpression, c : IntegerConstant) =>
      MatrixExpression(m.datatype, m.expressions.map(_.map(c * _).map(_.asInstanceOf[Expression])))
    case MultiplicationExpression(m : MatrixExpression, c : FloatConstant) =>
      MatrixExpression(m.datatype, m.expressions.map(_.map(c * _).map(_.asInstanceOf[Expression])))

    //})

    //this += new Transformation("Applying distributive law", {
    // FIXME: the two following applications are obviously contrary -> treat with caution when extending to general data types
    //    case MultiplicationExpression(AdditionExpression(leftLeft, leftRight), right : Expression)    => ((leftLeft * right) + (leftRight * right))
    //    case MultiplicationExpression(SubtractionExpression(leftLeft, leftRight), right : Expression) => ((leftLeft * right) - (leftRight * right))
    //    case AdditionExpression(
    //      MultiplicationExpression(FloatConstant(leftLeft), leftRight),
    //      MultiplicationExpression(FloatConstant(rightLeft), rightRight)) if (leftLeft == rightLeft) =>
    //      (leftLeft * (leftRight + rightRight))
    //})

    //this += new Transformation("Summarize constant additions", {
    //    case AdditionExpression(
    //      AdditionExpression(leftLeft, IntegerConstant(leftRight)),
    //      AdditionExpression(rightLeft, IntegerConstant(rightRight))) =>
    //      ((leftLeft + rightLeft) + (leftRight.v + rightRight.v))
    //    case AdditionExpression(
    //      AdditionExpression(leftLeft, IntegerConstant(leftRight)),
    //      SubtractionExpression(rightLeft, IntegerConstant(rightRight))) =>
    //      ((leftLeft + rightLeft) + (leftRight.v - rightRight.v))
    //    case AdditionExpression(
    //      SubtractionExpression(leftLeft, IntegerConstant(leftRight)),
    //      AdditionExpression(rightLeft, IntegerConstant(rightRight))) =>
    //      ((leftLeft + rightLeft) + (-leftRight.v + rightRight.v))
    //    case AdditionExpression(
    //      SubtractionExpression(leftLeft, IntegerConstant(leftRight)),
    //      SubtractionExpression(rightLeft, IntegerConstant(rightRight))) =>
    //      ((leftLeft + rightLeft) - (leftRight.v + rightRight.v))
    //})

    case loop @ ForLoopStatement(_, _, _, body, _) if (body.length == 1 && body(0).isInstanceOf[Scope]) =>
      loop.body = loop.body(0).asInstanceOf[Scope].body
      loop

    case scope @ Scope(body) if (body.length == 1 && body(0).isInstanceOf[Scope]) =>
      scope.body = scope.body(0).asInstanceOf[Scope].body
      scope

    case cond @ ConditionStatement(_, trueBody, _) if (trueBody.length == 1 && trueBody(0).isInstanceOf[Scope]) =>
      cond.trueBody = trueBody(0).asInstanceOf[Scope].body
      cond

    case cond @ ConditionStatement(_, _, falseBody) if (falseBody.length == 1 && falseBody(0).isInstanceOf[Scope]) =>
      cond.falseBody = falseBody(0).asInstanceOf[Scope].body
      cond

    case EqEqExpression(left : IntegerConstant, right : IntegerConstant)         => BooleanConstant(left.value == right.value)
    case NeqExpression(left : IntegerConstant, right : IntegerConstant)          => BooleanConstant(left.value != right.value)
    case LowerExpression(left : IntegerConstant, right : IntegerConstant)        => BooleanConstant(left.value < right.value)
    case LowerEqualExpression(left : IntegerConstant, right : IntegerConstant)   => BooleanConstant(left.value <= right.value)
    case GreaterExpression(left : IntegerConstant, right : IntegerConstant)      => BooleanConstant(left.value > right.value)
    case GreaterEqualExpression(left : IntegerConstant, right : IntegerConstant) => BooleanConstant(left.value >= right.value)

    case NegationExpression(BooleanConstant(b))                                  => BooleanConstant(!b)

    case AndAndExpression(BooleanConstant(true), right : Expression)             => right
    case AndAndExpression(BooleanConstant(false), right : Expression)            => BooleanConstant(false)
    case OrOrExpression(BooleanConstant(true), right : Expression)               => BooleanConstant(true)
    case OrOrExpression(BooleanConstant(false), right : Expression)              => right

    case AndAndExpression(left : Expression, right : BooleanConstant)            => AndAndExpression(right, left)
    case OrOrExpression(left : Expression, right : BooleanConstant)              => OrOrExpression(right, left)

    case ConditionStatement(BooleanConstant(true), body, _)                      => body
    case ConditionStatement(BooleanConstant(false), _, body) if (body.isEmpty)   => NullStatement
    case ConditionStatement(BooleanConstant(false), _, body)                     => body
  })
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
    case FunctionStatement(_, name, _, ListBuffer(), _, _) => {
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

