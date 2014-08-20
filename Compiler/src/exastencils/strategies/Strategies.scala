package exastencils.strategies

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

object PrintStrategy extends DefaultStrategy("Pretty-Print") {
  this += new Transformation("Pretty-Print", {
    case printable : FilePrettyPrintable =>
      printable.printToFile
      printable
  })
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
    case expandable : Expandable =>
      expandable.expand
  })
}

object SimplifyStrategy extends DefaultStrategy("Simplifying") {
  // FIXME: remove NullExpressions / NullStatements
  // FIXME: remove empty functions
  // FIXME: remove (true) conditions

  def doUntilDone(node : Option[Node] = None) = {
    do { apply(node) }
    while (results.last._2.matches > 0) // FIXME: cleaner code
  }

  def doUntilDoneStandalone(node : Node) = {
    val oldLvl = Logger.getLevel
    Logger.setLevel(1)
    do { applyStandalone(node) }
    while (results.last._2.matches > 0) // FIXME: cleaner code
    Logger.setLevel(oldLvl)
  }

  this += new Transformation("Improving the quality of some horrid code...", {
    // FIXME: for re-runs only the number of replacements of the last trafo is checked, thus only one big trafo (should also improve performance)
    // TODO: extend for general data types; extend with missing cases; extend for left-right-switched cases

    //this += new Transformation("Resolving unary operations", {
    case UnaryExpression(UnaryOperators.Negative, IntegerConstant(value)) =>
      IntegerConstant(-value.v)
    case UnaryExpression(UnaryOperators.Negative, FloatConstant(value)) =>
      FloatConstant(-value.v)
    //})

    //this += new Transformation("Resolving operations with mixed data types", {
    case AdditionExpression(left : IntegerConstant, right : FloatConstant) =>
      FloatConstant(left.v + right.v)
    case SubtractionExpression(left : IntegerConstant, right : FloatConstant) =>
      FloatConstant(left.v - right.v)
    case MultiplicationExpression(left : IntegerConstant, right : FloatConstant) =>
      FloatConstant(left.v * right.v)
    case DivisionExpression(left : IntegerConstant, right : FloatConstant) =>
      FloatConstant(left.v / right.v)
    //})

    //this += new Transformation("Permutating operations with constants on the 'wrong' side", {
    case AdditionExpression(left : IntegerConstant, right : Expression) if !right.isInstanceOf[IntegerConstant] =>
      right + left
    case MultiplicationExpression(left : IntegerConstant, right : Expression) if !right.isInstanceOf[IntegerConstant] =>
      right * left

    case AdditionExpression(left : FloatConstant, right : Expression) if !right.isInstanceOf[FloatConstant] =>
      right + left
    case MultiplicationExpression(left : FloatConstant, right : Expression) if !right.isInstanceOf[FloatConstant] =>
      right * left
    //})

    //this += new Transformation("Correcting signs", {
    case AdditionExpression(left, IntegerConstant(right)) if (right < 0) =>
      left - IntegerConstant(-right)
    case SubtractionExpression(left, IntegerConstant(right)) if (right < 0) =>
      left + IntegerConstant(-right)
    case AdditionExpression(left, FloatConstant(right)) if (right < 0) =>
      left - FloatConstant(-right)
    case SubtractionExpression(left, FloatConstant(right)) if (right < 0) =>
      left + FloatConstant(-right)
    //})

    //this += new Transformation("Resolving operations on constant integers", {
    case AdditionExpression(left : IntegerConstant, right : IntegerConstant) =>
      IntegerConstant(left.v + right.v)
    case SubtractionExpression(left : IntegerConstant, right : IntegerConstant) =>
      IntegerConstant(left.v - right.v)
    case MultiplicationExpression(left : IntegerConstant, right : IntegerConstant) =>
      IntegerConstant(left.v * right.v)
    case DivisionExpression(left : IntegerConstant, right : IntegerConstant) =>
      IntegerConstant(left.v / right.v)
    case ModuloExpression(left : IntegerConstant, right : IntegerConstant) =>
      IntegerConstant(left.v % right.v)

    case AdditionExpression(AdditionExpression(leftLeft, leftRight : IntegerConstant), right : IntegerConstant) =>
      (leftLeft + IntegerConstant(leftRight.v + right.v))
    case SubtractionExpression(AdditionExpression(leftLeft, leftRight : IntegerConstant), right : IntegerConstant) =>
      (leftLeft + IntegerConstant(leftRight.v - right.v))
    case AdditionExpression(SubtractionExpression(leftLeft, leftRight : IntegerConstant), right : IntegerConstant) =>
      (leftLeft + IntegerConstant(-leftRight.v + right.v))
    case SubtractionExpression(SubtractionExpression(leftLeft, leftRight : IntegerConstant), right : IntegerConstant) =>
      (leftLeft - IntegerConstant(leftRight.v + right.v))

    case AdditionExpression(left : FloatConstant, right : FloatConstant) =>
      FloatConstant(left.v + right.v)
    case SubtractionExpression(left : FloatConstant, right : FloatConstant) =>
      FloatConstant(left.v - right.v)
    case MultiplicationExpression(left : FloatConstant, right : FloatConstant) =>
      FloatConstant(left.v * right.v)
    case DivisionExpression(left : FloatConstant, right : FloatConstant) =>
      FloatConstant(left.v / right.v)

    case AdditionExpression(AdditionExpression(leftLeft, leftRight : FloatConstant), right : FloatConstant) =>
      (leftLeft + FloatConstant(leftRight.v + right.v))
    case SubtractionExpression(AdditionExpression(leftLeft, leftRight : FloatConstant), right : FloatConstant) =>
      (leftLeft + FloatConstant(leftRight.v - right.v))
    case AdditionExpression(SubtractionExpression(leftLeft, leftRight : FloatConstant), right : FloatConstant) =>
      (leftLeft + FloatConstant(-leftRight.v + right.v))
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

    case AdditionExpression(left : Expression, IntegerConstant(0))        => left
    case SubtractionExpression(left : Expression, IntegerConstant(0))     => left
    case AdditionExpression(left : Expression, FloatConstant(0))          => left
    case SubtractionExpression(left : Expression, FloatConstant(0))       => left

    //})

    //this += new Transformation("Applying distributive law", {
    // FIXME: the two following applications are obviously contrary -> treat with caution when extending to general data types
    //    case MultiplicationExpression(AdditionExpression(leftLeft, leftRight), right : Expression)    => ((leftLeft * right) + (leftRight * right))
    //    case MultiplicationExpression(SubtractionExpression(leftLeft, leftRight), right : Expression) => ((leftLeft * right) - (leftRight * right))
    case AdditionExpression(
      MultiplicationExpression(FloatConstant(leftLeft), leftRight),
      MultiplicationExpression(FloatConstant(rightLeft), rightRight)) if (leftLeft == rightLeft) =>
      (leftLeft * (leftRight + rightRight))
    //})

    //this += new Transformation("Summarize constant additions", {
    case AdditionExpression(
      AdditionExpression(leftLeft, IntegerConstant(leftRight)),
      AdditionExpression(rightLeft, IntegerConstant(rightRight))) =>
      ((leftLeft + rightLeft) + (leftRight.v + rightRight.v))
    case AdditionExpression(
      AdditionExpression(leftLeft, IntegerConstant(leftRight)),
      SubtractionExpression(rightLeft, IntegerConstant(rightRight))) =>
      ((leftLeft + rightLeft) + (leftRight.v - rightRight.v))
    case AdditionExpression(
      SubtractionExpression(leftLeft, IntegerConstant(leftRight)),
      AdditionExpression(rightLeft, IntegerConstant(rightRight))) =>
      ((leftLeft + rightLeft) + (-leftRight.v + rightRight.v))
    case AdditionExpression(
      SubtractionExpression(leftLeft, IntegerConstant(leftRight)),
      SubtractionExpression(rightLeft, IntegerConstant(rightRight))) =>
      ((leftLeft + rightLeft) - (leftRight.v + rightRight.v))
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
    case NeqNeqExpression(left : IntegerConstant, right : IntegerConstant)       => BooleanConstant(left.value != right.value)
    case LowerExpression(left : IntegerConstant, right : IntegerConstant)        => BooleanConstant(left.value < right.value)
    case LowerEqualExpression(left : IntegerConstant, right : IntegerConstant)   => BooleanConstant(left.value <= right.value)
    case GreaterExpression(left : IntegerConstant, right : IntegerConstant)      => BooleanConstant(left.value > right.value)
    case GreaterEqualExpression(left : IntegerConstant, right : IntegerConstant) => BooleanConstant(left.value >= right.value)

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
