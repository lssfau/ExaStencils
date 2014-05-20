package exastencils.strategies

import exastencils.core._
import exastencils.core.collectors._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.Transformation._
import exastencils.domain._
import exastencils.omp._

object PrintStrategy extends DefaultStrategy("Pretty-Print") {
  this += new Transformation("Pretty-Print", {
    case printable : FilePrettyPrintable =>
      printable.printToFile
      printable
  })
}

object ExpandStrategy extends DefaultStrategy("Expanding") {
  this += new Transformation("Hoho, expanding all day...", {
    case expandable : Expandable =>
      expandable.expand
  })
}

object SimplifyStrategy extends DefaultStrategy("Simplifying") {
  // FIXME: remove NullExpressions / NullStatements
  // FIXME: remove empty functions
  // FIXME: remove (true) conditions

  this += new Transformation("Improving the quality of some horrid code...", {
    // FIXME: for re-runs only the number of replacements of the last trafo is checked, thus only one big trafo (should also improve performance)
    // TODO: extend for general data types; extend with missing cases; extend for left-right-switched cases

    //this += new Transformation("Resolving unary operations", {
    case UnaryExpression(UnaryOperators.Negative, IntegerConstant(value)) =>
      IntegerConstant(-value.v)
    case UnaryExpression(UnaryOperators.Negative, FloatConstant(value)) =>
      FloatConstant(-value.v)
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

    case AdditionExpression(AdditionExpression(leftLeft, leftRight : IntegerConstant), right : IntegerConstant) =>
      (leftLeft + (leftRight.v + right.v))
    case SubtractionExpression(AdditionExpression(leftLeft, leftRight : IntegerConstant), right : IntegerConstant) =>
      (leftLeft + (leftRight.v - right.v))
    case AdditionExpression(SubtractionExpression(leftLeft, leftRight : IntegerConstant), right : IntegerConstant) =>
      (leftLeft + (-leftRight.v + right.v))
    case SubtractionExpression(SubtractionExpression(leftLeft, leftRight : IntegerConstant), right : IntegerConstant) =>
      (leftLeft - (leftRight.v + right.v))

    case AdditionExpression(left : FloatConstant, right : FloatConstant) =>
      FloatConstant(left.v + right.v)
    case SubtractionExpression(left : FloatConstant, right : FloatConstant) =>
      FloatConstant(left.v - right.v)
    case MultiplicationExpression(left : FloatConstant, right : FloatConstant) =>
      FloatConstant(left.v * right.v)
    case DivisionExpression(left : FloatConstant, right : FloatConstant) =>
      FloatConstant(left.v / right.v)
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

    case MultiplicationExpression(FloatConstant(0.0), right : Expression)                         => FloatConstant(0.0)
    case DivisionExpression(FloatConstant(0.0), right : Expression)                               => FloatConstant(0.0)

    case AdditionExpression(left : Expression, IntegerConstant(0))                                => left
    case SubtractionExpression(left : Expression, IntegerConstant(0))                             => left
    case AdditionExpression(left : Expression, FloatConstant(0))                                  => left
    case SubtractionExpression(left : Expression, FloatConstant(0))                               => left

    //})

    //this += new Transformation("Applying distributive law", {
    // FIXME: the two following applications are obviously contrary -> treat with caution when extending to general data types
    case MultiplicationExpression(AdditionExpression(leftLeft, leftRight), right : Expression)    => ((leftLeft * right) + (leftRight * right))
    case MultiplicationExpression(SubtractionExpression(leftLeft, leftRight), right : Expression) => ((leftLeft * right) - (leftRight * right))
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

  })
}

object AddMemberFunctionPrefix extends DefaultStrategy("Adding member function prefixes") {
  // FIXME: requires nested strategies which currently are not available
  //    this += new Transformation("Add function scope prefixes to class member functions", {
  //      case c : Class =>
  //        var strategyAddScopePrefix = new Strategy("strategyAddScopePrefix")
  //        strategyAddScopePrefix += new Transformation({
  //          case function : FunctionStatement =>
  //
  //            function.name = s"${c.className}::${f.name}"
  //
  //            Some(function)
  //        }, true, c)
  //
  //        strategyAddScopePrefix.apply
  //
  //        Some(c)
  //    })
  this += new Transformation("Adding function scope prefixes to class member functions", {
    case c : Class =>
      for (func <- c.functions) {
        func match { case f : FunctionStatement => f.name = s"${c.className}::${f.name}" }
      }
      c
  })
}
