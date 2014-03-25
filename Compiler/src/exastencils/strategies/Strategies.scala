package exastencils.strategies

import scala.reflect.ClassTag

import exastencils.core._
import exastencils.core.collectors._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.Transformation._
import exastencils.domain._
import exastencils.primitives.Class // FIXME
import exastencils.omp._

object PrintStrategy extends Strategy("Pretty-Print") {
  this += new Transformation("Pretty-Print", {
    case printable : FilePrettyPrintable =>
      printable.printToFile;
      Some(printable);
  });
}

object ExpandStrategy extends Strategy("Expanding") {
  val collector = new StackCollector;

  override def apply = {
    StateManager.register(collector);
    super.apply;
    StateManager.unregister(collector);
  }

  this += new Transformation("Hoho, expanding all day...", {
    case expandable : Expandable =>
      Some(expandable.expand(collector));
  });
}

object SimplifyStrategy extends Strategy("Simplifying") {
  this += new Transformation("Improving the quality of some horrid code...", {
    // FIXME: for re-runs only the number of replacements of the last trafo is checked, thus only one big trafo (should also improve performance)
    // TODO: extend for general data types; extend with missing cases; extend for left-right-switched cases

    //this += new Transformation("Resolving unary operations", {
    case UnaryExpression(UnaryOperators.Negative, IntegerConstant(value)) =>
      Some(IntegerConstant(-value))
    case UnaryExpression(UnaryOperators.Negative, FloatConstant(value)) =>
      Some(FloatConstant(-value))
    //})

    //this += new Transformation("Correcting signs", {
    case BinaryExpression(BinaryOperators.Addition, left, IntegerConstant(right)) if (right < 0) =>
      left - IntegerConstant(-right)
    case BinaryExpression(BinaryOperators.Subtraction, left, IntegerConstant(right)) if (right < 0) =>
      left + IntegerConstant(-right)
    case BinaryExpression(BinaryOperators.Addition, left, FloatConstant(right)) if (right < 0) =>
      left - FloatConstant(-right)
    case BinaryExpression(BinaryOperators.Subtraction, left, FloatConstant(right)) if (right < 0) =>
      left + FloatConstant(-right)
    //})

    //this += new Transformation("Resolving operations on constant integers", {
    case BinaryExpression(BinaryOperators.Addition, left : IntegerConstant, right : IntegerConstant) =>
      Some(IntegerConstant(left.v + right.v))
    case BinaryExpression(BinaryOperators.Subtraction, left : IntegerConstant, right : IntegerConstant) =>
      Some(IntegerConstant(left.v - right.v))
    case BinaryExpression(BinaryOperators.Multiplication, left : IntegerConstant, right : IntegerConstant) =>
      Some(IntegerConstant(left.v * right.v))
    case BinaryExpression(BinaryOperators.Division, left : IntegerConstant, right : IntegerConstant) =>
      Some(IntegerConstant(left.v / right.v))
    case BinaryExpression(BinaryOperators.Addition, left : FloatConstant, right : FloatConstant) =>
      Some(FloatConstant(left.v + right.v))
    case BinaryExpression(BinaryOperators.Subtraction, left : FloatConstant, right : FloatConstant) =>
      Some(FloatConstant(left.v - right.v))
    case BinaryExpression(BinaryOperators.Multiplication, left : FloatConstant, right : FloatConstant) =>
      Some(FloatConstant(left.v * right.v))
    case BinaryExpression(BinaryOperators.Division, left : FloatConstant, right : FloatConstant) =>
      Some(FloatConstant(left.v / right.v))
    //})

    //this += new Transformation("Resolving operations with 0/1", {
    case BinaryExpression(BinaryOperators.Multiplication, left : Expression, IntegerConstant(0)) =>
      Some(IntegerConstant(0))
    case BinaryExpression(BinaryOperators.Multiplication, left : Expression, FloatConstant(0.0)) =>
      Some(FloatConstant(0.0))

    case BinaryExpression(BinaryOperators.Multiplication, left : Expression, IntegerConstant(1)) =>
      Some(left)
    case BinaryExpression(BinaryOperators.Multiplication, left : Expression, FloatConstant(1.0)) =>
      Some(left)

    case BinaryExpression(op : BinaryOperators.Value, FloatConstant(0.0), right : Expression) if (op == BinaryOperators.Multiplication || op == BinaryOperators.Division) =>
      Some(FloatConstant(0.0))

    case BinaryExpression(op : BinaryOperators.Value, left : Expression, IntegerConstant(0)) if (op == BinaryOperators.Addition || op == BinaryOperators.Subtraction) =>
      Some(left)
    case BinaryExpression(op : BinaryOperators.Value, left : Expression, FloatConstant(0)) if (op == BinaryOperators.Addition || op == BinaryOperators.Subtraction) =>
      Some(left)
    //})

    //this += new Transformation("Applying distributive law", {
    // FIXME: the two following applications are obviously contrary -> treat with caution when extending to general data types
    case BinaryExpression(BinaryOperators.Multiplication, BinaryExpression(BinaryOperators.Addition, leftLeft, leftRight), right : Expression) =>
      Some((leftLeft * right) + (leftRight * right))
    case BinaryExpression(BinaryOperators.Addition,
      BinaryExpression(BinaryOperators.Multiplication, FloatConstant(leftLeft), leftRight),
      BinaryExpression(BinaryOperators.Multiplication, FloatConstant(rightLeft), rightRight)) if (leftLeft == rightLeft) =>
      Some(leftLeft * (leftRight + rightRight))
    //})

    //this += new Transformation("Summarize constant additions", {
    case BinaryExpression(BinaryOperators.Addition,
      BinaryExpression(BinaryOperators.Addition, leftLeft, IntegerConstant(leftRight)),
      BinaryExpression(BinaryOperators.Addition, rightLeft, IntegerConstant(rightRight))) =>
      Some((leftLeft + rightLeft) + (leftRight + rightRight))
    //})

  })
}

object AddMemberFunctionPrefix extends Strategy("Adding member function prefixes") {
  // FIXME: requires nested strategies which currently are not available
  //    this += new Transformation("Add function scope prefixes to class member functions", {
  //      case c : Class =>
  //        var strategyAddScopePrefix = new Strategy("strategyAddScopePrefix");
  //        strategyAddScopePrefix += new Transformation({
  //          case function : FunctionStatement =>
  //
  //            function.name = s"${c.className}::${f.name}";
  //
  //            Some(function);
  //        }, true, c)
  //
  //        strategyAddScopePrefix.apply;
  //
  //        Some(c);
  //    });
  this += new Transformation("Adding function scope prefixes to class member functions", {
    case c : Class =>
      for (func <- c.functions) {
        func match { case f : FunctionStatement => f.name = s"${c.className}::${f.name}"; }
      }
      Some(c);
  });
}

object AddOMPPragmas extends Strategy("Adding OMP pragmas") {
  this += new Transformation("Adding OMP critical pragmas", {
    case target : OMP_PotentiallyCritical =>
      Some(new OMP_Critical(target));
  }, false);

  this += new Transformation("Adding OMP parallel for pragmas", {
    case target : ForLoopStatement with OMP_PotentiallyParallel =>
      Some(new OMP_ParallelFor(target, target.addOMPStatements));
  }, false);
}