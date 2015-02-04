package exastencils.datastructures.l3

import exastencils.datastructures._
import exastencils.logger._

trait Expression extends Node {

  def eval(ctx : Context) : StaticLocation = {
    throw new Exception("Cannot evaluate to a static value.")
  }

  def dynamicEval(ctx : Context) : DynamicLocation = {
    throw new Exception("Cannot evaluate to a dynamic value.")
  }

  def scType(env : Environment) : ScType = {
    throw new Exception("This expression does not have a type... which is strange.")
  }
}

trait Number extends Expression

case class IdentifierExpression(val id : String) extends Expression {

  override def eval(ctx : Context) : StaticLocation = ctx.env.lookup(id)

  override def dynamicEval(ctx : Context) : DynamicLocation = {
    import ctx.env

    env.lookup(id).read match {
      case v : DynamicLocation => v
      case _                   => Logger.error(id ++ " is not a dynamic location.")
    }
  }

  override def scType(env : Environment) : ScType = env.lookup(id).scType

}

case class StringConstant(val value : String) extends Expression

case class IntegerConstant(val v : Int) extends Number {
  override def eval(ctx : Context) = StaticConstant(IntegerRValue(v))
}

case class FloatConstant(val v : Double) extends Number {
  override def eval(ctx : Context) = StaticConstant(FloatRValue(v))
}

case class FunctionArgument(val id : String, val datatype : ScType) extends Expression {
  override def scType(env : Environment) = datatype
}

/** Node for a call to a function. */
case class FunctionCallExpression(val id : String, val arguments : List[Expression])
    extends Expression {

  override def scType(env : Environment) = {
    env.lookup(id).read match {
      case fun : AbstractFunctionRValue => fun.scReturnType
      case _                            => Logger.error(id ++ " is not a function.")
    }
  }

  override def dynamicEval(ctx : Context) : DynamicLocation = {
    import ctx.env

    env.lookup(id).read match {
      case fun : AbstractFunctionRValue => fun.writeTcApplication(ctx, arguments)
      case _                            => Logger.error(id ++ " is not a function.")
    }

  }

  /** Execute the function. */
  override def eval(ctx : Context) : StaticLocation = {
    import ctx.env

    env.lookup(id).read match {
      case fun : AbstractFunctionRValue => fun.staticApplication(ctx, arguments)
      case _                            => Logger.error(id ++ " is not a function.")
    }
  }

}

case class BinaryExpression(var operator : String, var left : Expression, var right : Expression) extends Expression {

  override def dynamicEval(ctx : Context) : DynamicLocation = {
    val leftTc = left.dynamicEval(ctx)
    val rightTc = right.dynamicEval(ctx)

    operator match {
      case "+" => leftTc + rightTc
      case "-" => leftTc - rightTc
      case "*" => leftTc * rightTc
    }

    //new DynamicTcExpressionLocation(l4.BinaryExpression(operator, leftTc.tcForReading, rightTc.tcForReading), left.scType(ctx.env))
  }
  override def scType(env : Environment) : ScType = {
    val lt = left.scType(env)
    val rt = right.scType(env)
    if (lt != rt) {
      Logger.error("Incompatible types.")
    }
    lt
  }
}

case class ListExpression(elements : List[Expression]) extends Expression {
  override def scType(env : Environment) = StaticListDatatype()

  override def eval(ctx : Context) : StaticLocation = {
    val evald_elements = elements map { _.eval(ctx).read }

    StaticConstant(ListStaticValue(evald_elements))
  }
}

