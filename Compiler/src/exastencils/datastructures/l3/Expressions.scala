package exastencils.datastructures.l3

import exastencils.datastructures._
import exastencils.logger._

trait Expression extends Node {

  def eval(ctx : Context) : StaticLocation = {
    throw new Exception("Cannot evaluate to a static value.")
  }

  def dynamicLEval(ctx : Context) : DynamicLValue = {
    throw new Exception("Cannot evaluate to a dynamic l-value.")
  }

  def dynamicREval(ctx : Context) : DynamicRValue = {
    throw new Exception("Cannot evaluate to a dynamic r-value.")
  }

  def scType(env : Environment) : ScType = {
    throw new Exception("This expression does not have a type... which is strange.")
  }

}

trait Number extends Expression

case class IdentifierExpression(val id : String) extends Expression {

  override def eval(ctx : Context) : StaticLocation = ctx.env.lookup(id)

  override def dynamicREval(ctx : Context) : DynamicRValue = {
    import ctx.env

    env.lookup(id).read match {
      case v : FieldLValue => new DynamicRValue(v.deref.toTc(), FieldDatatype())
      case _               => Logger.error(id ++ " is not a variable")
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

  override def dynamicREval(ctx : Context) : DynamicRValue = {
    import ctx.env

    env.lookup(id).read match {
      case fun : AbstractFunctionRValue => new DynamicRValue(fun.writeTcApplication(ctx, arguments), fun.scReturnType)
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

  override def dynamicREval(ctx : Context) : DynamicRValue = {
    val leftTc = left.dynamicREval(ctx)
    val rightTc = right.dynamicREval(ctx)

    /** @todo: Auxiliary computations */
    new DynamicRValue(l4.BinaryExpression(operator, leftTc.tcExpression, rightTc.tcExpression), leftTc.scType)

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

