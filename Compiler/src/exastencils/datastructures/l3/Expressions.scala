package exastencils.datastructures.l3

import exastencils.datastructures._
import exastencils.logger._

trait Expression extends Node {

  def lEval(ctx : Context) : StaticLValue = {
    throw new Exception("Cannot evaluate to a static l-value.")
  }
  def rEval(ctx : Context) : StaticRValue = {
    throw new Exception("Cannot evaluate to a static r-value.")
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

trait Number extends Expression {
}

case class IdentifierExpression(val id : String) extends Expression {

  override def lEval(ctx : Context) : StaticLValue = {
    ctx.env.lookupLValue(id) match {
      case e : StaticLValue => e
      case _                => Logger.error(id ++ " is not a static l-value.")
    }
  }

  override def rEval(ctx : Context) : StaticRValue = {
    ctx.env.lookupRValue(id) match {
      case e : StaticRValue => e
      case _                => Logger.error(id ++ " is not a static r-value")
    }
  }

  override def dynamicREval(ctx : Context) : DynamicRValue = {
    import ctx.env

    env.lookupRValue(id) match {
      case v : FieldRValue => new DynamicRValue(v.toTc(), FieldDatatype())
      case _               => Logger.error(id ++ " is not a variable")
    }
  }

  override def scType(env : Environment) : ScType = env.lookup(id).scType

}

case class StringConstant(val value : String) extends Expression
case class IntegerConstant(val v : Int) extends Number {
  override def rEval(ctx : Context) : StaticRValue = IntegerRValue(v)
}

case class FloatConstant(val v : Double) extends Number {
  override def rEval(ctx : Context) : StaticRValue = FloatRValue(v)
}

case class FunctionArgument(val id : String, val datatype : ScType) extends Expression {
  override def scType(env : Environment) = datatype
}

case class FunctionCallExpression(val id : String, val arguments : List[Expression])
    extends Expression {

  override def scType(env : Environment) = {
    env.lookupRValue(id) match {
      case fun : AbstractFunctionRValue => fun.scReturnType
      case _                            => Logger.error(id ++ " is not a function.")
    }
  }

  override def dynamicREval(ctx : Context) : DynamicRValue = {
    import ctx.env

    env.lookupRValue(id) match {
      case fun : AbstractFunctionRValue => new DynamicRValue(fun.writeTcApplication(ctx, arguments), fun.scReturnType)
      case _                            => Logger.error(id ++ " is not a function.")
    }

  }

  override def lEval(ctx : Context) : StaticLValue = {
    import ctx.env

    env.lookupRValue(id) match {
      case fun : AbstractFunctionRValue => fun.staticApplication(ctx, arguments).asInstanceOf[StaticLValue]
      case _                            => Logger.error(id ++ " is not a function.")
    }
  }

  override def rEval(ctx : Context) : StaticRValue = {
    import ctx.env

    env.lookupRValue(id) match {
      case fun : AbstractFunctionRValue => fun.staticApplication(ctx, arguments).asInstanceOf[StaticRValue]
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

  override def rEval(ctx : Context) : StaticRValue = {

    val evald_elements = elements map { _.rEval(ctx) }

    StaticListRValue(evald_elements)
  }
}

