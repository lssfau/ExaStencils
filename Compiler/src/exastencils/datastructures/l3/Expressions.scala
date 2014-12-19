package exastencils.datastructures.l3

import exastencils.core.Logger
import scala.collection.mutable.ListBuffer

import exastencils.data
import exastencils.datastructures._
import exastencils.knowledge

trait Expression extends Node {

  def lEval(env : Environment) : StaticLValue = {
    throw new Exception("Cannot evaluate to a static l-value.")
  }
  def rEval(env : Environment) : StaticRValue = {
    throw new Exception("Cannot evaluate to a static r-value.")
  }
  def dynamicLEval(env : Environment) : DynamicLValue = {
    throw new Exception("Cannot evaluate to a dynamic l-value.")
  }
  def dynamicREval(env : Environment, block : TcbBlock) : DynamicRValue = {
    throw new Exception("Cannot evaluate to a dynamic r-value.")
  }
  def scType(env : Environment) : ScType = {
    throw new Exception("This expression does not have a type... which is strange.")
  }

}

trait Number extends Expression {
  def value : AnyVal
}

case class IdentifierExpression(val id : String) extends Expression {

  override def lEval(env : Environment) : StaticLValue = {
    env.lookupLValue(id) match {
      case e : StaticLValue => e
      case _                => Logger.error(id ++ " is not a static l-value.")
    }
  }

  override def rEval(env : Environment) : StaticRValue = {
    env.lookupRValue(id) match {
      case e : StaticRValue => e
      case _                => Logger.error(id ++ " is not a static r-value")
    }
  }

  override def dynamicREval(env : Environment, block : TcbBlock) : DynamicRValue = {

    env.lookupRValue(id) match {
      case v : FieldRValue => new DynamicRValue(v.toTc(), FieldDatatype())
      case _               => Logger.error(id ++ " is not a variable")
    }

  }

  override def scType(env : Environment) : ScType = env.lookup(id).scType

}

case class StringConstant(val value : String) extends Expression
case class IntegerConstant(val v : Long) extends Number {
  override def value = v
}

case class FloatConstant(val v : Double) extends Number {
  override def value = v
}

case class Variable(val id : String, val datatype : ScType) extends Expression {
  override def scType(env : Environment) = datatype
}

case class FunctionCallExpression(val id : String, val arguments : List[Expression])
    extends Expression {

  override def dynamicREval(env : Environment, block : TcbBlock) : DynamicRValue = {

    env.lookupRValue(id) match {
      case fun : AbstractFunctionRValue => new DynamicRValue(fun.writeTcApplication(env, block, arguments), fun.scReturnType)
      case _                            => Logger.error(id ++ " is not a function.")
    }

  }
}

case class BinaryExpression(var operator : String, var left : Expression, var right : Expression) extends Expression {

  override def dynamicREval(env : Environment, block : TcbBlock) : DynamicRValue = {
    val leftTc = left.dynamicREval(env, block)
    val rightTc = right.dynamicREval(env, block)

    /// @todo: Auxiliary computations
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

