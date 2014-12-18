package exastencils.datastructures.l3

import exastencils.core.Logger
import scala.collection.mutable.ListBuffer

import exastencils.data
import exastencils.datastructures._
import exastencils.knowledge

trait Expression extends Node {

  def lEval(env : Environment) : LValue = {
    throw new Exception("Cannot evaluate to a static l-value.")
  }
  def rEval(env : Environment) : RValue = {
    throw new Exception("Cannot evaluate to a static r-value.")
  }
  def dynamicLEval(env : Environment) : DynamicLValue = {
    throw new Exception("Cannot evaluate to a dynamic l-value.")
  }
  def dynamicREval(env : Environment) : DynamicRValue = {
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

  override def lEval(env : Environment) : LValue = {
    env.lookup(id) match {
      case Environment.StaticValueItem(e) => e.asInstanceOf[LValue]
      case _                              => Logger.error(id ++ " is not a static l-value")
    }
  }

  override def rEval(env : Environment) : RValue = {
    env.lookup(id) match {
      case Environment.StaticValueItem(e) => e.asInstanceOf[RValue]
      case _                              => Logger.error(id ++ " is not a static r-value")
    }
  }

  override def dynamicREval(env : Environment) : DynamicRValue = {

    env.lookup(id) match {
      case Environment.VariableItem(tcId, scType) =>
        new DynamicRValue(List(), l4.UnresolvedAccess(tcId, None, None, None))
      case Environment.StaticValueItem(v) =>
        v match {
          case FieldLValue(tcId) =>
            new DynamicRValue(List(),
              l4.FieldAccess(
                tcId,
                l4.CurrentLevelSpecification(),
                l4.IntegerConstant(0),
                -1)) // FIXME@Christian array index

          case _ => Logger.error(id ++ " is not a Field")
        }

      case _ => Logger.error(id ++ " is not a variable")
    }

  }

  override def scType(env : Environment) : ScType = {
    env.lookup(id) match {
      case Environment.StaticValueItem(e) => e.scType
      case _                              => ???
    }
  }
}

case class StringConstant(val value : String) extends Expression /* {
  def progressToIr : ir.StringConstant = ir.StringConstant(value)
}*/

case class IntegerConstant(val v : Long) extends Number {
  override def value = v
  //  def progressToIr : ir.IntegerConstant = ir.IntegerConstant(v)
}

case class FloatConstant(val v : Double) extends Number {
  override def value = v
  //  def progressToIr : ir.FloatConstant = ir.FloatConstant(v)
}

case class Variable(val id : String, val datatype : ScType) extends Expression {
  override def scType(env : Environment) = datatype
}

case class FunctionCallExpression(val identifier : String, val arguments : List[Expression]) extends Expression {
  //  def progressToIr : ir.FunctionCallExpression = {
  //    ir.FunctionCallExpression(ir.StringConstant(identifier.progressToIr.asInstanceOf[ir.StringConstant].value),
  //      arguments.map(s => s.progressToIr).to[ListBuffer])
  //  }
}

case class BinaryExpression(var operator : String, var left : Expression, var right : Expression) extends Expression {

  override def dynamicREval(env : Environment) : DynamicRValue = {
    val leftTc = left.dynamicREval(env)
    val rightTc = right.dynamicREval(env)

    /// @todo: Auxiliary computations
    new DynamicRValue(
      List(),
      l4.BinaryExpression(operator, leftTc.tcExpression, rightTc.tcExpression))

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

