package exastencils.l3

import scala.collection.mutable

class Environment() {
  val map = mutable.HashMap[String, Value]() 
  
  def lookup(symbol : String) : Value = map(symbol)
  
  def bind(symbol : String, value : Value) : Unit = {
    
    if (map contains symbol) {
      // do not allow rebinding
      throw new Exception("Symbol '" + symbol + "' already bound.")
    }
    
    map += symbol -> value
  }
  
  def bindNew(symbols : List[String], values : List[Value]) : Environment = {
    ???
  }
}

abstract class Expression() {  
  def eval(env : Environment) : Value
}

class Value extends Expression {
  def eval(env : Environment) = this
}

class Variable(val symbol : String) extends Expression {
  def eval(env : Environment) = env.lookup(symbol)
}

class Condition(
    val condition : Expression,
    val consequence : Expression,
    val alternative : Expression) extends Expression {
  def eval(env : Environment) = {
    
    condition.eval(env) match {
      case BooleanValue(value) =>
        if (value) {
          consequence.eval(env)
        } else {
          alternative.eval(env)
        }
      case _ =>
        throw new Exception("Boolean expected.")
    }
  }
}

class Apply(val fun_expr : Expression, val args : List[Expression]) {
  def eval(env : Environment) = {
    
    fun_expr.eval(env) match {
      case fun : Function => fun.apply(args.map { _.eval(env) })
      case _ => throw new Exception("Applying a non-function value.")
    }
  }
}

case class BooleanValue(val internal : Boolean) extends Value
case class RealNumber(val internal : Double) extends Value
case class IntegerNumber(val internal : Integer) extends Value
case class Cons(val first : Value, val second : Value) extends Value
case class Nil() extends Value

abstract class Function() extends Value {
  def apply(args : List[Value]) : Value
}

case class Lambda(
    val contained_env : Environment,
    val args_names : List[String], 
    val body : List[Expression]) extends Function {
  
  def apply(args : List[Value]) = {
    val env = contained_env.bindNew(args_names, args)
    
    // return the value of the last expression in the body
    var return_value : Value = Nil();
    for (body_expr <- body) {
      return_value = body_expr.eval(env)
    }
    
    return_value
  }
}



