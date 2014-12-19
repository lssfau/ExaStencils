package exastencils.datastructures.l3

import TcbImplicits._
import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l3._

abstract class Statement extends Node {
  def writeTc(env : Environment, block : TcbBlock)
}

case class FunctionStatement(
  val id : String,
  val returntype : ScType,
  val arguments : List[Variable],
  val body : List[Statement])
    extends Statement {

  // runtime arguments
  def dynamicArguments : List[Variable] = {
    // return only dynamic arguments
    arguments filter { a => !a.datatype.isStatic }
  }

  def staticArguments : List[Variable] = {
    arguments filter { a => a.datatype.isStatic }
  }

  override def writeTc(env : Environment, block : TcbBlock) {
    env.bind(id, Environment.FunctionItem(this))
  }

  def mangleName(args : List[StaticValue]) : String = ???

  /** Return the target code of an instance of this function. */
  def writeTcInstance(env : Environment, block : TcbBlock, givenStaticArgs : List[StaticValue], predefinedTcId : Option[String]) {

    val tcId = predefinedTcId match {
      case Some(i) => i
      case None    => mangleName(givenStaticArgs)
    }

    val tcArgs = dynamicArguments map {
      case Variable(id, scType) =>
        l4.Variable(l4.BasicIdentifier(id), scType.toTcType)
    }

    // bind function arguments
    val body_env = new Environment(Some(env))
    for ((givenArg, requestedArg) <- givenStaticArgs zip staticArguments) {
      body_env.bind(requestedArg.id, Environment.StaticValueItem(givenArg))
    }

    // transform to target code and concat
    val funTc = new TcbFunction(tcId, tcArgs)
    body foreach { _.writeTc(body_env, funTc.body) }

    block += funTc
  }
}

case class FunctionCallStatement(val call : FunctionCallExpression) extends Statement {
  override def writeTc(env : Environment, block : TcbBlock) {
    throw new Exception("Not implemented")
  }
}

case class FunctionInstantiationStatement(
    val functionId : String,
    val instantiationId : Option[String],
    val arguments : List[Expression],
    val level : LevelSpecification) extends Statement {

  override def writeTc(env : Environment, block : TcbBlock) {

    val f = env.lookup(functionId) match {
      case Environment.FunctionItem(fdef) => fdef
      case _                              => Logger.error("Expected a function")
    }

    val evaluated_args = arguments map { a =>
      a.scType(env) match {
        case FieldDatatype()   => a.lEval(env)
        case StencilDatatype() => a.rEval(env)
        case _                 => throw new Exception("Static argument expected.")
      }
    }

    f.writeTcInstance(env, block, evaluated_args, instantiationId)
  }

}

case class VariableDeclarationStatement(
    val id : String,
    val scType : ScType,
    val expression : Option[Expression] = None) extends Statement {

  override def writeTc(env : Environment, block : TcbBlock) {
    throw new Exception("Not implemented")
  }
}

case class ValueDeclarationStatement(
    val identifier : String,
    val datatype : ScType,
    val expression : Expression) extends Statement {

  override def writeTc(env : Environment, block : TcbBlock) {
    throw new Exception("Not implemented")
  }
}

// FIXME@Christian The op parameter is unnecessary
case class AssignmentStatement(
    val dest : IdentifierExpression,
    val src : Expression,
    val op : String) extends Statement {

  override def writeTc(env : Environment, block : TcbBlock) {

    // compute the l-value
    // since l4 does not implement references this has to be a static evaluation
    val lvalue = dest.lEval(env)
    val tcRhs = src.dynamicREval(env)

    lvalue.writeTcAssignment(block, tcRhs)
  }
}

