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

  override def writeTc(env : Environment, block : TcbBlock) {
    env.bind(id, FunctionRValue(id, returntype, arguments, body))
  }

}

case class FunctionCallStatement(val call : FunctionCallExpression) extends Statement {

  override def writeTc(env : Environment, block : TcbBlock) {
    val expr = call.dynamicREval(env, block).tcExpression
    // convert the function call expression into a statement
    expr match {
      case expr : TcUnit => /* drop the unit type */
      case expr : l4.FunctionCallExpression =>
        block += l4.FunctionCallStatement(expr)
      case _ =>
        throw new Exception("Only a function call expression can be turned into a statement " ++
          "but we got something else.")
    }
  }
}

case class FunctionInstantiationStatement(
    val functionId : String,
    val instantiationId : Option[String],
    val arguments : List[Expression],
    val level : LevelSpecification) extends Statement {

  override def writeTc(env : Environment, block : TcbBlock) {

    val fun = env.lookupRValue(functionId) match {
      case fun : FunctionRValue => fun
      case _                    => Logger.error("Expected a function")
    }

    // evaluate the static arguments
    val evaluated_args = arguments map { a =>
      a.scType(env) match {
        case FieldDatatype()   => a.lEval(env)
        case StencilDatatype() => a.rEval(env)
        case _                 => throw new Exception("Static argument expected.")
      }
    }

    fun.writeTcInstance(env, block, evaluated_args, instantiationId)
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

    if (op != "=") {
      Logger.error("%s is not a valid assignment operator.".format(op))
    }

    // compute the l-value
    // since l4 does not implement references this has to be a static evaluation
    val lvalue = dest.lEval(env)
    val tcRhs = src.dynamicREval(env, block)

    lvalue.writeTcAssignment(block, tcRhs)
  }
}

