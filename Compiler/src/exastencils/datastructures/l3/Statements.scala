package exastencils.datastructures.l3

import TcbImplicits._
import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l3._
import exastencils.logger._

abstract class Statement extends Node {
  def writeTc(ctx : Context)
}

case class FunctionDefinitionStatement(
  val id : String,
  val returntype : ScType,
  val arguments : List[FunctionArgument],
  val body : List[Statement])
    extends Statement {

  override def writeTc(ctx : Context) {
    ctx.env.bind(id, FunctionRValue(id, returntype, arguments, body))
  }

}

case class FunctionCallStatement(val call : FunctionCallExpression) extends Statement {

  override def writeTc(ctx : Context) {
    val expr = call.dynamicREval(ctx).tcExpression
    // convert the function call expression into a statement
    expr match {
      case expr : TcUnit => /* drop the unit type */
      case expr : l4.FunctionCallExpression =>
        ctx.tcb += l4.FunctionCallStatement(expr)
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

  override def writeTc(ctx : Context) {
    import ctx.env

    val fun = env.lookupRValue(functionId) match {
      case fun : FunctionRValue => fun
      case _                    => Logger.error("Expected a function")
    }

    /** @todo Clean up this cases. */

    // evaluate the static arguments
    val evaluated_args = arguments map { a =>
      a.scType(env) match {
        case FieldDatatype()      => a.lEval(ctx)
        case StencilDatatype()    => a.rEval(ctx)
        case StaticListDatatype() => a.rEval(ctx)
        case _                    => throw new Exception("Static argument expected.")
      }
    }

    fun.writeTcInstance(ctx, evaluated_args, instantiationId)
  }
}

case class StaticAssignmantStatement(
    val id : String,
    val expr : Expression) extends Statement {

  override def writeTc(ctx : Context) {

    /** @todo Clean implementation w/o exceptions */

    val evald_rhs = expr.scType(ctx.env) match {
      case FieldDatatype()      => expr.lEval(ctx)
      case StencilDatatype()    => expr.rEval(ctx)
      case StaticListDatatype() => expr.rEval(ctx)
      case _                    => throw new Exception("Static argument expected.")
    }
    ctx.env.bind(id, evald_rhs)
  }
}

case class VariableDeclarationStatement(
    val id : String,
    val scType : ScType,
    val expression : Option[Expression] = None) extends Statement {

  override def writeTc(ctx : Context) {
    throw new Exception("Not implemented")
  }
}

case class ValueDeclarationStatement(
    val identifier : String,
    val datatype : ScType,
    val expression : Expression) extends Statement {

  override def writeTc(ctx : Context) {
    throw new Exception("Not implemented")
  }
}

case class AssignmentStatement(
    val dest : IdentifierExpression,
    val src : Expression,
    val op : String) extends Statement {

  override def writeTc(ctx : Context) {
    if (op != "=") {
      Logger.error("%s is not a valid assignment operator.".format(op))
    }

    // compute the l-value
    // since l4 does not implement references this has to be a static evaluation
    val lvalue = dest.lEval(ctx)
    val tcRhs = src.dynamicREval(ctx)

    lvalue.writeTcAssignment(ctx.tcb, tcRhs)
  }
}

