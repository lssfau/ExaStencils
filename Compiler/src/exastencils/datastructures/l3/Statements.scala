package exastencils.datastructures.l3

import TcbImplicits._
import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l3._
import exastencils.logger._

abstract class Statement extends Node {
  def writeTc(ctx : Context)

  /**
    * Execute statement at compile time.
    *
    * If it returns some value a return statement has been executed.
    */
  def exec(ctx : Context) : Option[StaticValue] = {
    throw new Exception("This statement can not be carried out at compile time.")
  }
}

case class FunctionDefinitionStatement(
  val id : String,
  val returntype : ScType,
  val arguments : List[FunctionArgument],
  val body : List[Statement])
    extends Statement {

  override def writeTc(ctx : Context) {
    exec(ctx)
  }

  override def exec(ctx : Context) = {
    ctx.env.bind(id, StaticConstant(FunctionRValue(id, returntype, arguments, body)))
    None
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

  override def exec(ctx : Context) = {
    val return_value = call.eval(ctx).read

    if (return_value == NilStaticValue()) {
      None
    } else {
      Some(return_value)
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

    val fun = env.lookup(functionId).read match {
      case fun : FunctionRValue => fun
      case _                    => Logger.error("Expected a function")
    }

    // evaluate the static arguments
    val evaluated_args = arguments map { a => a.eval(ctx) }

    fun.writeTcInstance(ctx, evaluated_args, instantiationId)
  }
}

case class StaticAssignmantStatement(
    val id : String,
    val expr : Expression) extends Statement {

  override def writeTc(ctx : Context) {
    exec(ctx)
  }

  override def exec(ctx : Context) = {

    // create a new location for the value
    val location = new StaticLocation()
    ctx.env.bind(id, location)

    // set the value at the location to the value of the expression
    val evald_rhs = expr.eval(ctx).read
    location.write(evald_rhs)

    None
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

    /** @todo: Implement general assignment. */

    // compute the l-value
    // since l4 does not implement references this has to be a static evaluation
    val lvalue = dest.eval(ctx).read.asInstanceOf[FieldLValue]
    val tcRhs = src.dynamicREval(ctx)

    lvalue.writeTcAssignment(ctx.tcb, tcRhs)
  }
}

