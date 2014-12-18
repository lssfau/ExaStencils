package exastencils.datastructures.l3

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l3._

abstract class Statement extends Node with ProgressableToL4 {
  override def toTc(env : Environment) : TargetCode = {
    throw new Exception("Not implemented")
  }
}

case class FunctionStatement(
  val id : String,
  val returntype : ScType,
  val arguments : List[Variable],
  val statements : List[Statement])
    extends Statement {

  // runtime arguments
  def dynamicArguments : List[Variable] = {
    // return only dynamic arguments
    arguments filter { a => !a.datatype.isStatic }
  }

  def staticArguments : List[Variable] = {
    arguments filter { a => a.datatype.isStatic }
  }

  override def toTc(env : Environment) : TargetCode = {
    env.bind(id, Environment.FunctionItem(this))
    TargetCode()
  }
}

case class FunctionCallStatement(var call : FunctionCallExpression) extends Statement {

}

case class FunctionInstantiationStatement(
    val functionId : String,
    val instantiationId : Option[String],
    val arguments : List[Expression],
    val level : LevelSpecification) extends Statement with ProgressableToL4 {

  // def progressToL4 : l4.FunctionStatement = {
  // ???
  //    val funcTemplate = StateManager.root.asInstanceOf[l3.Root].getFunctionByIdentifier(functionId).get
  //
  //    new l4.FunctionStatement(
  //      new l4.LeveledIdentifier(instantiationId.get, level.progressToL4),
  //      funcTemplate.returntype.progressToL4,
  //      funcTemplate.arguments.map(arg -> arg.progressToL4),
  //      funcTemplate.statements.map(arg -> arg.progressToL4))
  //}

  override def toTc(env : Environment) : TargetCode = {

    val f = env.lookup(functionId)

    val evaluated_args = arguments map { a =>
      /// @todo: determine the type of the expression
      /// Depending on that we have to do l or r eval
      //a.lEval(env)
      None
    }

    TargetCode(
      new l4.FunctionStatement(
        l4.LeveledIdentifier(functionId, l4.AllLevelsSpecification()),
        l4.UnitDatatype(),
        List(),
        List()))

  }

}

case class VariableDeclarationStatement(
    val id : String,
    val scType : ScType,
    val expression : Option[Expression] = None) extends Statement {

}

case class ValueDeclarationStatement(
    val identifier : String,
    val datatype : ScType,
    val expression : Expression) extends Statement {

}

case class AssignmentStatement(var dest : IdentifierExpression, var src : Expression, var op : String) extends Statement {

}

