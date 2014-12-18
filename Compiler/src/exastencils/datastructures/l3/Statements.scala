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

  def mangleName(args : List[StaticValue]) : String = ???

  /** Return the target code of an instance of this function. */
  def instanceTc(args : List[StaticValue], env : Environment, predefinedTcId : Option[String]) : TargetCode = {

    val tcId = predefinedTcId match {
      case Some(i) => i
      case None    => mangleName(args)
    }

    val tcArgs = dynamicArguments map {
      case Variable(id, scType) =>
        l4.Variable(l4.BasicIdentifier(id), scType.toTcType)
    }

    TargetCode(
      new l4.FunctionStatement(
        l4.LeveledIdentifier(tcId, l4.AllLevelsSpecification()),
        l4.UnitDatatype(),
        tcArgs,
        List()))

  }
}

case class FunctionCallStatement(val call : FunctionCallExpression) extends Statement {

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

    val f = env.lookup(functionId) match {
      case Environment.FunctionItem(f) => f
      case _                           => Logger.error("Expected a function")
    }

    val evaluated_args = arguments map { a =>
      a.scType(env) match {
        case FieldDatatype()   => a.lEval(env)
        case StencilDatatype() => a.rEval(env)
        case _                 => new Exception("Static argument expected.")
      }
    }

    f.instanceTc(List(), env, instantiationId)
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

case class AssignmentStatement(val dest : IdentifierExpression, val src : Expression, val op : String) extends Statement {

}

