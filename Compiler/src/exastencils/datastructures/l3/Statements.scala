package exastencils.datastructures.l3

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l3._

abstract class Statement extends Node with ProgressableToL4

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

  override def toTc(env : Environment) : TargetCode = {
    env.bind(id, Environment.FunctionItem(this))
    TargetCode()
  }

  def mangleName(args : List[StaticValue]) : String = ???

  /** Return the target code of an instance of this function. */
  def instanceTc(givenStaticArgs : List[StaticValue], env : Environment, predefinedTcId : Option[String]) : TargetCode = {

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

    println(arguments)
    println(staticArguments)
    println(givenStaticArgs)
    println(body_env)

    // transform to target code and concat
    val tcBody = (body map { _.toTc(body_env) }).foldLeft(TargetCode()) { (x : TargetCode, y : TargetCode) => x ++ y }

    TargetCode(
      new l4.FunctionStatement(
        l4.LeveledIdentifier(tcId, l4.AllLevelsSpecification()),
        l4.UnitDatatype(),
        tcArgs,
        List()))

  }
}

case class FunctionCallStatement(val call : FunctionCallExpression) extends Statement {
  override def toTc(env : Environment) : TargetCode = {
    throw new Exception("Not implemented")
  }
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
      case Environment.FunctionItem(fdef) => fdef
      case _                              => Logger.error("Expected a function")
    }

    println(arguments)

    val evaluated_args = arguments map { a =>
      a.scType(env) match {
        case FieldDatatype()   => a.lEval(env)
        case StencilDatatype() => a.rEval(env)
        case _                 => throw new Exception("Static argument expected.")
      }
    }

    println(evaluated_args)

    f.instanceTc(evaluated_args, env, instantiationId)
  }

}

case class VariableDeclarationStatement(
    val id : String,
    val scType : ScType,
    val expression : Option[Expression] = None) extends Statement {

  override def toTc(env : Environment) : TargetCode = {
    throw new Exception("Not implemented")
  }
}

case class ValueDeclarationStatement(
    val identifier : String,
    val datatype : ScType,
    val expression : Expression) extends Statement {

  override def toTc(env : Environment) : TargetCode = {
    throw new Exception("Not implemented")
  }
}

// @todo The op parameter is unnecessary
case class AssignmentStatement(
    val dest : IdentifierExpression,
    val src : Expression,
    val op : String) extends Statement {

  override def toTc(env : Environment) : TargetCode = {

    val lvalue = dest.lEval(env)

    // depending on the type
    val tcAccess = lvalue match {
      case FieldLValue(tcId) =>

        new l4.FieldAccess(
          tcId,
          l4.CurrentLevelSpecification(),
          l4.IntegerConstant(0),
          0)

      case _ => ???
    }

    val tcRhs = src.dynamicREval(env)

    TargetCode(l4.AssignmentStatement(tcAccess, ???, op))

  }
}

