package exastencils.datastructures.l3

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l3._

abstract class Statement extends Node

case class FunctionStatement(var identifier : String,
    var returntype : ScType,
    var arguments : List[Variable],
    var statements : List[Statement]) extends Statement {
}

case class FunctionCallStatement(var call : FunctionCallExpression) extends Statement {
  //  def progressToIr : ir.ExpressionStatement = {
  //    ir.ExpressionStatement(call.progressToIr)
  //  }
}

case class FunctionInstantiationStatement(
    var functionId : String,
    var instantiationId : Option[String],
    var arguments : List[Expression],
    var level : LevelSpecification) extends Statement with ProgressableToL4 {

  override def progressToL4 : l4.FunctionStatement = {
    ???
    //    val funcTemplate = StateManager.root.asInstanceOf[l3.Root].getFunctionByIdentifier(functionId).get
    //
    //    new l4.FunctionStatement(
    //      new l4.LeveledIdentifier(instantiationId.get, level.progressToL4),
    //      funcTemplate.returntype.progressToL4,
    //      funcTemplate.arguments.map(arg -> arg.progressToL4),
    //      funcTemplate.statements.map(arg -> arg.progressToL4))
  }

  override def toDc(env : Environment) : DestinationCode = {
    ???
  }

}

case class VariableDeclarationStatement(val id : String, val scType : ScType, val expression : Option[Expression] = None) extends Statement {

}

case class ValueDeclarationStatement(var identifier : String, var datatype : ScType, var expression : Expression) extends Statement {
  //  def progressToIr : ir.ValueDeclarationStatement = {
  //    ir.ValueDeclarationStatement(datatype.progressToIr,
  //      identifier.progressToIr.asInstanceOf[ir.StringConstant].value,
  //      expression.get.progressToIr
  //  }
  //  def progressToIr : ir.Statement = ir.NullStatement
}

case class AssignmentStatement(var dest : String, var src : Expression, var op : String) extends Statement {
  //  def progressToIr : ir.AssignmentStatement = {
  //    ir.AssignmentStatement(dest.progressToIr, src.progressToIr, op)
  //  }
}

