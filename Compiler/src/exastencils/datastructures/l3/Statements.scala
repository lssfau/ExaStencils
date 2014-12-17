package exastencils.datastructures.l3

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._
import exastencils.knowledge
import exastencils.omp
import exastencils.util._
import exastencils.datastructures.l3._

abstract class Statement extends Node with ProgressibleToL4 {
  override def toDc(env : Environment) : DestinationCode = {
    DestinationCode()
    // throw new Exception("Not implemented")
  }
}

case class FunctionStatement(
    val identifier : String,
    val returntype : ScType,
    val arguments : List[Variable],
    val statements : List[Statement]) extends Statement {
}

case class FunctionCallStatement(var call : FunctionCallExpression) extends Statement {

}

case class FunctionInstantiationStatement(
    val functionId : String,
    val arguments : List[Expression],
    val level : LevelSpecification) extends Statement with ProgressibleToL4 {

  override def toDc(env : Environment) : DestinationCode = {

    DestinationCode(
      new l4.FunctionStatement(
        l4.LeveledIdentifier(functionId, l4.AllLevelsSpecification()),
        l4.UnitDatatype(),
        List(),
        List()))

  }

}

case class VariableDeclarationStatement(val id : String, val scType : ScType, val expression : Option[Expression] = None) extends Statement {

}

case class ValueDeclarationStatement(var identifier : String, var datatype : ScType, var expression : Expression) extends Statement {

}

case class AssignmentStatement(var dest : String, var src : Expression, var op : String) extends Statement {

}

