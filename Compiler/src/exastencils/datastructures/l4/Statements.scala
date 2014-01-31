package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer
import exastencils.datastructures._
import exastencils.datastructures.l4._

abstract class Statement extends Node

case class VariableDeclarationStatement(var variable : Variable, var expression : Option[Expression] = None)
    extends Statement

case class AssignmentStatement(var identifier : Identifier, var expression : Expression)
    extends Statement

case class ForStatement(var begin : VariableDeclarationStatement, end : Expression, inc : Expression, var statements : List[Statement])
    extends Statement

case class LoopOverDomainStatement(area : String, level : String, order : Option[String], blocksize : Any, var statements : List[Statement])
    extends Statement

case class FunctionStatement(name : String, var returntype : Datatype , var arguments : List[Variable], var statements : List[Statement])
    extends Statement
