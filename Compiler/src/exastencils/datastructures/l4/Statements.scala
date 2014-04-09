package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer
import exastencils.datastructures._
import exastencils.datastructures.l4._

abstract class Statement extends Node

case class VariableDeclarationStatement(var identifier : String, var datatype : Datatype, var expression : Option[Expression] = None)
  extends Statement

case class DomainDeclarationStatement(name : String) extends Statement

case class AssignmentStatement(var identifier : Identifier, var expression : Expression)
  extends Statement

case class LoopOverDomainStatement(iterationset : String, blocksize : Option[Index], var statements : List[Statement])
  extends Statement

case class IterationSetDeclarationStatement(identifier : Identifier, begin : Index, end : Index, increment : Option[Index]) extends Statement

case class FunctionStatement(var identifier : Identifier, var returntype : Datatype, var arguments : List[Variable], var statements : List[Statement])
  extends Statement

case class RepeatUpStatement(number : Int, var statements : List[Statement]) extends Statement

case class RepeatUntilStatement(var comparison : BooleanExpression, var statements : List[Statement]) extends Statement

case class ReductionStatement(var statement : Statement) extends Statement

case class FunctionCallStatement(identifier : Identifier, var arguments : List[Expression]) extends Statement

case class ConditionalStatement(var expression : BooleanExpression, var statements : List[Statement]) extends Statement

case class FieldDeclarationStatement(name : String, var datatype : Datatype, var offset : Index, var level : Option[LevelSpecification]) extends Statement {
  var communicate = false
  var ghostlayers = 0
  var padding = 0
  var slots = 1
  var bcDir0 = false
  def set(t : TempOption) { // FIXME hack
    t.key match {
      case "communicate" => communicate = t.value.toBoolean
      case "ghostlayers" => ghostlayers = t.value.toInt
      case "padding"     => padding = t.value.toInt
      case "slots"       => slots = t.value.toInt
      case "bcDir"       => bcDir0 = t.value.toBoolean
      case _             => exastencils.core.ERROR(s"Unknown option ${t.key} = ${t.value}")
    }
  }
}
