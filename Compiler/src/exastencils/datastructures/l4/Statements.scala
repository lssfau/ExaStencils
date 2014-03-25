package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer
import exastencils.datastructures._
import exastencils.datastructures.l4._

abstract class Statement extends Node

case class VariableDeclarationStatement(var identifier : String, var datatype : Datatype, var expression : Option[Expression] = None)
  extends Statement

case class DomainDeclarationStatement(name : String) extends Statement

case class AssignmentStatement(var identifier : String, var expression : Expression)
  extends Statement

object LoopOverDomainStatement {
  object AreaType extends Enumeration {
    type Area = Value
    val Domain, Inner, Boundary = Value
    def get(key : String) = {
      key match {
        case "domain"   => Domain
        case "inner"    => Inner
        case "boundary" => Boundary
        case _          => throw new MatchError(s"""Invalid identifier "$key"""")
      }
    }
  }
  type Area = AreaType.Area

  object OrderType extends Enumeration {
    type Order = Value
    val Lexical, RedBlack = Value
    def get(key : String) = {
      key match {
        case "lexical"  => Lexical
        case "redblack" => RedBlack
        case _          => throw new MatchError(s"""Invalid identifier "$key"""")
      }
    }
  }
  type Order = OrderType.Order

  abstract class Blocksize
  class Blocksize2D(val X : Int, val Y : Int) extends Blocksize
  class Blocksize3D(val X : Int, val Y : Int, val Z : Int) extends Blocksize

  def apply(area : String, levels : Option[Int], order : Option[String], blocksize : Option[Blocksize], statements : List[Statement]) =
    new LoopOverDomainStatement(AreaType.get(area), levels, OrderType.get(order.getOrElse("lexical")), blocksize.getOrElse(new Blocksize2D(1, 1)), statements)
}

case class LoopOverDomainStatement(area : LoopOverDomainStatement.Area, levels : Option[Int], order : LoopOverDomainStatement.Order, blocksize : LoopOverDomainStatement.Blocksize, var statements : List[Statement])
  extends Statement

case class FunctionStatement(name : String, var returntype : Datatype, var arguments : List[Variable], var statements : List[Statement])
  extends Statement

case class RepeatUntilStatement(var comparison : BooleanExpression, var statements : List[Statement]) extends Statement

case class FunctionCallStatement(name : String, var arguments : List[Expression]) extends Statement

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
      case "bcDir0"      => bcDir0 = t.value.toBoolean
      case _             => exastencils.core.ERROR(s"Unknown option ${t.key} = ${t.value}")
    }
  }
}

