package exastencils.datastructures.l4

import scala.collection.mutable.HashSet
import exastencils.datastructures._

trait LevelSpecification extends Node with Annotatable

// helper traits for checking the parse tree
trait DeclarationLevelSpecification // can be used for declarations, e.g., functions
trait AccessLevelSpecification // can be used for accesses, e.g., in fields or function calls

case class SingleLevelSpecification(var level : Int) extends LevelSpecification with DeclarationLevelSpecification with AccessLevelSpecification {
  override def toString() = level.toString
}

case class RangeLevelSpecification(var begin : LevelSpecification, var end : LevelSpecification) extends LevelSpecification with DeclarationLevelSpecification

case class ListLevelSpecification(var levels : HashSet[LevelSpecification]) extends LevelSpecification with DeclarationLevelSpecification {
  def this() = this(HashSet[LevelSpecification]())
  def this(level : LevelSpecification) = this(HashSet(level))
  def add(level : LevelSpecification) = levels += level
}

case class CurrentLevelSpecification() extends LevelSpecification with DeclarationLevelSpecification with AccessLevelSpecification
case class CoarserLevelSpecification() extends LevelSpecification with DeclarationLevelSpecification with AccessLevelSpecification
case class FinerLevelSpecification() extends LevelSpecification with DeclarationLevelSpecification with AccessLevelSpecification
case class CoarsestLevelSpecification() extends LevelSpecification with DeclarationLevelSpecification with AccessLevelSpecification
case class FinestLevelSpecification() extends LevelSpecification with DeclarationLevelSpecification with AccessLevelSpecification
case class NegatedLevelSpecification(var l : LevelSpecification) extends LevelSpecification with DeclarationLevelSpecification
case class RelativeLevelSpecification(var operator : String, var base : LevelSpecification, var offset : Int) extends LevelSpecification with DeclarationLevelSpecification with AccessLevelSpecification

case class AllLevelsSpecification() extends LevelSpecification with DeclarationLevelSpecification
