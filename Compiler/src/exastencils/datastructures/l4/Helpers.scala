package exastencils.datastructures.l4

import scala.collection.mutable.HashSet
import exastencils.datastructures._

trait LevelSpecification extends Node with Annotatable

case class SingleLevelSpecification(var level : Int) extends LevelSpecification {
  override def toString() = level.toString
}

case class RangeLevelSpecification(var begin : Int, var end : Int) extends LevelSpecification

case class ListLevelSpecification(var levels : HashSet[LevelSpecification]) extends LevelSpecification {
  def this() = this(HashSet[LevelSpecification]())
  def this(level : LevelSpecification) = this(HashSet(level))
  def add(level : LevelSpecification) = levels += level
}

case class CurrentLevelSpecification() extends LevelSpecification
case class CoarserLevelSpecification() extends LevelSpecification
case class FinerLevelSpecification() extends LevelSpecification

case class TempOption(val key : String, val value : String) extends Annotatable

trait Index extends Annotatable
case class Index2D(var x : Int, var y : Int) extends Index
case class Index3D(var x : Int, var y : Int, var z : Int) extends Index
