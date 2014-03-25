package exastencils.datastructures.l4

import scala.collection.mutable.HashSet
import exastencils.datastructures.Annotatable

trait LevelSpecification extends Annotatable

case class SingleLevelSpecification(level : Int) extends LevelSpecification

case class RangeLevelSpecification(begin : SingleLevelSpecification, end : SingleLevelSpecification) extends LevelSpecification

case class ListLevelSpecification(levels : HashSet[LevelSpecification]) extends LevelSpecification {
  def this() = this(HashSet[LevelSpecification]())
  def this(level : LevelSpecification) = this(HashSet(level))
  def add(level : LevelSpecification) = levels += level
}

case class TempOption(val key : String, val value : String) extends Annotatable

trait Index extends Annotatable
case class Index2D(x : Int, y : Int) extends Index
case class Index3D(x : Int, y : Int, z : Int) extends Index
