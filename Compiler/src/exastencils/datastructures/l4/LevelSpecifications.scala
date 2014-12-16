package exastencils.datastructures.l4

import scala.collection.mutable.HashSet

import exastencils.datastructures._
import exastencils.prettyprinting._

trait LevelSpecification extends Node with Annotatable with PrettyPrintable

// helper traits for checking the parse tree
trait DeclarationLevelSpecification // can be used for declarations, e.g., functions
trait AccessLevelSpecification // can be used for accesses, e.g., in fields or function calls

case class SingleLevelSpecification(var level : Int) extends LevelSpecification with DeclarationLevelSpecification with AccessLevelSpecification {
  def prettyprint(out : PpStream) = { out << level }

  override def toString() = level.toString
}

case class RangeLevelSpecification(var begin : LevelSpecification, var end : LevelSpecification) extends LevelSpecification with DeclarationLevelSpecification {
  def prettyprint(out : PpStream) = { out << '(' << begin << " to " << end << ')' }
}

case class ListLevelSpecification(var levels : HashSet[LevelSpecification]) extends LevelSpecification with DeclarationLevelSpecification {
  def this() = this(HashSet[LevelSpecification]())
  def this(level : LevelSpecification) = this(HashSet(level))
  def add(level : LevelSpecification) = levels += level

  def prettyprint(out : PpStream) = { out << '(' <<< (levels, ", ") << ')' }
}

case class CurrentLevelSpecification() extends LevelSpecification with DeclarationLevelSpecification with AccessLevelSpecification {
  def prettyprint(out : PpStream) = { out << "current" }
}

case class CoarserLevelSpecification() extends LevelSpecification with DeclarationLevelSpecification with AccessLevelSpecification {
  def prettyprint(out : PpStream) = { out << "coarser" }
}

case class FinerLevelSpecification() extends LevelSpecification with DeclarationLevelSpecification with AccessLevelSpecification {
  def prettyprint(out : PpStream) = { out << "finer" }
}

case class CoarsestLevelSpecification() extends LevelSpecification with DeclarationLevelSpecification with AccessLevelSpecification {
  def prettyprint(out : PpStream) = { out << "coarsest" }
}

case class FinestLevelSpecification() extends LevelSpecification with DeclarationLevelSpecification with AccessLevelSpecification {
  def prettyprint(out : PpStream) = { out << "finest" }
}

case class NegatedLevelSpecification(var l : LevelSpecification) extends LevelSpecification with DeclarationLevelSpecification {
  def prettyprint(out : PpStream) = { out << "not(" << l << ')' }
}

case class RelativeLevelSpecification(var operator : String, var base : LevelSpecification, var offset : Int) extends LevelSpecification with DeclarationLevelSpecification with AccessLevelSpecification {
  def prettyprint(out : PpStream) = { out << '(' << base << ' ' << operator << ' ' << offset << ')' }
}

case class AllLevelsSpecification() extends LevelSpecification with DeclarationLevelSpecification {
  def prettyprint(out : PpStream) = { out << "all" }
}

