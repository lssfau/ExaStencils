package exastencils.datastructures.l4

import scala.collection.mutable.HashSet

import exastencils.datastructures._
import exastencils.prettyprinting._

trait LevelSpecification extends Node with Annotatable with PrettyPrintable

// helper traits for checking the parse tree
trait DeclarationLevelSpecification extends LevelSpecification // can be used for declarations, e.g., functions
trait AccessLevelSpecification extends LevelSpecification // can be used for accesses, e.g., in fields or function calls

case class SingleLevelSpecification(var level : Int) extends DeclarationLevelSpecification with AccessLevelSpecification {
  def prettyprint(out : PpStream) = { out << level }

  override def toString() = level.toString
}

case class RangeLevelSpecification(var begin : LevelSpecification, var end : LevelSpecification) extends DeclarationLevelSpecification {
  def prettyprint(out : PpStream) = { out << '(' << begin << " to " << end << ')' }
}

case class ListLevelSpecification(var levels : HashSet[LevelSpecification]) extends DeclarationLevelSpecification {
  def this() = this(HashSet[LevelSpecification]())
  def this(level : LevelSpecification) = this(HashSet(level))
  def add(level : LevelSpecification) = levels += level

  def prettyprint(out : PpStream) = { out << '(' <<< (levels, ", ") << ')' }

  def flatten() : Unit = {
    levels.foreach(elem => elem match {
      case ListLevelSpecification(x) =>
        levels.++=(x)
        levels.remove(elem)
      case _ =>
    })
  }

  def cleanup() : Unit = {
    levels.foreach(elem => elem match {
      case NegatedLevelSpecification(ListLevelSpecification(x)) => {
        levels.--=(x)
        levels.remove(elem)
      }
      case _ =>
    })
  }

  def contains(level : Integer) : Boolean = {
    levels.find(l => l.isInstanceOf[SingleLevelSpecification] && l.asInstanceOf[SingleLevelSpecification].level == level).isDefined
  }
}

case class CurrentLevelSpecification() extends DeclarationLevelSpecification with AccessLevelSpecification {
  def prettyprint(out : PpStream) = { out << "current" }
}

case class CoarserLevelSpecification() extends DeclarationLevelSpecification with AccessLevelSpecification {
  def prettyprint(out : PpStream) = { out << "coarser" }
}

case class FinerLevelSpecification() extends DeclarationLevelSpecification with AccessLevelSpecification {
  def prettyprint(out : PpStream) = { out << "finer" }
}

case class CoarsestLevelSpecification() extends DeclarationLevelSpecification with AccessLevelSpecification {
  def prettyprint(out : PpStream) = { out << "coarsest" }
}

case class FinestLevelSpecification() extends DeclarationLevelSpecification with AccessLevelSpecification {
  def prettyprint(out : PpStream) = { out << "finest" }
}

case class NegatedLevelSpecification(var l : ListLevelSpecification) extends DeclarationLevelSpecification {
  def prettyprint(out : PpStream) = { out << "not"; l.prettyprint(out) }
}

case class RelativeLevelSpecification(var operator : String, var base : LevelSpecification, var offset : Int) extends DeclarationLevelSpecification with AccessLevelSpecification {
  def prettyprint(out : PpStream) = { out << '(' << base << ' ' << operator << ' ' << offset << ')' }
}

case class AllLevelsSpecification() extends DeclarationLevelSpecification {
  def prettyprint(out : PpStream) = { out << "all" }
}

