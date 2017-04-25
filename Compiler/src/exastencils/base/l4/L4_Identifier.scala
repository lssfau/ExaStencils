package exastencils.base.l4

import scala.collection.mutable.ListBuffer

import exastencils.core.Duplicate
import exastencils.datastructures.Node
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L4_HasIdentifier

trait L4_HasIdentifier {
  // read and write access to identifier
  var identifier : L4_Identifier
}

/// L4_Identifier

object L4_Identifier {
  def doDuplicate[T <: L4_HasIdentifier](toDuplicate : T, level : L4_LevelSpecification, onlyFor : Option[Int] = None) : ListBuffer[T] = {
    def duplicateInstance(newLevel : L4_LevelSpecification) = {
      val newInstance = Duplicate(toDuplicate)
      newInstance.identifier = L4_LeveledIdentifier(newInstance.identifier.name, newLevel)
      newInstance
    }

    var duplicated = ListBuffer[T]()
    level match {
      case L4_SingleLevel(lvl) if onlyFor.isDefined =>
        // only add declaration if filter is satisfied
        if (lvl == onlyFor.get)
          duplicated += duplicateInstance(level)

      case level @ (L4_CurrentLevel | L4_CoarserLevel | L4_FinerLevel) if onlyFor.isDefined =>
        Logger.warn("Level filter for declaration is required, but relative level is provided")
        duplicated += duplicateInstance(level)

      case level @ (L4_SingleLevel(_) | L4_CurrentLevel | L4_CoarserLevel | L4_FinerLevel) =>
        duplicated += duplicateInstance(level)

      case level : L4_LevelList =>
        level.levels.foreach(level => duplicated ++= doDuplicate(toDuplicate, level, onlyFor))

      case level : L4_LevelRange =>
        val (begin, end) = (level.begin.resolveLevel, level.end.resolveLevel)
        for (level <- math.min(begin, end) to math.max(begin, end))
          duplicated ++= doDuplicate(toDuplicate, L4_SingleLevel(level), onlyFor)

      case _ =>
        Logger.error(s"Invalid level specification for Value $toDuplicate: $level")
    }

    duplicated
  }
}

trait L4_Identifier extends Node with PrettyPrintable {
  // read and write access to name
  var name : String
  def fullName : String
}

/// L4_BasicIdentifier

case class L4_BasicIdentifier(var name : String) extends L4_Identifier {
  override def prettyprint(out : PpStream) = out << name
  override def fullName = name
}

/// L4_LeveledIdentifier

case class L4_LeveledIdentifier(var name : String, var level : L4_LevelSpecification) extends L4_Identifier {
  override def prettyprint(out : PpStream) = out << name << '@' << level
  override def fullName = name + "_" + level.prettyprint
}
