package exastencils.baseExt.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting._
import exastencils.util.l4.L4_LevelCollector

/// L4_LevelScope

object L4_LevelScope {
  def apply(level : L4_LevelSpecification, body : List[L4_Statement]) = new L4_LevelScope(level, body.to[ListBuffer])
}

case class L4_LevelScope(var level : L4_LevelSpecification, var body : ListBuffer[L4_Statement]) extends L4_Statement {
  override def prettyprint(out : PpStream) = out << "@" << level << " {\n" <<< (body, "\n") << "\n}"
  override def progress = Logger.error("Trying to progress " + this.getClass.getName + " which is unsupported")
}

/// L4_ResolveLevelScopes

object L4_ResolveLevelScopes extends DefaultStrategy("Resolve level-specific scopes") {
  var levelCollector = new L4_LevelCollector
  this.register(levelCollector)

  // Flatten leveled scope or remove completely
  this += new Transformation("Resolve", {
    case scope : L4_LevelScope => scope.level match {
      case s : L4_SingleLevel =>
        if (levelCollector.getCurrentLevel == s.level)
          scope.body
        else
          List()
      case s : L4_LevelList   =>
        if (s.contains(levelCollector.getCurrentLevel))
          scope.body
        else
          List()
    }
  })
}
