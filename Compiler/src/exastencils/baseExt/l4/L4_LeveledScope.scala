package exastencils.baseExt.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L4_LeveledScope

object L4_LeveledScope {
  def apply(level : L4_LevelSpecification, body : List[L4_Statement]) = new L4_LeveledScope(level, body.to[ListBuffer])
}

case class L4_LeveledScope(var level : L4_LevelSpecification, var body : ListBuffer[L4_Statement]) extends L4_Statement {
  override def prettyprint(out : PpStream) = out << "@" << level << " {\n" <<< (body, "\n") << "\n}"
  override def progress = Logger.error("Trying to progress " + this.getClass.getName + " which is unsupported")
}

/// L4_ResolveLeveledScopes

object L4_ResolveLeveledScopes extends DefaultStrategy("Resolve leveled scopes") {
  var levelCollector = new L4_LevelCollector
  this.register(levelCollector)

  // Flatten leveled scope or remove completely
  this += new Transformation("Resolve leveled scopes", {
    case scope : L4_LeveledScope => scope.level match {
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
