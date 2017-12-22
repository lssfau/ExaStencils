package exastencils.baseExt.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.baseExt.l4.L4_LevelScope
import exastencils.datastructures._
import exastencils.prettyprinting._
import exastencils.util.l3.L3_LevelCollector

/// L3_LevelScope

object L3_LevelScope {
  def apply(level : L3_LevelSpecification, body : List[L3_Statement]) = new L3_LevelScope(level, body.to[ListBuffer])
}

case class L3_LevelScope(var level : L3_LevelSpecification, var body : ListBuffer[L3_Statement]) extends L3_Statement {
  override def prettyprint(out : PpStream) = out << "@" << level << " {\n" <<< (body, "\n") << "\n}"
  override def progress = ProgressLocation(L4_LevelScope(level.progress, body.map(_.progress)))
}

/// L3_ResolveLevelScopes

object L3_ResolveLevelScopes extends DefaultStrategy("Resolve level-specific scopes") {
  var levelCollector = new L3_LevelCollector
  this.register(levelCollector)

  // Flatten leveled scope or remove completely
  this += new Transformation("Resolve", {
    case scope : L3_LevelScope => scope.level match {
      case s : L3_SingleLevel =>
        if (levelCollector.getCurrentLevel == s.level)
          scope.body
        else
          List()
      case s : L3_LevelList   =>
        if (s.contains(levelCollector.getCurrentLevel))
          scope.body
        else
          List()
    }
  })
}
