package exastencils.solver.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3.L3_Access
import exastencils.base.l3.L3_DeclarationLevelSpecification
import exastencils.base.l3.L3_LevelSpecification
import exastencils.base.l3.L3_SingleLevel
import exastencils.base.l3.L3_Statement
import exastencils.core.Duplicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_SolverModification

abstract class L3_SolverModification extends L3_Statement {
  def levels : Option[L3_DeclarationLevelSpecification]
  def levels_=(nju : Option[L3_DeclarationLevelSpecification])

  def unfold() : List[L3_SolverModification] = {
    val levelList = L3_LevelSpecification.extractLevelListDefAll(levels)
    levelList.map(level => {
      val nju = Duplicate(this)
      nju.levels = Some(L3_SingleLevel(level))
      nju
    })
  }

  override def progress = Logger.error("Trying to progress L3_SolverModification; unsupported")
}

/// L3_SolverModificationForObject

case class L3_SolverModificationForObject(
    var modification : String,
    var target : String,
    var access : L3_Access,
    var levels : Option[L3_DeclarationLevelSpecification]) extends L3_SolverModification {

  override def prettyprint(out : PpStream) = {
    if ("replace" != modification) Logger.warn(s"Unknown modification $modification")
    out << "replace " << target
    if (levels.isDefined) out << " @" << levels.get
    out << " with " << access
  }
}

/// L3_SolverModificationForStage

object L3_SolverModificationForStage {
  def apply(modification : String, target : String, statements : List[L3_Statement], levels : Option[L3_DeclarationLevelSpecification]) =
    new L3_SolverModificationForStage(modification, target, statements.to[ListBuffer], levels)
}

case class L3_SolverModificationForStage(
    var modification : String,
    var target : String,
    var statements : ListBuffer[L3_Statement],
    var levels : Option[L3_DeclarationLevelSpecification]) extends L3_SolverModification {

  override def prettyprint(out : PpStream) = {
    modification match {
      case "append" | "prepend" => out << modification << " to"
      case "replace"            => out << modification
    }
    out << " " << target
    if (levels.isDefined) out << " @" << levels.get
    out << " {\n" <<< (statements, "\n") << "\n}"
  }
}

/// L3_UnfoldSolverModifications

object L3_UnfoldSolverModifications extends DefaultStrategy("Unfold leveled L3 solver modifications") {
  this += Transformation("Process new modifications", {
    case mod : L3_SolverModification => mod.unfold()
  })
}
