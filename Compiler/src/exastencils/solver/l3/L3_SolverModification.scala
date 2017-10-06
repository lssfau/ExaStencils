package exastencils.solver.l3

import scala.collection.mutable.ListBuffer

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

object L3_SolverModification {
  def apply(modification : String, target : String, statements : List[L3_Statement], levels : Option[L3_DeclarationLevelSpecification]) =
    new L3_SolverModification(modification, target, statements.to[ListBuffer], levels)
}

case class L3_SolverModification(
    var modification : String,
    var target : String,
    var statements : ListBuffer[L3_Statement],
    var levels : Option[L3_DeclarationLevelSpecification]) extends L3_Statement {

  override def prettyprint(out : PpStream) = {
    modification match {
      case "append" | "prepend" => out << modification << " to"
      case "replace"            => out << modification
    }
    out << " " << target
    if (levels.isDefined) out << " @" << levels.get
    out << " {\n" <<< (statements, "\n") << "\n}"
  }

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

/// L3_UnfoldSolverModifications

object L3_UnfoldSolverModifications extends DefaultStrategy("Unfold leveled L3 solver modifications") {
  this += Transformation("Process new modifications", {
    case mod : L3_SolverModification => mod.unfold()
  })
}
