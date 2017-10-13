package exastencils.base.l2

import exastencils.base.l3._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L2_VariableDeclaration

case class L2_VariableDeclaration(
    var name : String,
    var levels : Option[L2_DeclarationLevelSpecification],
    var datatype : L2_Datatype,
    var initialValue : Option[L2_Expression],
    var isConst : Boolean) extends L2_Statement {

  override def prettyprint(out : PpStream) = {
    out << (if (isConst) "Val " else "Var ") << name
    if (levels.isDefined) out << '@' << levels.get
    out << " : " << datatype
    if (initialValue.isDefined) out << " = " << initialValue.get
  }

  def unfold = {
    if (levels.isEmpty) Logger.warn(s"Unfolding un-leveled variable/ value declaration for $name")

    val levelList = L2_LevelSpecification.extractLevelListDefAll(levels)
    levelList.map(level => {
      val newDecl = Duplicate(this)
      newDecl.levels = Some(L2_SingleLevel(level))
      newDecl
    })
  }

  override def progress = {
    L3_VariableDeclaration(
      name,
      L2_ProgressOption(levels)(_.progress),
      datatype.progress,
      L2_ProgressOption(initialValue)(_.progress),
      isConst)
  }
}

/// L2_UnfoldLeveledVariableDeclarations

object L2_UnfoldLeveledVariableDeclarations extends DefaultStrategy("Unfold leveled value and variable declarations") {
  this += new Transformation("Unfold", {
    case decl : L2_VariableDeclaration if decl.levels.isDefined => decl.unfold
  })
}
