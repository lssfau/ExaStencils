package exastencils.base.l3

import exastencils.base.l4._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_VariableDeclaration

object L3_VariableDeclaration {
  def apply(access : L3_PlainVariableAccess)
  = new L3_VariableDeclaration(access.name, None, access.datatype, None, access.isConst)

  def apply(access : L3_PlainVariableAccess, initialValue : L3_Expression)
  = new L3_VariableDeclaration(access.name, None, access.datatype, Some(initialValue), access.isConst)
}

case class L3_VariableDeclaration(
    var name : String,
    var levels : Option[L3_DeclarationLevelSpecification],
    var datatype : L3_Datatype,
    var initialValue : Option[L3_Expression],
    var isConst : Boolean) extends L3_Statement {

  override def prettyprint(out : PpStream) = {
    out << (if (isConst) "Val " else "Var ") << name
    if (levels.isDefined) out << '@' << levels.get
    out << " : " << datatype
    if (initialValue.isDefined) out << " = " << initialValue.get
  }

  def unfold = {
    if (levels.isEmpty) Logger.warn(s"Unfolding un-leveled variable/ value declaration for $name")

    val levelList = L3_LevelSpecification.extractLevelListDefAll(levels)
    levelList.map(level => {
      val newDecl = Duplicate(this)
      newDecl.levels = Some(L3_SingleLevel(level))
      newDecl
    })
  }

  override def progress = {
    L4_VariableDeclaration(
      name,
      L3_ProgressOption(levels)(_.progress),
      datatype.progress,
      L3_ProgressOption(initialValue)(_.progress),
      isConst)
  }
}

/// L3_UnfoldLeveledVariableDeclarations

object L3_UnfoldLeveledVariableDeclarations extends DefaultStrategy("Unfold leveled value and variable declarations") {
  this += new Transformation("Unfold", {
    case decl : L3_VariableDeclaration if decl.levels.isDefined => decl.unfold
  })
}
