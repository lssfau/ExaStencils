package exastencils.baseExt.l3

import exastencils.base.l3._
import exastencils.baseExt.l4.L4_ExpressionDeclaration
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_ExpressionDeclaration

case class L3_ExpressionDeclaration(
    var name : String,
    var levels : Option[L3_DeclarationLevelSpecification],
    var expr : L3_Expression) extends L3_Statement {

  override def prettyprint(out : PpStream) = {
    out << "Expr " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " = " << expr
  }

  def unfold = {
    if (levels.isEmpty) Logger.warn(s"Unfolding un-leveled expression declaration for $name")

    val levelList = L3_LevelSpecification.extractLevelListDefAll(levels)
    levelList.map(level => {
      val newDecl = Duplicate(this)
      newDecl.levels = Some(L3_SingleLevel(level))
      newDecl
    })
  }

  override def progress = L4_ExpressionDeclaration(name, L3_ProgressOption(levels)(_.progress), expr.progress)
}

/// L3_UnfoldLeveledExpressionDeclarations

object L3_UnfoldLeveledExpressionDeclarations extends DefaultStrategy("Unfold leveled expression declarations") {
  this += new Transformation("Unfold", {
    case decl : L3_ExpressionDeclaration if decl.levels.isDefined => decl.unfold
  })
}
