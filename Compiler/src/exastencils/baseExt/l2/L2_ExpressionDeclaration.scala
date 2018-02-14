package exastencils.baseExt.l2

import exastencils.base.l2._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L2_ExpressionDeclaration

case class L2_ExpressionDeclaration(
    var name : String,
    var levels : Option[L2_DeclarationLevelSpecification],
    var expr : L2_Expression) extends L2_Statement {

  override def prettyprint(out : PpStream) = {
    out << "Expr " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " = " << expr
  }

  def unfold = {
    if (levels.isEmpty) Logger.warn(s"Unfolding un-leveled expression declaration for $name")

    val levelList = L2_LevelSpecification.extractLevelListDefAll(levels)
    levelList.map(level => {
      val newDecl = Duplicate(this)
      newDecl.levels = Some(L2_SingleLevel(level))
      newDecl
    })
  }

  override def progress = Logger.error("Trying to progress L2_ExpressionDeclaration; unsupported")
}

/// L2_UnfoldLeveledExpressionDeclarations

object L2_UnfoldLeveledExpressionDeclarations extends DefaultStrategy("Unfold leveled expression declarations") {
  this += new Transformation("Unfold", {
    case decl : L2_ExpressionDeclaration if decl.levels.isDefined => decl.unfold
  })
}
