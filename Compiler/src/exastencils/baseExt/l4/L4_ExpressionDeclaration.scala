package exastencils.baseExt.l4

import exastencils.base.l4._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_ExpressionDeclaration

case class L4_ExpressionDeclaration(
    var name : String,
    var levels : Option[L4_DeclarationLevelSpecification],
    var expr : L4_Expression) extends L4_Statement {

  override def prettyprint(out : PpStream) = {
    out << "Expr " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " = " << expr
  }

  def unfold = {
    if (levels.isEmpty) Logger.warn(s"Unfolding un-leveled expression declaration for $name")

    val levelList = L4_LevelSpecification.extractLevelListDefAll(levels)
    levelList.map(level => {
      val newDecl = Duplicate(this)
      newDecl.levels = Some(L4_SingleLevel(level))
      newDecl
    })
  }

  override def progress = Logger.error("Trying to progress L4_ExpressionDeclaration; unsupported")
}

/// L4_UnfoldLeveledExpressionDeclarations

object L4_UnfoldLeveledExpressionDeclarations extends DefaultStrategy("Unfold leveled expression declarations") {
  this += new Transformation("Unfold", {
    case decl : L4_ExpressionDeclaration if decl.levels.isDefined => decl.unfold
  })
}
