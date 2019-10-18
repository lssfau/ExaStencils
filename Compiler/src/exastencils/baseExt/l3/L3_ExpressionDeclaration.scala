//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.baseExt.l3

import exastencils.base.ProgressLocation
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

  override def progress = ProgressLocation(L4_ExpressionDeclaration(name, L3_ProgressOption(levels)(_.progress), expr.progress))
}

/// L3_UnfoldLeveledExpressionDeclarations

object L3_UnfoldLeveledExpressionDeclarations extends DefaultStrategy("Unfold leveled expression declarations") {
  this += new Transformation("Unfold", {
    case decl : L3_ExpressionDeclaration if decl.levels.isDefined => decl.unfold
  })
}
