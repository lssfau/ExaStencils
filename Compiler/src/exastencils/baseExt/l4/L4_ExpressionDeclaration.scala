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
