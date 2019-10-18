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

package exastencils.base.l1

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L1_VariableDeclaration

case class L1_VariableDeclaration(
    var name : String,
    var levels : Option[L1_DeclarationLevelSpecification],
    var datatype : L1_Datatype,
    var initialValue : Option[L1_Expression],
    var isConst : Boolean) extends L1_Statement {

  override def prettyprint(out : PpStream) = {
    out << (if (isConst) "Val " else "Var ") << name
    if (levels.isDefined) out << '@' << levels.get
    out << " : " << datatype
    if (initialValue.isDefined) out << " = " << initialValue.get
  }

  def unfold = {
    if (levels.isEmpty) Logger.warn(s"Unfolding un-leveled variable/ value declaration for $name")

    val levelList = L1_LevelSpecification.extractLevelListDefAll(levels)
    levelList.map(level => {
      val newDecl = Duplicate(this)
      newDecl.levels = Some(L1_SingleLevel(level))
      newDecl
    })
  }

  override def progress = ProgressLocation {
    L2_VariableDeclaration(
      name,
      L1_ProgressOption(levels)(_.progress),
      datatype.progress,
      L1_ProgressOption(initialValue)(_.progress),
      isConst)
  }
}

/// L1_UnfoldLeveledVariableDeclarations

object L1_UnfoldLeveledVariableDeclarations extends DefaultStrategy("Unfold leveled value and variable declarations") {
  this += new Transformation("Unfold", {
    case decl : L1_VariableDeclaration if decl.levels.isDefined => decl.unfold
  })
}
