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
import exastencils.baseExt.l1.L1_UnresolvedAccess
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting._
import exastencils.util.l1._

/// L1_VariableAccess

trait L1_VariableAccess extends L1_Access

/// L1_PlainVariableAccess

case class L1_PlainVariableAccess(var name : String, var datatype : L1_Datatype, var isConst : Boolean) extends L1_VariableAccess {
  override def prettyprint(out : PpStream) : Unit = out << name
  override def progress = ProgressLocation(L2_PlainVariableAccess(name, datatype.progress, isConst))
}

/// L1_LeveledVariableAccess

case class L1_LeveledVariableAccess(var name : String, var level : Int, var datatype : L1_Datatype, var isConst : Boolean) extends L1_VariableAccess {
  override def prettyprint(out : PpStream) : Unit = out << name << '@' << level
  override def progress = ProgressLocation(L2_LeveledVariableAccess(name, level, datatype.progress, isConst))
}

/// L1_ResolveVariableAccesses

object L1_ResolveVariableAccesses extends DefaultStrategy("Resolve value and variable accesses") {
  var declCollector = new L1_VariableDeclarationCollector
  this.register(declCollector)

  val levelCollector = new L1_LevelCollector
  this.register(levelCollector)
  
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve", {
    case access : L1_UnresolvedAccess if declCollector.exists(access.name) =>
      // check for level in access and decl
      if (declCollector.existsPlain(access.name)) {
        // access to plain variable
        if (access.level.nonEmpty) Logger.warn(s"Level access to un-leveled variable/ value ${ access.name } will be ignored")

        val decl = declCollector.getDeclaration(access.name)
        if (Knowledge.experimental_l1_inlineValueDeclarations && decl.isConst)
          Duplicate(decl.initialValue.get)
        else
          L1_PlainVariableAccess(decl.name, decl.datatype, decl.isConst)
      } else {
        // access to leveled variable
        val lvl = {
          if (access.level.isDefined) access.level.get.resolveLevel
          else if (levelCollector.inLevelScope) levelCollector.getCurrentLevel
          else Logger.error(s"Missing level for calling of ${ access.name }")
        }

        val decl = declCollector.getDeclaration(access.name, lvl)
        if (Knowledge.experimental_l1_inlineValueDeclarations && decl.isConst)
          Duplicate(decl.initialValue.get)
        else
          L1_LeveledVariableAccess(decl.name, lvl, decl.datatype, decl.isConst)
      }
  })
}
