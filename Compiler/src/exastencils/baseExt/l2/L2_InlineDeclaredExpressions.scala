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

package exastencils.baseExt.l2

import exastencils.base.l2._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.util.l2._

/// L2_InlineDeclaredExpressions

object L2_InlineDeclaredExpressions extends DefaultStrategy("Inline accesses to declared expressions") {
  var declCollector = new L2_ExpressionDeclarationCollector
  this.register(declCollector)

  val levelCollector = new L2_LevelCollector
  this.register(levelCollector)

  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve", {
    case access : L2_UnresolvedAccess if declCollector.exists(access.name) =>
      // check for level in access and decl
      val decl =
        if (declCollector.existsPlain(access.name)) {
          // access to plain variable
          if (access.level.nonEmpty) Logger.warn(s"Level access to un-leveled expression ${ access.name } will be ignored")

          declCollector.getDeclaration(access.name)
        } else {
          // access to leveled variable
          val lvl = {
            if (access.level.isDefined) access.level.get.resolveLevel
            else if (levelCollector.inLevelScope) levelCollector.getCurrentLevel
            else Logger.error(s"Missing level for calling of ${ access.name }")
          }

          declCollector.getDeclaration(access.name, lvl)
        }

      val wrappedExpr = L2_ExpressionStatement(Duplicate(decl.expr))

      // inherit modifiers
      if (access.offset.isDefined) {
        L2_OffsetAllApplicable.offset = access.offset.get
        L2_OffsetAllApplicable.applyStandalone(wrappedExpr)
      }
      if (access.dirAccess.isDefined)
        Logger.warn(s"Ignoring direction access when inlining expression ${ access.name }")
      if (access.slot.isDefined)
        Logger.warn(s"Ignoring slot access when inlining expression ${ access.name }")

      wrappedExpr.expression
  })

  this += new Transformation("Remove applicable declarations", {
    case g : L2_GlobalSection         => g // skip globals since expression might be needed on lower layers
    case _ : L2_ExpressionDeclaration => None // consume declaration
  }, false)
}
