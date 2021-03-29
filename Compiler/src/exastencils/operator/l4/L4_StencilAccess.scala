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

package exastencils.operator.l4

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.operator.ir.IR_StencilAccess
import exastencils.prettyprinting.PpStream

/// L4_StencilAccess

object L4_StencilAccess {
  def apply(access : L4_FutureStencilAccess) =
    new L4_StencilAccess(L4_StencilCollection.getByIdentifier(access.name, access.level).get, access.offset, access.dirAccess, access.arrayIndex)
}

case class L4_StencilAccess(
    var target : L4_Stencil,
    var offset : Option[L4_ConstIndex] = None,
    var dirAccess : Option[L4_ConstIndex] = None,
    var arrayIndex : Option[Int] = None) extends L4_OperatorAccess with L4_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    out << target.name << '@' << target.level
    if (offset.isDefined) out << "@" << offset.get
    if (dirAccess.isDefined) out << ":" << dirAccess.get
  }

  override def progress : IR_StencilAccess = ProgressLocation {
    if (arrayIndex.isDefined) Logger.warn("Unresolved arrayIndex")
    if (dirAccess.isDefined) Logger.warn("Unresolved dirAccess")

    IR_StencilAccess(target.getProgressedObj(), L4_ProgressOption(offset)(_.progress))
  }

  override def assembleOffsetMap() = target.assembleOffsetMap()
}

/// L4_ResolveStencilComponentAccesses

object L4_ResolveStencilComponentAccesses extends DefaultStrategy("Resolve accesses to single components of stencils") {
  this += new Transformation("Resolve applicable accesses", {
    case access : L4_StencilAccess if access.arrayIndex.isDefined =>
      if (access.dirAccess.isDefined)
        Logger.warn(s"Access to stencil ${ access.target.name } on level ${ access.target.level } has dirAccess and array subscript modifiers; " +
          "array index will be given precedence, dirAccess will be ignored")

      val coeff = L4_ExpressionStatement(Duplicate(access.target.entries(access.arrayIndex.get).coefficient))
      if (access.offset.isDefined) {
        L4_OffsetAccesses.offset = access.offset.get
        L4_OffsetAccesses.applyStandalone(coeff)
      }
      coeff.expression

    case access : L4_StencilAccess if access.dirAccess.isDefined =>
      val coeff = L4_ExpressionStatement(Duplicate(access.target.findStencilEntry(access.dirAccess.get).get.coefficient))
      if (access.offset.isDefined) {
        L4_OffsetAccesses.offset = access.offset.get
        L4_OffsetAccesses.applyStandalone(coeff)
      }
      coeff.expression
  })
}

/// L4_ResolveStencilAccesses

object L4_ResolveStencilAccesses extends DefaultStrategy("Resolve accesses to stencils") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L4_FutureStencilAccess if L4_StencilCollection.exists(access.name, access.level) =>
      access.toStencilAccess
  })
}

/// L4_UnresolveStencilAccesses

object L4_UnresolveStencilAccesses extends DefaultStrategy("Revert stencil accesses to unresolved accesses") {
  this += new Transformation("Replace", {
    case L4_StencilAccess(target, offset, dirAccess, arrayIndex) =>
      L4_UnresolvedAccess(target.name, Some(L4_SingleLevel(target.level)), None, offset, dirAccess, arrayIndex, None)
  })
}
