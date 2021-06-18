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
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.l4._
import exastencils.datastructures._
import exastencils.field.l4._
import exastencils.logger.Logger
import exastencils.operator.ir.IR_StencilFieldAccess
import exastencils.prettyprinting.PpStream

/// L4_StencilFieldAccess

object L4_StencilFieldAccess {
  def apply(access : L4_FutureStencilFieldAccess) =
    new L4_StencilFieldAccess(L4_StencilFieldCollection.getByIdentifier(access.name, access.level).get, access.slot, access.offset, access.dirAccess, access.matIndex)
}

case class L4_StencilFieldAccess(
    var target : L4_StencilField,
    var slot : L4_SlotSpecification,
    var offset : Option[L4_ConstIndex] = None,
    var dirAccess : Option[L4_ConstIndex] = None,
    var matIndex : Option[L4_MatIndex] = None) extends L4_OperatorAccess with L4_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    out << target.name
    if (target.field.numSlots > 1) out << '[' << slot << ']'
    out << '@' << target.level
    if (offset.isDefined) out << "@" << offset
    if (dirAccess.isDefined) out << ":" << dirAccess
    if (matIndex.isDefined) out << matIndex.get
  }

  def progressOffset(numDims : Int) = {
    if (offset.isDefined) {
      val progressedOffset = offset.get.progress
      while (progressedOffset.indices.length < numDims) progressedOffset.indices :+= 0
      Some(progressedOffset)
    } else {
      None
    }
  }

  override def progress : IR_StencilFieldAccess = ProgressLocation {
    if (dirAccess.isDefined) Logger.warn("Unresolved dirAccess")

    val numDims = target.field.fieldLayout.numDimsGrid
    val index = IR_LoopOverDimensions.defIt(numDims)

    IR_StencilFieldAccess(target.getProgressedObj(), L4_FieldAccess.resolveSlot(target.field.getProgressedObj(), slot), index, progressOffset(index.length))
  }

  override def assembleOffsetMap() = target.stencil.assembleOffsetMap()
}

/// L4_ResolveStencilFieldAccesses

object L4_ResolveStencilFieldAccesses extends DefaultStrategy("Resolve accesses to stencil fields") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L4_FutureStencilFieldAccess if L4_StencilFieldCollection.exists(access.name, access.level) =>
      access.toStencilFieldAccess
  })
}

/// L4_ResolveStencilFieldComponentAccesses

object L4_ResolveStencilFieldComponentAccesses extends DefaultStrategy("Resolve accesses to single components of stencil fields") {
  this += new Transformation("Resolve applicable accesses", {
    case access : L4_StencilFieldAccess if access.matIndex.isDefined =>
      if (access.dirAccess.isDefined)
        Logger.warn(s"Access to stencil field ${ access.target.name } on level ${ access.target.level } has dirAccess and array subscript modifiers; " +
          "array index will be given precedence, dirAccess will be ignored")

      L4_FieldAccess(access.target.field, access.slot, access.offset, false, access.matIndex)

    case access : L4_StencilFieldAccess if access.dirAccess.isDefined =>
      val arrayIdx = access.target.stencil.findStencilEntryIndex(access.dirAccess.get)
      L4_FieldAccess(access.target.field, access.slot, access.offset, false, if(arrayIdx.isDefined) Some(L4_MatIndex(Array(L4_ConstIndex(arrayIdx.get)))) else None)
  })
}

/// L4_UnresolveStencilFieldAccesses

object L4_UnresolveStencilFieldAccesses extends DefaultStrategy("Revert stencil field accesses to unresolved accesses") {
  this += new Transformation("Replace", {
    case L4_StencilFieldAccess(target, slot, offset, dirAccess, matIndex) =>
      val newSlot = if (L4_ActiveSlot == slot) None else Some(slot)
      L4_UnresolvedAccess(target.name, Some(L4_SingleLevel(target.level)), newSlot, offset, dirAccess, matIndex)
  })
}
