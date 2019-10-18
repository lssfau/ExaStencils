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

package exastencils.grid.l3

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.baseExt.l3._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.grid.l4._
import exastencils.knowledge.l3.L3_LeveledKnowledgeAccess
import exastencils.optimization.l3.L3_GeneralSimplifyWrapper
import exastencils.prettyprinting.PpStream

/// L3_VirtualFieldAccess

object L3_VirtualFieldAccess {
  def apply(access : L3_FutureVirtualFieldAccess) = {
    val target = L3_VirtualFieldCollection.getByIdentifier(access.name, access.level).get
    val offset = access.offset

    var index = L3_FieldIteratorAccess.fullIndex(target.numDims)
    if (offset.isDefined) index += offset.get

    new L3_VirtualFieldAccess(target, index)
  }
}

case class L3_VirtualFieldAccess(
    var target : L3_VirtualField,
    var index : L3_ExpressionIndex) extends L3_LeveledKnowledgeAccess with L3_CanBeOffset with L3_MayBlockResolution {

  allDone = !(target.resolutionPossible && Knowledge.experimental_l3_resolveVirtualFields)

  def prettyprint(out : PpStream) = {
    out << target.name << '@' << target.level << '@' << extractOffset
  }

  var offset : Option[L3_ConstIndex] = None
  override def offsetWith(newOffset : L3_ConstIndex) = index += newOffset

  def extractOffset = {
    var offset = Duplicate(index) - L3_FieldIteratorAccess.fullIndex(target.numDims)
    offset = L3_GeneralSimplifyWrapper.process(offset)
    offset.toConstIndex
  }

  def tryResolve : L3_Expression = {
    if (!target.resolutionPossible)
      return this // do nothing

    target match {
      case scalar : L3_VirtualFieldWithScalar => scalar.resolve(index)
      case vector : L3_VirtualFieldWithVec    => this // FIXME: L3_VectorExpression(vector.listPerDim.zipWithIndex.map((va, dim) => L3_VirtualFieldAccess(va, Duplicate(offset), dim)))
    }
  }

  override def progress = ProgressLocation(L4_VirtualFieldAccess(target.getProgressedObj(), index.progress))
}

/// L3_ResolveVirtualFieldAccesses

object L3_ResolveVirtualFieldAccesses extends DefaultStrategy("Resolve accesses to virtual fields") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L3_FutureVirtualFieldAccess if L3_VirtualFieldCollection.exists(access.name, access.level) =>
      access.toVirtualFieldAccess

    // attempt further resolution if requested
    case access : L3_VirtualFieldAccess if !access.allDone =>
      access.tryResolve
  })
}
