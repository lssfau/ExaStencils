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

package exastencils.grid.l4

import exastencils.base.ProgressLocation
import exastencils.base.l4.L4_ImplicitConversion._
import exastencils.base.l4._
import exastencils.baseExt.l4._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.grid.ir._
import exastencils.knowledge.l4.L4_LeveledKnowledgeAccess
import exastencils.logger.Logger
import exastencils.optimization.l4.L4_GeneralSimplifyWrapper
import exastencils.prettyprinting.PpStream

/// L4_VirtualFieldAccess

object L4_VirtualFieldAccess {
  def apply(access : L4_FutureVirtualFieldAccess) = {
    val target = L4_VirtualFieldCollection.getByIdentifier(access.name, access.level).get
    val offset = access.offset

    var index = L4_FieldIteratorAccess.fullIndex(target.numDims)
    if (offset.isDefined) index += offset.get

    new L4_VirtualFieldAccess(target, index)
  }
}

case class L4_VirtualFieldAccess(
    var target : L4_VirtualField,
    var index : L4_ExpressionIndex,
    var arrayIndex : Option[Int] = None) extends L4_LeveledKnowledgeAccess with L4_CanBeOffset with L4_MayBlockResolution {

  allDone = !(target.resolutionPossible && Knowledge.experimental_l4_resolveVirtualFields)

  def prettyprint(out : PpStream) = {
    out << target.name << '@' << target.level << '@' << extractOffset
  }

  var offset : Option[L4_ConstIndex] = None
  override def offsetWith(newOffset : L4_ConstIndex) = index += newOffset

  def extractOffset = {
    var offset = Duplicate(index) - L4_FieldIteratorAccess.fullIndex(target.numDims)
    offset = L4_GeneralSimplifyWrapper.process(offset)
    offset.toConstIndex
  }

  def tryResolve : L4_Expression = {
    if (!target.resolutionPossible)
      return this // do nothing

    target match {
      case scalar : L4_VirtualFieldWithScalar => scalar.resolve(index)

      case vector : L4_VirtualFieldWithVec =>
        L4_VectorExpression(Some(target.datatype.asInstanceOf[L4_VectorDatatype].datatype),
          vector.listPerDim.map(L4_VirtualFieldAccess(_, Duplicate(index))).toList)
    }
  }

  override def progress : IR_VirtualFieldAccess = ProgressLocation {
    if (target.datatype.dimensionality > 0) {
      index.indices ++= Array.fill(target.datatype.dimensionality)(0 : L4_Expression)
      if (arrayIndex.isDefined)
        index(target.numDims) = arrayIndex.get
    } else if (arrayIndex.isDefined)
      Logger.warn(s"Meaningless array index in access to scalar field ${ target.name }")

    IR_VirtualFieldAccess(target.getProgressedObj(), index.progress)
  }
}

/// L4_ResolveVirtualFieldAccesses

object L4_ResolveVirtualFieldAccesses extends DefaultStrategy("Resolve accesses to virtual fields") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L4_FutureVirtualFieldAccess if L4_VirtualFieldCollection.exists(access.name, access.level) =>
      access.toVirtualFieldAccess

    // attempt further resolution if requested
    case access : L4_VirtualFieldAccess if !access.allDone =>
      access.tryResolve
  })
}
