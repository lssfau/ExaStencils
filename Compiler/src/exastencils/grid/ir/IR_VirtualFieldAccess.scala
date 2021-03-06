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

package exastencils.grid.ir

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.knowledge.ir.IR_LeveledKnowledgeAccess
import exastencils.logger.Logger

/// IR_VirtualFieldAccess

case class IR_VirtualFieldAccess(
    var target : IR_VirtualField,
    var index : IR_ExpressionIndex,
    var fragIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_LeveledKnowledgeAccess with IR_SpecialExpandable with IR_CanBeOffset {

  override def datatype = target.datatype

  var offset : Option[IR_ConstIndex] = None
  override def offsetWith(newOffset : IR_ConstIndex) = index += newOffset

  def tryResolve : IR_Expression = {
    if (!target.resolutionPossible)
      Logger.warn(s"Access to ir virtual field without resolution found: ${ target.name }@${ target.level }")

    target match {
      case scalar : IR_VirtualFieldWithScalar => scalar.resolve(index)

      case vector : IR_VirtualFieldWithVec =>
        IR_MatrixExpression(target.datatype.asInstanceOf[IR_MatrixDatatype],
          vector.listPerDim.map(IR_VirtualFieldAccess(_, Duplicate(index)) : IR_Expression))
    }
  }
}

/// IR_ResolveVirtualFieldAccesses

object IR_ResolveVirtualFieldAccesses extends DefaultStrategy("Resolve accesses to virtual fields") {
  this += new Transformation("Resolve", {
    case access : IR_VirtualFieldAccess => access.tryResolve
  })
}
