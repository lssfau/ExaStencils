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

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_VectorDatatype
import exastencils.boundary.l4.L4_NoBC
import exastencils.config.Knowledge
import exastencils.domain.l4._
import exastencils.field.l4._
import exastencils.grid.ir._
import exastencils.logger.Logger

/// L4_VF_NodePositionAsVec

object L4_VF_NodePositionAsVec {
  def find(level : Int) = L4_VirtualField.findVirtualField(s"vf_nodePosition", level).asInstanceOf[L4_VF_NodePositionAsVec]
  def access(level : Int, index : L4_ExpressionIndex) = L4_VirtualFieldAccess(find(level), index)
}

case class L4_VF_NodePositionAsVec(
    var level : Int,
    var domain : L4_Domain
) extends L4_VirtualFieldWithVec {

  override def name = "vf_nodePosition"
  override def knownAliases = ListBuffer("vf_nodePositionAsVec", "vf_nodePosAsVec", "vf_nodePos")
  override def localization = L4_AtNode
  override def resolutionPossible = true

  override def createDuplicate() = L4_VF_NodePositionAsVec(level, domain)

  override def listPerDim = (0 until numDims).map(L4_VF_NodePositionPerDim.find(level, _) : L4_VirtualField).to[ListBuffer]

  override def addAdditionalFieldsToKnowledge() = {
    if (!Knowledge.grid_isAxisAligned) {
      val layout = L4_FieldLayout(
        s"${ name }_layout", level, numDims,
        L4_VectorDatatype(L4_RealDatatype, Knowledge.dimensionality), L4_AtNode,
        L4_ConstIndex(Array.fill(domain.numDims)(2)), communicatesGhosts = true,
        L4_ConstIndex(Array.fill(domain.numDims)(1)), communicatesDuplicated = true,
        L4_ConstIndex((0 until numDims).toArray.map(dim => (1 << level) * Knowledge.domain_fragmentLengthAsVec(dim) - 1)))

      val fieldIndex = L4_FieldDecl.runningIndex
      L4_FieldDecl.runningIndex += 1

      val field = L4_Field(name, level, fieldIndex, domain, layout, 1, L4_NoBC)

      L4_FieldLayoutCollection.add(layout)
      L4_FieldCollection.add(field)
    }
  }

  override def progressImpl() = IR_VF_NodePositionAsVec(level, domain.getProgressedObj())
}

/// L4_VF_NodePositionPerDim

object L4_VF_NodePositionPerDim {
  def find(level : Int, dim : Int) = L4_VirtualField.findVirtualField(s"vf_nodePosition_$dim", level).asInstanceOf[L4_VF_NodePositionPerDim]
  def access(level : Int, dim : Int, index : L4_ExpressionIndex) = L4_VirtualFieldAccess(find(level, dim), index)
}

case class L4_VF_NodePositionPerDim(
    var level : Int,
    var domain : L4_Domain,
    var dim : Int
) extends L4_VirtualFieldPerDim {

  override def name = s"vf_nodePosition_$dim"
  override def knownAliases = ListBuffer(s"vf_nodePosition_${ L4_Localization.dimToString(dim) }", s"vf_nodePos_$dim", s"vf_nodePos_${ L4_Localization.dimToString(dim) }")
  override def localization = L4_AtNode
  override def resolutionPossible = false

  override def addAdditionalFieldsToKnowledge() = {
    if (Knowledge.grid_isAxisAligned && !Knowledge.grid_isUniform) {
      def zeroIndex = L4_ConstIndex(Array.fill(domain.numDims)(0))
      def oneIndex = L4_ConstIndex(Array.fill(domain.numDims)(1))

      val layout = L4_FieldLayout(
        s"${ name }_layout", level, numDims,
        L4_RealDatatype, L4_HACK_OtherLocalization("Edge_Node"),
        L4_GridUtil.offsetIndex(zeroIndex, 2, dim), communicatesGhosts = true,
        L4_GridUtil.offsetIndex(zeroIndex, 1, dim), communicatesDuplicated = true,
        L4_GridUtil.offsetIndex(oneIndex, ((1 << level) * Knowledge.domain_fragmentLengthAsVec(dim) - 1) - 1, dim))

      val fieldIndex = L4_FieldDecl.runningIndex
      L4_FieldDecl.runningIndex += 1

      val field = L4_Field(name, level, fieldIndex, domain, layout, 1, L4_NoBC)

      L4_FieldLayoutCollection.add(layout)
      L4_FieldCollection.add(field)
    }
  }

  override def createDuplicate() = L4_VF_NodePositionPerDim(level, domain, dim)

  override def resolve(index : L4_ExpressionIndex) = Logger.error("Trying to resolve node position; unsupported")

  override def progressImpl() = IR_VF_NodePositionPerDim(level, domain.getProgressedObj(), dim)
}
