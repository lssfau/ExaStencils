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

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_Domain
import exastencils.field.ir.IR_FieldCollection
import exastencils.logger.Logger

/// IR_VF_CellCenterAsVec

object IR_VF_CellCenterAsVec {
  def find(level : Int) = IR_VirtualField.findVirtualField(s"vf_cellCenter", level).asInstanceOf[IR_VF_CellCenterAsVec]
  def access(level : Int, index : IR_ExpressionIndex) = IR_VirtualFieldAccess(find(level), index)
}

case class IR_VF_CellCenterAsVec(
    var level : Int,
    var domain : IR_Domain
) extends IR_VirtualFieldWithVec {

  override def name = "vf_cellCenter"
  override def knownAliases = ListBuffer("vf_cellCenterAsVec", "vf_cellCenAsVec", "vf_cellCen")
  override def localization = IR_AtCellCenter
  override def resolutionPossible = true

  override def createDuplicate() = IR_VF_CellCenterAsVec(level, domain)

  override def listPerDim = (0 until numDims).map(IR_VF_CellCenterPerDim.find(level, _) : IR_VirtualField).to[ListBuffer]

  def associatedField = {
    if (!Knowledge.grid_isAxisAligned)
      IR_FieldCollection.getByIdentifier(name, level).get
    else
      Logger.error("Trying to access associated field for IR_VF_CellCenterAsVec; not found")
  }

  override def generateInitCode() = {
    val stmts = ListBuffer[IR_Statement]()

    if (!Knowledge.grid_isAxisAligned) {
      stmts ++= IR_SetupCellCenter.for_nonAA(level)
    }

    stmts
  }

  override def generateInitCodeDependsOn() = {
    if (!Knowledge.grid_isAxisAligned)
      ListBuffer(IR_VF_NodePositionAsVec.find(level))
    else
      ListBuffer()
  }
}

/// IR_VF_CellCenterPerDim

object IR_VF_CellCenterPerDim {
  def find(level : Int, dim : Int) = IR_VirtualField.findVirtualField(s"vf_cellCenter_$dim", level).asInstanceOf[IR_VF_CellCenterPerDim]
  def access(level : Int, dim : Int, index : IR_ExpressionIndex) = IR_VirtualFieldAccess(find(level, dim), index)
}

case class IR_VF_CellCenterPerDim(
    var level : Int,
    var domain : IR_Domain,
    var dim : Int
) extends IR_VirtualFieldPerDim {

  override def name = s"vf_cellCenter_$dim"
  override def knownAliases = ListBuffer(s"vf_cellCenter_${ IR_Localization.dimToString(dim) }", s"vf_cellCen_$dim", s"vf_cellCen_${ IR_Localization.dimToString(dim) }")
  override def localization = IR_AtCellCenter
  override def resolutionPossible = true

  override def createDuplicate() = IR_VF_CellCenterPerDim(level, domain, dim)

  override def resolve(index : IR_ExpressionIndex) = {
    if (Knowledge.grid_isUniform) {
      // nodePos + 0.5 * cellWidth
      IR_VF_NodePositionPerDim.access(level, dim, index) + 0.5 * IR_VF_CellWidthPerDim.access(level, dim, index)

    } else if (Knowledge.grid_isAxisAligned) {
      // interpolation of two node positions suffices
      0.5 * (IR_VF_NodePositionPerDim.access(level, dim, index)
        + IR_VF_NodePositionPerDim.access(level, dim, IR_GridUtil.offsetIndex(index, 1, dim)))

    } else {
      // interpolation of the current cell's corner positions

      var accesses = ListBuffer[IR_VirtualFieldAccess](IR_VF_NodePositionPerDim.access(level, dim, index))

      for (d <- 0 until numDims)
        accesses = accesses.flatMap(base => {
          val left = Duplicate(base)
          val right = Duplicate(base)
          right.index = IR_GridUtil.offsetIndex(right.index, 1, d)

          ListBuffer(left, right)
        })

      (1.0 / accesses.length) * IR_Addition(accesses.map(_.asInstanceOf[IR_Expression]))
    }
  }
}
