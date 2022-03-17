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

package exastencils.boundary.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.NeighborInfo
import exastencils.config.Knowledge
import exastencils.core._
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.grid.ir._
import exastencils.logger.Logger

/// IR_DirichletBC

case class IR_DirichletBC(var boundaryValue : IR_Expression, var order : Int) extends IR_BoundaryCondition {

  override def generateFieldUpdatesNode(field : IR_FieldLike, slot : IR_Expression, fragIdx : IR_Expression, neigh : NeighborInfo) = {
    val index = IR_LoopOverDimensions.defIt(field.numDimsGrid)
    ListBuffer(IR_Assignment(accessField(field, Duplicate(slot), Duplicate(fragIdx), index), boundaryValue))
  }

  override def generateFieldUpdatesCell(field : IR_FieldLike, slot : IR_Expression, fragIdx : IR_Expression, neigh : NeighborInfo) = {
    val numDims = field.numDimsGrid
    val index = IR_LoopOverDimensions.defIt(numDims)
    def offsetIndex = IR_ExpressionIndex(neigh.dir ++ Array.fill(numDims - field.layout.numDimsGrid)(0))
    def offsetIndexWithTrafo(f : (Int => Int)) = IR_ExpressionIndex(neigh.dir.map(f) ++ Array.fill(numDims - field.layout.numDimsGrid)(0))

    ListBuffer(order match {
      case 1 => IR_Assignment(accessField(field, Duplicate(slot), Duplicate(fragIdx), index + offsetIndex),
        (2.0 * boundaryValue) - accessField(field, Duplicate(slot), Duplicate(fragIdx), index))
      case 2 =>
        // determine weights for interpolation
        var w_0_5, w_1, w_2 : IR_Expression = 0
        if (Knowledge.grid_isUniform) {
          w_0_5 = 8.0 / 3.0
          w_1 = -2
          w_2 = 1.0 / 3.0
        } else {
          // non-linear => use Lagrange polynomials
          val nonZero = neigh.dir.zipWithIndex.filter(_._1 != 0)
          if (nonZero.length != 1) Logger.error("Malformed neighbor index vector " + neigh.dir.mkString(", "))
          val dim = nonZero(0)._2
          def x = IR_VF_CellCenterPerDim.access(field.level, dim, index + offsetIndex)
          def x_0_5 =
            if (neigh.dir(dim) > 0)
              IR_VF_NodePositionPerDim.access(field.level, dim, index + offsetIndex)
            else
              IR_VF_NodePositionPerDim.access(field.level, dim, index)
          def x_1 = IR_VF_CellCenterPerDim.access(field.level, dim, index)
          def x_2 = IR_VF_CellCenterPerDim.access(field.level, dim, index + offsetIndexWithTrafo(i => -i))
          w_0_5 = ((x - x_1) * (x - x_2)) / ((x_0_5 - x_1) * (x_0_5 - x_2))
          w_1 = ((x - x_0_5) * (x - x_2)) / ((x_1 - x_0_5) * (x_1 - x_2))
          w_2 = ((x - x_0_5) * (x - x_1)) / ((x_2 - x_0_5) * (x_2 - x_1))
        }
        IR_Assignment(accessField(field, Duplicate(slot), Duplicate(fragIdx), index + offsetIndex),
          w_0_5 * boundaryValue + w_1 * accessField(field, Duplicate(slot), Duplicate(fragIdx), index)
            + w_2 * accessField(field, Duplicate(slot), Duplicate(fragIdx), index + offsetIndexWithTrafo(i => -i)))
    })
  }
}
