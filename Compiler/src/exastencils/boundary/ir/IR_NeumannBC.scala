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
import exastencils.field.ir._

/// IR_NeumannBC

object IR_NeumannBC {
  def apply() = new IR_NeumannBC(Knowledge.discr_defaultNeumannOrder)
}

case class IR_NeumannBC(var order : Int) extends IR_BoundaryCondition {

  override def generateFieldUpdatesNode(field : IR_FieldLike, slot : IR_Expression, fragIdx : IR_Expression, neigh : NeighborInfo) = {
    // FIXME: this works for now, but users may want to specify bc's per vector element
    // FIXME: (update) adapt for numDimsGrid once new vector and matrix data types are fully integrated
    val numDims = field.numDimsGrid
    val index = IR_LoopOverDimensions.defIt(numDims)

    def offsetIndex = IR_ExpressionIndex(neigh.dir ++ Array.fill(numDims - field.layout.numDimsGrid)(0))
    def offsetIndexWithTrafo(f : (Int => Int)) = IR_ExpressionIndex(neigh.dir.map(f) ++ Array.fill(numDims - field.layout.numDimsGrid)(0))

    ListBuffer(order match {
      case 1 =>
        IR_Assignment(accessField(field, Duplicate(slot), Duplicate(fragIdx), index),
          accessField(field, Duplicate(slot), Duplicate(fragIdx), index + offsetIndexWithTrafo(i => -i)))

      case 2 =>
        IR_Assignment(accessField(field, Duplicate(slot), Duplicate(fragIdx), index),
          ((4.0 / 3.0) * accessField(field, Duplicate(slot), Duplicate(fragIdx), index + offsetIndexWithTrafo(i => -i)))
            + ((-1.0 / 3.0) * accessField(field, Duplicate(slot), Duplicate(fragIdx), index + offsetIndexWithTrafo(i => -2 * i))))

      case 3 => // TODO: do we want this? what do we do on the coarser levels?
        IR_Assignment(accessField(field, Duplicate(slot), Duplicate(fragIdx), index),
          ((3.0 * 6.0 / 11.0) * accessField(field, Duplicate(slot), Duplicate(fragIdx), index + offsetIndexWithTrafo(i => -1 * i)))
            + ((-3.0 / 2.0 * 6.0 / 11.0) * accessField(field, Duplicate(slot), Duplicate(fragIdx), index + offsetIndexWithTrafo(i => -2 * i)))
            + ((1.0 / 3.0 * 6.0 / 11.0) * accessField(field, Duplicate(slot), Duplicate(fragIdx), index + offsetIndexWithTrafo(i => -3 * i))))
    })
  }

  override def generateFieldUpdatesCell(field : IR_FieldLike, slot : IR_Expression, fragIdx : IR_Expression, neigh : NeighborInfo) = {
    val numDims = field.numDimsGrid

    val index = IR_LoopOverDimensions.defIt(numDims)
    def offsetIndex = IR_ExpressionIndex(neigh.dir ++ Array.fill(numDims - field.layout.numDimsGrid)(0))
    def offsetIndexWithTrafo(f : (Int => Int)) = IR_ExpressionIndex(neigh.dir.map(f) ++ Array.fill(numDims - field.layout.numDimsGrid)(0))

    ListBuffer(order match {
      case 1 =>
        IR_Assignment(accessField(field, Duplicate(slot), Duplicate(fragIdx), index + offsetIndex),
          accessField(field, Duplicate(slot), Duplicate(fragIdx), index))
    })
  }
}
