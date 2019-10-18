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

package exastencils.operator.l1

import scala.collection.mutable.ListBuffer

import exastencils.base.l1.L1_Expression
import exastencils.base.l1.L1_ImplicitConversion._
import exastencils.core._
import exastencils.logger.Logger
import exastencils.optimization.l1.L1_GeneralSimplify

/// L1_StencilOps

object L1_StencilOps {
  def add(left : L1_Stencil, right : L1_Stencil) : L1_Stencil = {
    if (left.numDims != right.numDims) Logger.warn("Non-matching dimensionalities")
    if (left.level != right.level) Logger.warn("Non-matching levels")

    val newStencil = left.createDuplicate()
    newStencil.name += "_add_" + right.name
    newStencil.entries ++= Duplicate(right.entries)
    newStencil.squash()

    newStencil
  }

  def mul(left : L1_Stencil, right : L1_Stencil) : L1_Stencil = {
    if (left.numDims != right.numDims) Logger.warn("Non-matching dimensionalities")
    if (left.level != right.level) Logger.warn("Non-matching levels")

    Logger.warn(s"Mul: ${ left.name } * ${ right.name }")

    val numDims = left.numDims

    val newStencil = L1_Stencil(left.name + "_mul_" + right.name, left.level, numDims, ListBuffer())

    for (left <- left.entries; right <- right.entries) {
      val offset = left.offset + right.offset
      newStencil.entries += L1_StencilEntry(offset, Duplicate(left.coefficient) * Duplicate(right.coefficient))
    }

    newStencil.entries.foreach(L1_GeneralSimplify.doUntilDoneStandalone(_))

    newStencil.squash()
    newStencil.filter()
    newStencil
  }

  def scale(stencil : L1_Stencil, factor : L1_Expression) : L1_Stencil = {
    val newStencil = stencil.createDuplicate()
    newStencil.name += "_scaled"
    newStencil.entries.foreach(_.coefficient *= factor)
    newStencil
  }

  def inverse(stencil : L1_Stencil) : L1_Stencil = {
    val newStencil = stencil.createDuplicate()
    newStencil.name += "_inverted"
    newStencil.squash()

    if (stencil.entries.length > 1 || stencil.entries.exists(_.offset.indices.exists(_ != 0)))
      Logger.error(s"Inverse is only possible for diagonal stencils; trying to invert ${ stencil.printStencilToStr() }")

    newStencil.entries.foreach(e => e.coefficient = 1.0 / e.coefficient)

    newStencil
  }
}
