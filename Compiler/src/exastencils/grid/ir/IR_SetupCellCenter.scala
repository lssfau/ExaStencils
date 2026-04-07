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
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.ir._
import exastencils.fieldlike.ir.IR_FieldLikeAccess
import exastencils.logger.Logger

/// IR_SetupCellCenter

object IR_SetupCellCenter {
  def for_nonAA(level : Int) : ListBuffer[IR_Statement] = {
    if (IR_DomainCollection.objects.size > 1) Logger.warn("More than one domain is currently not supported for non-uniform grids; defaulting to global domain")

    val domain = IR_DomainCollection.getByIdentifier("global").get
    val numDims = domain.numDims

    val field = IR_VF_CellCenterAsVec.find(level).associatedField
    val baseIndex = IR_LoopOverDimensions.defIt(numDims)
    val baseAccess = IR_FieldLikeAccess(field, 0, baseIndex)

    val npField = IR_VF_NodePositionAsVec.find(level).associatedField
    var interpolateExps = ListBuffer(IR_FieldLikeAccess(npField, 0, baseIndex))
    var factor = 1.0
    for (dim <- Knowledge.dimensions) {
      interpolateExps = interpolateExps.flatMap(fieldAccess =>
        ListBuffer(Duplicate(fieldAccess), IR_GridUtil.offsetAccess(fieldAccess, 1, dim)))
      factor /= 2.0
    }

    ListBuffer[IR_Statement](
      IR_LoopOverPoints(field, None,
        IR_ExpressionIndex(Array.fill(numDims)(-1)),
        IR_ExpressionIndex(Array.fill(numDims)(-1)),
        IR_ExpressionIndex(Array.fill(numDims)(1)),
        ListBuffer[IR_Statement](IR_Assignment(Duplicate(baseAccess), factor * IR_Addition(interpolateExps.map(e => e : IR_Expression))))))
  }
}
