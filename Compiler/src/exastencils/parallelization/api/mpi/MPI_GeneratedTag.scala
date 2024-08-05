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

package exastencils.parallelization.api.mpi

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation.Output
import exastencils.logger.Logger

/// MPI_GeneratedTag

case class MPI_GeneratedTag(var from : IR_Expression, var to : IR_Expression, var dirOfSend : IR_Expression, var concurrencyId : Int, var indexOfRefinedNeighbor : Option[IR_Expression]) extends IR_Expression with IR_Expandable {
  override def datatype = IR_UnitDatatype

  // number of bits needed to encode communication direction
  // if only axis neighbors -> 3 bits needed to represent 6 (3D) possible values, otherwise 5 bits for 32 possibilities
  private val commDirEncodeSize = if (Knowledge.comm_onlyAxisNeighbors) 3 else 5

  // number of bits needed to encode communication IDs for source or destination
  private val commIdEncodeSize = if (Knowledge.mpi_generateCompactTags) 5 else 10

  // number of bits needed to encode refinement index of neighbor
  // 2 bits needed to encode maximum four neighbors when using 2:1 ratio (3D)
  private val commRefinedNeigborIndexSize = if (Knowledge.refinement_maxFineNeighborsPerDim == 2) 2 else Logger.error("Only 2:1 refinement ration supported.")

  private val toShift = commDirEncodeSize
  private val fromShift = toShift + commIdEncodeSize
  private val indexOfRefinedNeighborShift = fromShift + commIdEncodeSize
  private val concurrencyIdShift = indexOfRefinedNeighborShift + (if (indexOfRefinedNeighbor.isDefined) commRefinedNeigborIndexSize else 0)

  def expand() : Output[IR_Expression] = {
    // ("((unsigned int)" ~ from ~ " << 20)") + ("((unsigned int)(" ~ to ~ ") << 10)") + concurrencyId
    //CastExpression(SpecialDatatype("unsigned int"), from << IntegerConstant(20)) + CastExpression(SpecialDatatype("unsigned int"), to << IntegerConstant(10)) + concurrencyId
    if (indexOfRefinedNeighbor.isDefined)
      (IR_Cast(IR_SpecialDatatype("unsigned int"), concurrencyId << concurrencyIdShift)
        + IR_Cast(IR_SpecialDatatype("unsigned int"), indexOfRefinedNeighbor.get << indexOfRefinedNeighborShift)
        + IR_Cast(IR_SpecialDatatype("unsigned int"), from << fromShift)
        + IR_Cast(IR_SpecialDatatype("unsigned int"), to << toShift)
        + dirOfSend)
    else
      (IR_Cast(IR_SpecialDatatype("unsigned int"), concurrencyId << concurrencyIdShift)
        + IR_Cast(IR_SpecialDatatype("unsigned int"), from << fromShift)
        + IR_Cast(IR_SpecialDatatype("unsigned int"), to << toShift)
        + dirOfSend)
  }
}
