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

package exastencils.parallelization.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._

/// IR_ParallelizationInfo

/**
  * @param potentiallyParallel        specifies if the loop attached can be executed in parallel
  * @param isInnermost                specifies if the loop attached is the innermost in a loop nest AND it should be optimized as such
  * @param isVectorizable             specifies if the loop attached can safely vectorized, even if it is not parallel
  * @param noVect                     specifies if the vectorization of the loop is skipped
  * @param collapseDepth              specifies how many nested loops can be parallelized
  * @param privateVars                variables for which each thread should have a private storage
  * @param reduction                  identifies a reduction, if some
  */
case class IR_ParallelizationInfo(
    var potentiallyParallel : Boolean = false,
    var isInnermost : Boolean = false,
    var isVectorizable : Boolean = false,
    var noVect : Boolean = false,
    var collapseDepth : Int = 1,
    var privateVars : ListBuffer[IR_VariableAccess] = ListBuffer(),
    var reduction : Option[IR_Reduction] = None,
    var parallelizationReasonable : Boolean = true

) extends IR_Node {}

/// IR_HasParallelizationInfo

trait IR_HasParallelizationInfo {
  var parallelization : IR_ParallelizationInfo
}
