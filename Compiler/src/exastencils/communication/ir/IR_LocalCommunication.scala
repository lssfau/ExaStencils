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

package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir._

/// IR_LocalCommunication

abstract class IR_LocalCommunication extends IR_Statement with IR_Expandable {
  def insideFragLoop : Boolean

  def wrapFragLoop(toWrap : IR_Statement, parallel : Boolean) : IR_Statement = {
    if (insideFragLoop) {
      toWrap
    } else {
      val loop = IR_LoopOverFragments(toWrap)
      loop.parallelization.potentiallyParallel = parallel
      loop
    }
  }

  def wrapFragLoop(toWrap : ListBuffer[IR_Statement], parallel : Boolean) : ListBuffer[IR_Statement] = {
    if (insideFragLoop) {
      toWrap
    } else {
      val loop = new IR_LoopOverFragments(toWrap)
      loop.parallelization.potentiallyParallel = parallel
      ListBuffer[IR_Statement](loop)
    }
  }
}
