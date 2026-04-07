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

package exastencils.baseExt.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.parallelization.ir.IR_ParallelizationInfo
import exastencils.prettyprinting.PpStream

/// L4_LoopOverFragments

object L4_LoopOverFragments {
  def apply(body : L4_Statement*) = new L4_LoopOverFragments(body.to[ListBuffer], None)

  def apply(statements : List[L4_Statement], reduction : Option[L4_Reduction]) =
    new L4_LoopOverFragments(statements.to[ListBuffer], reduction)
}

case class L4_LoopOverFragments(
    var body : ListBuffer[L4_Statement],
    var reduction : Option[L4_Reduction]) extends L4_LoopOverProcessLocalBlocks {

  override def prettyprint(out : PpStream) = {
    out << "loop over fragments "
    if (reduction.isDefined) out << "with " << reduction.get
    out << "{\n" <<< (body, "\n") << "\n}"
  }

  override def progress = ProgressLocation {
    // TODO: introduce L4_ParallelizationInfo
    val parallelization = IR_ParallelizationInfo()
    // assume parallelizabilty by default
    parallelization.potentiallyParallel = true
    parallelization.reduction = reduction.map(_.progress)

    new IR_LoopOverFragments(body.map(_.progress), parallelization)
  }
}
