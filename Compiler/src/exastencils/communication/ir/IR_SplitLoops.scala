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

import scala.collection.mutable._

import exastencils.base.ir.IR_ExpressionIndex
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.datastructures._
import exastencils.fieldlike.ir.IR_DirectFieldLikeAccess
import exastencils.fieldlike.ir.IR_FieldLikeAccess

/// TODO: IR_SplitLoopsForCommunication

/// IR_GatherFieldAccessOffsets

object IR_GatherFieldAccessOffsets extends QuietDefaultStrategy("Gather field access offsets honoring reference offsets") {
  var accesses = HashMap[String, ListBuffer[IR_ExpressionIndex]]()

  def addAccess(key : String, index : IR_ExpressionIndex) = {
    if (!accesses.contains(key)) accesses.put(key, ListBuffer())
    accesses(key) += index
  }

  this += new Transformation("Gather", {
    case fa : IR_FieldLikeAccess        =>
      addAccess(fa.field.codeName, fa.index - IR_LoopOverDimensions.defIt(fa.index.length))
      fa
    case dfa : IR_DirectFieldLikeAccess =>
      addAccess(dfa.field.codeName, dfa.index - dfa.field.referenceOffset - IR_LoopOverDimensions.defIt(dfa.index.length))
      dfa
  })
}
