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

package exastencils.parallelization.api.cuda

import scala.collection._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_VariableAccess
import exastencils.baseExt.ir.IR_InternalVariable
import exastencils.communication.ir.IR_IV_CommBufferLike
import exastencils.datastructures._
import exastencils.field.ir.IR_SlotAccess

/// CUDA_GatherIVs

object CUDA_GatherIVs extends QuietDefaultStrategy("Gather local InternalVariable nodes") {
  var ivAccesses = mutable.HashMap[String, IR_InternalVariable]()

  this += new Transformation("Searching", {
    case iv : IR_IV_CommBufferLike =>
      // skip due to separate handling
      iv

    case iv : IR_InternalVariable =>
      ivAccesses.put(iv.prettyprint, iv)
      iv
  }, false)
}

/// CUDA_ReplaceIVs

object CUDA_ReplaceIVs extends QuietDefaultStrategy("Replace local InternalVariable nodes") {
  var ivAccesses = mutable.HashMap[String, IR_InternalVariable]()

  this += new Transformation("Searching", {
    case iv : IR_IV_CommBufferLike =>
      // skip due to separate handling
      iv
    case slot : IR_SlotAccess =>
      val ivAccess = ivAccesses.find(_._2 == slot.slot).get
      IR_VariableAccess(ivAccess._1, ivAccess._2.resolveDatatype()) + slot.offset

    case iv : IR_InternalVariable =>
      val ivAccess = ivAccesses.find(_._2 == iv).get // TODO: improve performance
      IR_VariableAccess(ivAccess._1, ivAccess._2.resolveDatatype())
  })
}

