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

import scala.collection.mutable._

import exastencils.base.ir._
import exastencils.communication.ir._
import exastencils.datastructures._

/// CUDA_GatherLinearizedBufferAccess

object CUDA_GatherLinearizedBufferAccess extends QuietDefaultStrategy("Gather local buffer access nodes") {
  var bufferAccesses = HashMap[String, IR_IV_CommBufferLike]()

  def clear() = {
    bufferAccesses.clear()
  }

  this += new Transformation("Searching", {
    case access @ IR_ArrayAccess(buffer : IR_IV_CommBufferLike, _, _) =>
      bufferAccesses.put(buffer.resolveName(), buffer)

      access
  }, false)
}

/// CUDA_ReplaceLinearizedBufferAccess

object CUDA_ReplaceLinearizedBufferAccess extends QuietDefaultStrategy("Replace local LinearizedBufferAccess nodes") {
  var bufferAccesses = HashMap[String, IR_IV_CommBufferLike]()

  this += new Transformation("Searching", {
    case access @ IR_ArrayAccess(buffer : IR_IV_CommBufferLike, _, _) =>
      IR_ArrayAccess(IR_VariableAccess(buffer.resolveName(), buffer.field.resolveDeclType), access.index)
  })
}
