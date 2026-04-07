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

import scala.collection.mutable.StringBuilder

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_Linearization
import exastencils.datastructures._
import exastencils.polyhedron.IR_PolyArrayAccessLike

/// IR_TempBufferAccess

case class IR_TempBufferAccess(var buffer : IR_IV_CommBufferLike, var index : IR_ExpressionIndex, var strides : IR_ExpressionIndex) extends IR_Expression with IR_SpecialExpandable with IR_PolyArrayAccessLike {
  override def datatype = buffer.datatype

  override def uniqueID : String = {
    val name = new StringBuilder("buffer")
    name.append('_').append(buffer.direction)
    name.append('_').append(buffer.field.name).append(buffer.field.index).append('_').append(buffer.field.level)
    name.append("_n").append(buffer.neighIdx.prettyprint())
    name.append("_f").append(buffer.fragmentIdx.prettyprint())
    name.toString()
  }

  // use Knowledge.data_alignTmpBufferPointers for alignedAccessPossible if aligned vector operations are possible for tmp buffers
  def linearize = IR_ArrayAccess(buffer, IR_Linearization.linearizeIndex(index, strides), alignedAccessPossible = false)
}

/// IR_LinearizeTempBufferAccess

object IR_LinearizeTempBufferAccess extends DefaultStrategy("Linearize TempBufferAccess nodes") {
  this += new Transformation("Linearize", {
    case access : IR_TempBufferAccess => access.linearize
  })
}
