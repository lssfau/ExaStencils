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
import exastencils.baseExt.ir.IR_Linearization
import exastencils.config._
import exastencils.prettyprinting._

/// CUDA_SharedArray

case class CUDA_SharedArray(name : String, arrayType : IR_Datatype, var size : Array[Long]) extends CUDA_HostStatement {
  size = if (Knowledge.cuda_linearizeSharedMemoryAccess) Array(size.product) else size

  override def prettyprint(out : PpStream) : Unit = {
    out << "__shared__ " << arrayType << " " << name
    size.foreach(s => out << "[" << s << "]")
    out << ";"
  }
}

/// CUDA_UnsizedExternSharedArray

case class CUDA_UnsizedExternSharedArray(name : String, arrayType : IR_ScalarDatatype) extends CUDA_HostStatement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "extern __shared__ " << arrayType << " " << name << "[];"
  }
}

/// CUDA_SharedArrayAccess

case class CUDA_SharedArrayAccess(base : IR_Expression, indices : ListBuffer[IR_Expression], strides : IR_ExpressionIndex) extends IR_Access {
  def this(base : IR_Expression, indices : Array[IR_Expression], strides : IR_ExpressionIndex) = this(base, indices.to[ListBuffer], strides)

  override def datatype = base.datatype
  override def prettyprint(out : PpStream) : Unit = {
    out << base
    if (Knowledge.cuda_linearizeSharedMemoryAccess) {
      out << "[" << linearizeAccess() << "]"
    } else {
      indices.foreach(i => out << "[" << i << "]")
    }
  }

  def linearizeAccess() : IR_Expression = {
    IR_Linearization.linearizeIndex(IR_ExpressionIndex(indices.toArray.reverse), strides : IR_ExpressionIndex)
  }
}
