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

package exastencils.baseExt.ir

import exastencils.base.ir._

/// IR_FieldIteratorAccess

object IR_FieldIteratorAccess {
  def apply(dim : Int) = {
    val ret = new IR_FieldIteratorAccess()
    ret.dim = dim
    ret
  }

  def fullIndex(numDims : Int) = IR_ExpressionIndex((0 until numDims).map(this (_) : IR_Expression).toArray)
}

class IR_FieldIteratorAccess extends IR_VariableAccess("i0", IR_IntegerDatatype) {
  private var dim_ : Int = 0
  def dim_=(d : Int) = {
    dim_ = d
    name = s"i$dim_"
  }
  def dim = dim_

  override def equals(obj : scala.Any) = {
    obj match {
      case other : IR_FieldIteratorAccess => other.dim == dim
      case other : IR_VariableAccess      => other.name == name
      case _                              => super.equals(obj)
    }
  }
}
