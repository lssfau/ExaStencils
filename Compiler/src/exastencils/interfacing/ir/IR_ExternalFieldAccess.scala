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

package exastencils.interfacing.ir

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.config._
import exastencils.datastructures._

/// IR_ExternalFieldAccess

case class IR_ExternalFieldAccess(var name : IR_Expression, var field : IR_ExternalField, var index : IR_ExpressionIndex) extends IR_Expression with IR_SpecialExpandable {
  // TODO: var index : IR_Index
  override def datatype = field.fieldLayout.datatype

  val alignedAccessPossible = false

  def linearize : IR_ArrayAccess = {
    if (Knowledge.generateFortranInterface) {
      // Fortran requires multi-index access to multidimensional arrays
      val it = IR_LoopOverDimensions.defIt(field.fieldLayout.numDimsData)
      var ret = name
      for (dim <- field.fieldLayout.numDimsData - 1 to 0 by -1)
        ret = IR_ArrayAccess(ret, it(dim), alignedAccessPossible)
      ret.asInstanceOf[IR_ArrayAccess]
    } else
      IR_ArrayAccess(name, field.fieldLayout.linearizeIndex(index), alignedAccessPossible)
  }
}

/// IR_LinearizeExternalFieldAccess

object IR_LinearizeExternalFieldAccess extends DefaultStrategy("Linearize ExternalFieldAccess nodes") {
  this += new Transformation("Linearize", {
    case access : IR_ExternalFieldAccess => access.linearize
  })
}
