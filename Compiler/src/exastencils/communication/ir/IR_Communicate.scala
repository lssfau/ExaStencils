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
import exastencils.baseExt.ir.IR_FieldIteratorAccess
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.field.ir.IR_Field

/// IR_Communicate

case class IR_Communicate(
    // FIXME: incorporate component accesses
    var field : IR_Field,
    var slot : IR_Expression,
    var op : String,
    var targets : ListBuffer[IR_CommunicateTarget],
    var condition : Option[IR_Expression],
    var direction : String) extends IR_Statement with IR_SpecialExpandable {

  // shift all index accesses in condition as later functions will generate direct field accesses and according loop bounds
  // TODO: extract to separate transformation
  if (condition.isDefined) ShiftIndexAccesses.applyStandalone(IR_ExpressionStatement(condition.get))

  // TODO: extract strategy - field package?
  object ShiftIndexAccesses extends QuietDefaultStrategy("Shifting index accesses") {
    this += new Transformation("SearchAndReplace", {
      case access : IR_VariableAccess =>
        var ret : IR_Expression = access
        val numDims = field.layout.numDimsData
        for (dim <- 0 until numDims)
          if (IR_FieldIteratorAccess(dim) == access)
            ret = IR_FieldIteratorAccess(dim) - field.referenceOffset(dim)
        ret
    }, false)
  }

}

/// IR_CommunicateTarget

// FIXME: IR_ExpressionIndex -> IR_Index
case class IR_CommunicateTarget(var target : String, var begin : Option[IR_ExpressionIndex], var end : Option[IR_ExpressionIndex]) extends IR_Node {
  if (begin.isDefined && !end.isDefined) // create end if only one 'index' is to be communicated
    end = Some(Duplicate(begin.get) + IR_ConstIndex(Array.fill(begin.get.length)(1)))
}
