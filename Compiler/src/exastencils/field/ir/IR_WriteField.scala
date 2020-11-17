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

package exastencils.field.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir._

/// IR_WriteField

case class IR_WriteField(
    var filename : IR_Expression,
    var field : IR_Field,
    var slot : IR_Expression,
    var ioInterface : IR_Expression,
    var includeGhostLayers : Boolean,
    var binaryOutput : Boolean = false,
    var separator : IR_Expression = IR_StringConstant(" "),
    var condition: IR_Expression = true,
    var dataset : IR_Expression = IR_NullExpression) extends IR_FieldIO(filename, field, slot, ioInterface, doWrite = true, onlyVals = true, includeGhostLayers, binaryOutput, separator, condition, dataset) {

  override def expand() : Output[StatementList] = {

    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements += generateFileAccess()

    statements
  }
}
