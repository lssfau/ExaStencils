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

package exastencils.util.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.config.Settings
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir._

/// IR_BuildString

object IR_BuildString {
  private var counter : Int = 0
  def getNewName() : String = {
    counter += 1
    "string_builder_%02d".format(counter)
  }
}

case class IR_BuildString(var stringName : IR_Expression, var toPrint : ListBuffer[IR_Expression]) extends IR_Statement with IR_Expandable {
  override def expand() : Output[StatementList] = {
    // TODO: only add to the containing source file, function or function collection
    if (!Settings.additionalIncludes.contains("sstream"))
      Settings.additionalIncludes += "sstream"

    val streamName = IR_BuildString.getNewName()
    def streamType = IR_SpecialDatatype("std::ostringstream")
    def stream = IR_VariableAccess(streamName, streamType)

    ListBuffer(
      // declare stream
      IR_VariableDeclaration(stream),
      // shove expressions into the stream
      IR_Print(stream, toPrint),
      // evaluate and store resulting string
      IR_Assignment(stringName, IR_MemberFunctionCall(stream, "str")))
  }
}
