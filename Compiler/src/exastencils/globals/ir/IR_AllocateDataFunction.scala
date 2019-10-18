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

package exastencils.globals.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.communication.NeighborInfo
import exastencils.field.ir._

/// IR_AllocateDataFunction

object IR_AllocateDataFunction {
  val fctName = "setupBuffers"
}

// TODO: split to separate functions for (host) fields, communication buffers and device data
case class IR_AllocateDataFunction(
    var fields : ListBuffer[IR_Field],
    var neighbors : ListBuffer[NeighborInfo]) extends IR_FuturePlainFunction {

  override var name = IR_AllocateDataFunction.fctName
  override def prettyprint_decl() : String = prettyprint

  override def generateFct() = {
    val body = ListBuffer[IR_Statement]()

    // add static allocations here

    IR_PlainFunction(name, IR_UnitDatatype, body)
  }
}
