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
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.communication.DefaultNeighbors
import exastencils.communication.NeighborInfo
import exastencils.config.Knowledge
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.ir._
import exastencils.scheduling.NoStrategyWrapper

/// IR_AdaptAllocateDataFunction

object IR_AdaptAllocateDataFunction extends DefaultStrategy("Enable closely spaced allocations") {
  this += Transformation("Add workaround", {
    case func : IR_Function if func.name == IR_AllocateDataFunction.fctName =>
      if (!IR_GlobalCollection.get.externalDependencies.contains("malloc.h"))
        IR_GlobalCollection.get.externalDependencies += "malloc.h"

      val maxFieldSize = IR_FieldCollection.objects.map(field => (0 until field.layout.numDimsData).map(d =>
        field.layout.defTotal(d)).product * field.resolveBaseDatatype.typicalByteSize).max
      val allocSize = if (Knowledge.experimental_compactBufferAllocationSize > 0)
        Knowledge.experimental_compactBufferAllocationSize
      else
        math.ceil(maxFieldSize / 1024.0).toInt * 1024

      func.body.prepend(IR_FunctionCall(IR_ExternalFunctionReference("mallopt"), "M_MMAP_THRESHOLD", allocSize))

      func
  })
}

/// IR_SetupAllocateDataFunctionWrapper

object IR_SetupAllocateDataFunctionWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => IR_GlobalCollection.get += IR_AllocateDataFunction(IR_FieldCollection.objects, DefaultNeighbors.neighbors)
}

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
