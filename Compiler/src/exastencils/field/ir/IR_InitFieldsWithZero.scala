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
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.parallelization.ir.IR_ParallelizationInfo

/// IR_InitFieldsWithZero

case class IR_InitFieldsWithZero() extends IR_FuturePlainFunction {
  override var name = "initFieldsWithZero"
  override def prettyprint_decl() : String = prettyprint

  override def generateFct() = {
    val fields = IR_FieldCollection.sortedObjects
    var statements : ListBuffer[IR_Statement] = new ListBuffer

    for (field <- fields) {
      val numDims = field.layout.numDimsData
      val index = IR_LoopOverDimensions.defIt(numDims)

      val loopOverDims = new IR_LoopOverDimensions(numDims, IR_ExpressionIndexRange(
        IR_ExpressionIndex((0 until numDims).toArray.map(dim => field.layout.idxById("GLB", dim))),
        IR_ExpressionIndex((0 until numDims).toArray.map(dim => field.layout.idxById("GRE", dim)))),
        (0 until field.numSlots).to[ListBuffer].map(slot =>
          IR_Assignment(
            IR_DirectFieldAccess(field, slot, Duplicate(index)),
            0.0) : IR_Statement))
      loopOverDims.parallelization.potentiallyParallel = true
      loopOverDims.polyOptLevel = 1

      // parallelize only field dimensions (< Knowledge.dimensionality) and not matrix dimensions (important for the NUMA-aware initialization of matrix-fields)
      loopOverDims.parDims.retain(p => p < Knowledge.dimensionality)

      val wrapped = IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(field.domain.index), loopOverDims),
        IR_ParallelizationInfo(potentiallyParallel = true))

      if ("MSVC" == Platform.targetCompiler /*&& Platform.targetCompilerVersion <= 11*/ ) // fix for https://support.microsoft.com/en-us/kb/315481
        statements += IR_Scope(wrapped)
      else
        statements += wrapped
    }

    IR_PlainFunction(name, IR_UnitDatatype, statements)
  }
}
