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

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.field.ir._

/// IR_CopyFromExternalField

case class IR_CopyFromExternalField(var dest : IR_Field, var src : IR_ExternalField) extends IR_FuturePlainFunction {
  override var name = "set" + src.name
  override def prettyprint_decl() : String = prettyprint

  def getFortranCompDT() : IR_Datatype = {
    var dt : IR_Datatype = src.resolveBaseDatatype
    for (dim <- 0 until src.fieldLayout.numDimsData)
      dt = IR_ArrayDatatype_VS(dt, src.fieldLayout.idxById("TOT", dim))
    dt
  }

  override def generateFct() = {
    val externalDT = if (Knowledge.generateFortranInterface)
      getFortranCompDT()
    else
      IR_PointerDatatype(dest.resolveBaseDatatype)

    val loopDim = src.fieldLayout.numDimsData
    def multiIndex = IR_LoopOverDimensions.defIt(loopDim)

    // access field layouts
    val internal = dest.layout
    val external = src.fieldLayout

    // match ghost layer info from internal and external fields
    def numGhostInternalLeft(dim : Integer) = internal.idxById("DLB", dim) - internal.idxById("GLB", dim)
    def numGhostExternalLeft(dim : Integer) = external.idxById("DLB", dim) - external.idxById("GLB", dim)
    def numGhostInternalRight(dim : Integer) = internal.idxById("GRE", dim) - internal.idxById("DRE", dim)
    def numGhostExternalRight(dim : Integer) = external.idxById("GRE", dim) - external.idxById("DRE", dim)
    def idxBegin(dim : Integer) : IR_Expression =
      internal.idxById("DLB", dim) - IR_Minimum(numGhostInternalLeft(dim), numGhostExternalLeft(dim))
    def idxEnd(dim : Integer) : IR_Expression =
      internal.idxById("DRE", dim) + IR_Minimum(numGhostInternalRight(dim), numGhostExternalRight(dim))
    def offsetForExtField = IR_ExpressionIndex((0 until loopDim).map(dim => numGhostExternalLeft(dim) - numGhostInternalLeft(dim) : IR_Expression).toArray)

    // compile loop body
    def destAccess = IR_DirectFieldAccess(dest, "slot", multiIndex)
    def srcAccess = IR_ExternalFieldAccess("src", src, multiIndex + offsetForExtField)
    def loopBody = IR_Assignment(destAccess, srcAccess)

    // compile loop
    val loop = new IR_LoopOverDimensions(loopDim, IR_ExpressionIndexRange(
      IR_ExpressionIndex((0 until loopDim).toArray.map(dim => idxBegin(dim))),
      IR_ExpressionIndex((0 until loopDim).toArray.map(dim => idxEnd(dim)))),
      ListBuffer[IR_Statement](loopBody))
    loop.polyOptLevel =
      if (Knowledge.maxLevel - dest.level < Knowledge.poly_numFinestLevels)
        Knowledge.poly_optLevel_fine
      else
        Knowledge.poly_optLevel_coarse
    loop.parallelization.potentiallyParallel = true

    // compile final function
    val fct = IR_PlainFunction(name, IR_UnitDatatype,
      ListBuffer(IR_FunctionArgument("src", externalDT), IR_FunctionArgument("slot", IR_IntegerDatatype)),
      loop)

    fct.allowInlining = false
    fct
  }
}

