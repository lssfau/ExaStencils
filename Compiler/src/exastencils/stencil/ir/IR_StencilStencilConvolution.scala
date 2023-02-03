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

package exastencils.stencil.ir

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.config._
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.operator.ir._
import exastencils.util.ir.IR_ResultingDatatype

// TODO: is it really necessary to wrap convolutions in separate nodes?
// TODO: update convolutions with new dimensionality logic

/// IR_StencilStencilConvolution

case class IR_StencilStencilConvolution(var left : IR_StencilAccess, var right : IR_StencilAccess) extends IR_Expression with IR_Expandable {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)

  def stencilLeft = left.target
  def stencilRight = right.target

  override def expand() : Output[IR_StencilAccess] = {
    ???
    // FIXME: integrate with the new system
//    var entries : ListBuffer[IR_StencilEntry] = ListBuffer()
//
//    for (re <- stencilRight.entries) {
//      for (le <- stencilLeft.entries) {
//        val rightOffset = Duplicate(re.offset)
//
//        val leftOffset = Duplicate(le.offset)
//        if (stencilRight.level > stencilLeft.level) {
//          for (d <- 0 until Knowledge.dimensionality)
//            leftOffset(d) = (IR_DimToString(d) : IR_Expression) / 2 + leftOffset(d)
//        } else {
//          for (d <- 0 until Knowledge.dimensionality)
//            leftOffset(d) = (IR_DimToString(d) : IR_Expression) + leftOffset(d)
//        }
//
//        val combOff = leftOffset
//        IR_ReplaceIndexOccurrences.replacement = rightOffset
//        IR_ReplaceIndexOccurrences.doUntilDoneStandalone(combOff)
//
//        var combCoeff : IR_Expression = re.coefficient * le.coefficient
//        IR_GeneralSimplify.doUntilDoneStandalone(combOff)
//        IR_GeneralSimplify.doUntilDoneStandalone(combCoeff)
//        val addToEntry = entries.find(e => e.offset match { case o if combOff == o => true; case _ => false })
//        if (addToEntry.isDefined) {
//          combCoeff += addToEntry.get.coefficient
//          IR_GeneralSimplify.doUntilDoneStandalone(combCoeff)
//          addToEntry.get.coefficient = combCoeff
//        } else entries += IR_StencilEntry(combOff, combCoeff)
//      }
//    }
//
//    if (left.offset.isDefined)
//      Logger.warn("Ignoring unsupported offset access in stencil stencil convolution: " + left.offset.get)
//    if (right.offset.isDefined)
//      Logger.warn("Ignoring unsupported offset access in stencil stencil convolution: " + right.offset.get)
//
//    IR_StencilAccess(IR_Stencil(stencilLeft.name + "_" + stencilRight.name, stencilLeft.level, entries), None)
  }
}

/// IR_StencilFieldStencilConvolution

case class IR_StencilFieldStencilConvolution(var left : IR_StencilFieldAccess, var right : IR_StencilAccess) extends IR_Expression with IR_Expandable {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)

  def stencilLeft = left.target
  def stencilRight = right.target

  override def expand() : Output[IR_StencilAccess] = {
    ???
    // FIXME: integrate with the new system
//    var entries : ListBuffer[IR_StencilEntry] = ListBuffer()
//
//    for (re <- stencilRight.entries) {
//      for (e <- stencilLeft.offsets.indices) {
//        val stencilFieldIdx = Duplicate(left.index)
//        stencilFieldIdx(Knowledge.dimensionality) = e
//        for (dim <- 0 until Knowledge.dimensionality)
//          stencilFieldIdx(dim) += re.offset(dim)
//        stencilFieldIdx.indices :+= (e : IR_Expression)
//        val fieldSel = stencilLeft.toFieldSelection
//
//        val rightOffset = Duplicate(re.offset)
//
//        val leftOffset = Duplicate(stencilLeft.offsets(e))
//        if (stencilRight.level > stencilLeft.stencilField.level) {
//          for (d <- 0 until Knowledge.dimensionality)
//            leftOffset(d) = (IR_DimToString(d) : IR_Expression) / 2 + leftOffset(d)
//        } else {
//          for (d <- 0 until Knowledge.dimensionality)
//            leftOffset(d) = (IR_DimToString(d) : IR_Expression) + leftOffset(d)
//        }
//
//        val combOff = leftOffset
//        IR_ReplaceIndexOccurrences.replacement = rightOffset
//        IR_ReplaceIndexOccurrences.doUntilDoneStandalone(combOff)
//
//        var combCoeff : IR_Expression = re.coefficient * IR_FieldLikeAccess(fieldSel, stencilFieldIdx)
//        IR_GeneralSimplify.doUntilDoneStandalone(combOff)
//        IR_GeneralSimplify.doUntilDoneStandalone(combCoeff)
//        val addToEntry = entries.find(e => e.offset match {
//          case o if combOff == o => true
//          case _                 => false
//        })
//        if (addToEntry.isDefined) {
//          combCoeff += addToEntry.get.coefficient
//          IR_GeneralSimplify.doUntilDoneStandalone(combCoeff)
//          addToEntry.get.coefficient = combCoeff
//        } else entries += IR_StencilEntry(combOff, combCoeff)
//      }
//    }
//
//    if (right.offset.isDefined)
//      Logger.warn("Ignoring unsupported offset access in stencil stencil convolution: " + right.offset.get)
//
//    IR_StencilAccess(IR_Stencil(stencilLeft.stencilField.name + "_" + stencilRight.name, stencilLeft.stencilField.level, entries), None)
  }
}

/// IR_ReplaceIndexOccurrences

object IR_ReplaceIndexOccurrences extends DefaultStrategy("Replace index occurrences with something else") {
  var replacement : IR_ExpressionIndex = IR_LoopOverDimensions.defIt(Knowledge.dimensionality) // to be overwritten

  def doUntilDone(node : Option[Node] = None) = {
    do { apply(node) }
    while (results.last._2.matches > 0) // FIXME: cleaner code
  }

  def doUntilDoneStandalone(node : Node) = {
    val oldLvl = Logger.getLevel
    Logger.setLevel(Logger.WARNING)
    do { applyStandalone(node) }
    while (results.last._2.matches > 0) // FIXME: cleaner code
    Logger.setLevel(oldLvl)
  }

  Knowledge.dimensionality match { // TODO: update and extend -> arbitrary dimensionality, VariableAccesses and name of indices
    case 1 => this += new Transformation("SearchAndReplace", {
      case IR_StringLiteral("x") => replacement(0)
    })
    case 2 => this += new Transformation("SearchAndReplace", {
      case IR_StringLiteral("x") => replacement(0)
      case IR_StringLiteral("y") => replacement(1)
    })
    case 3 => this += new Transformation("SearchAndReplace", {
      case IR_StringLiteral("x") => replacement(0)
      case IR_StringLiteral("y") => replacement(1)
      case IR_StringLiteral("z") => replacement(2)
    })
  }
}
