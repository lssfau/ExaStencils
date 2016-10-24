package exastencils.stencil.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures._
import exastencils.deprecated.ir.IR_DimToString
import exastencils.field.ir.IR_FieldAccess
import exastencils.logger.Logger
import exastencils.operator.ir._
import exastencils.optimization.ir.IR_GeneralSimplify
import exastencils.util.ir.IR_ResultingDatatype

// TODO: is it really necessary to wrap convolutions in separate nodes?
// TODO: update convolutions with new dimensionality logic

/// IR_StencilStencilConvolution

case class IR_StencilStencilConvolution(var stencilLeft : IR_Stencil, var stencilRight : IR_Stencil) extends IR_Expression with IR_Expandable {
  override def datatype = IR_ResultingDatatype(stencilLeft.datatype, stencilRight.datatype)

  override def expand() : Output[IR_StencilAccess] = {
    var entries : ListBuffer[IR_StencilEntry] = ListBuffer()

    for (re <- stencilRight.entries) {
      for (le <- stencilLeft.entries) {
        val rightOffset = Duplicate(re.offset)

        val leftOffset = Duplicate(le.offset)
        if (stencilRight.level > stencilLeft.level) {
          for (d <- 0 until Knowledge.dimensionality)
            leftOffset(d) = (IR_DimToString(d) : IR_Expression) / 2 + leftOffset(d)
        } else {
          for (d <- 0 until Knowledge.dimensionality)
            leftOffset(d) = (IR_DimToString(d) : IR_Expression) + leftOffset(d)
        }

        val combOff = leftOffset
        IR_ReplaceIndexOccurrences.replacement = rightOffset
        IR_ReplaceIndexOccurrences.doUntilDoneStandalone(combOff)

        var combCoeff : IR_Expression = re.coefficient * le.coefficient
        IR_GeneralSimplify.doUntilDoneStandalone(combOff)
        IR_GeneralSimplify.doUntilDoneStandalone(combCoeff)
        val addToEntry = entries.find(e => e.offset match { case o if combOff == o => true; case _ => false })
        if (addToEntry.isDefined) {
          combCoeff += addToEntry.get.coefficient
          IR_GeneralSimplify.doUntilDoneStandalone(combCoeff)
          addToEntry.get.coefficient = combCoeff
        } else entries += IR_StencilEntry(combOff, combCoeff)
      }
    }

    IR_StencilAccess(IR_Stencil(stencilLeft.name + "_" + stencilRight.name, stencilLeft.level, entries))
  }
}

/// IR_StencilFieldStencilConvolution

case class IR_StencilFieldStencilConvolution(var stencilLeft : IR_StencilFieldAccess, var stencilRight : IR_Stencil) extends IR_Expression with IR_Expandable {
  override def datatype = IR_ResultingDatatype(stencilLeft.datatype, stencilRight.datatype)

  override def expand() : Output[IR_StencilAccess] = {
    var entries : ListBuffer[IR_StencilEntry] = ListBuffer()

    for (re <- stencilRight.entries) {
      for (e <- stencilLeft.stencilFieldSelection.offsets.indices) {
        val stencilFieldIdx = Duplicate(stencilLeft.index)
        stencilFieldIdx(Knowledge.dimensionality) = e
        for (dim <- 0 until Knowledge.dimensionality)
          stencilFieldIdx(dim) += re.offset(dim)
        val fieldSel = stencilLeft.stencilFieldSelection.toFieldSelection
        fieldSel.arrayIndex = Some(e)

        val rightOffset = Duplicate(re.offset)

        val leftOffset = Duplicate(stencilLeft.stencilFieldSelection.offsets(e))
        if (stencilRight.level > stencilLeft.stencilFieldSelection.stencilField.level) {
          for (d <- 0 until Knowledge.dimensionality)
            leftOffset(d) = (IR_DimToString(d) : IR_Expression) / 2 + leftOffset(d)
        } else {
          for (d <- 0 until Knowledge.dimensionality)
            leftOffset(d) = (IR_DimToString(d) : IR_Expression) + leftOffset(d)
        }

        val combOff = leftOffset
        IR_ReplaceIndexOccurrences.replacement = rightOffset
        IR_ReplaceIndexOccurrences.doUntilDoneStandalone(combOff)

        var combCoeff : IR_Expression = re.coefficient * IR_FieldAccess(fieldSel, stencilFieldIdx)
        IR_GeneralSimplify.doUntilDoneStandalone(combOff)
        IR_GeneralSimplify.doUntilDoneStandalone(combCoeff)
        val addToEntry = entries.find(e => e.offset match {
          case o if combOff == o => true
          case _                 => false
        })
        if (addToEntry.isDefined) {
          combCoeff += addToEntry.get.coefficient
          IR_GeneralSimplify.doUntilDoneStandalone(combCoeff)
          addToEntry.get.coefficient = combCoeff
        } else entries += IR_StencilEntry(combOff, combCoeff)
      }
    }

    IR_StencilAccess(IR_Stencil(stencilLeft.stencilFieldSelection.stencilField.name + "_" + stencilRight.name, stencilLeft.stencilFieldSelection.stencilField.level, entries))
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
