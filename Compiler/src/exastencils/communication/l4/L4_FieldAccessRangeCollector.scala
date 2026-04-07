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

package exastencils.communication.l4

import scala.collection.mutable.HashMap

import exastencils.base.l4._
import exastencils.baseExt.l4._
import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.field.l4._
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.fieldlike.ir.IR_FieldLikeLayout
import exastencils.fieldlike.l4.L4_FieldLike
import exastencils.fieldlike.l4.L4_FieldLikeAccess
import exastencils.grid.l4._
import exastencils.logger.Logger
import exastencils.operator.l4._
import exastencils.solver.l4.L4_LocalSolve

/// L4_FieldAccessRangeCollector

object L4_FieldAccessRangeCollector {

  case class L4_FieldWithSlot(var field : L4_FieldLike[_ <: IR_FieldLike, _ <: IR_FieldLikeLayout], var slot : L4_SlotSpecification) {
    def numDimsGrid = field.numDimsGrid
    def fieldLayout = field.fieldLayout
    def boundary = field.boundary
  }

}

class L4_FieldAccessRangeCollector() extends Collector {

  import L4_FieldAccessRangeCollector.L4_FieldWithSlot

  var beginOffset = Array[Int]()
  var endOffset = Array[Int]()
  var contractionOffsetBegin = Array[Int]()
  var contractionOffsetEnd = Array[Int]()

  var readExtentMin = HashMap[L4_FieldWithSlot, Array[Int]]()
  var readExtentMax = HashMap[L4_FieldWithSlot, Array[Int]]()
  var writeExtentMin = HashMap[L4_FieldWithSlot, Array[Int]]()
  var writeExtentMax = HashMap[L4_FieldWithSlot, Array[Int]]()

  var ignore : Boolean = true

  def adaptNodeBasedFields() = {
    def adapt(map : HashMap[L4_FieldWithSlot, Array[Int]]) = {
      for (field <- map) {
        // deduct offset due to node-based localization
        field._1.fieldLayout.localization match {
          case L4_AtNode                => field._2.transform(_ - 1)
          case L4_AtFaceCenter(faceDim) => field._2(faceDim) -= 1
          case L4_AtCellCenter          =>
          case other                    => Logger.warn(s"Found unknown localization $other")
        }
      }
    }

    adapt(readExtentMax)
    adapt(writeExtentMax)
  }

  def minValuesForExprIndex(index : L4_ExpressionIndex) : Array[Int] = {
    index.indices.map {
      case L4_IntegerConstant(const) => const.toInt
      case _ : L4_Modulo             => 0
      case other                     => Logger.warn(s"Unsupported offset found: $other"); 0
    }
  }

  def minValuesForAnyIndex(index : L4_Index) : Array[Int] = {
    index match {
      case ei : L4_ExpressionIndex => minValuesForExprIndex(ei)
      case ci : L4_ConstIndex      => ci.indices
    }
  }

  def maxValuesForExprIndex(index : L4_ExpressionIndex) : Array[Int] = {
    index.indices.map {
      case L4_IntegerConstant(const)             => const.toInt
      case L4_Modulo(_, L4_IntegerConstant(div)) => (div - 1).toInt
      case other                                 => Logger.warn(s"Unsupported offset found: $other"); 0
    }
  }

  def maxValuesForAnyIndex(index : L4_Index) : Array[Int] = {
    index match {
      case ei : L4_ExpressionIndex => maxValuesForExprIndex(ei)
      case ci : L4_ConstIndex      => ci.indices
    }
  }

  def extractMinOffsetArray(numDims : Int, offset : Option[L4_Index]) : Array[Int] = {
    if (offset.isEmpty)
      Array.fill(numDims)(0)
    else
      minValuesForAnyIndex(offset.get)
  }

  def extractMaxOffsetArray(numDims : Int, offset : Option[L4_Index]) : Array[Int] = {
    if (offset.isEmpty)
      Array.fill(numDims)(0)
    else
      maxValuesForAnyIndex(offset.get)
  }

  def processReadExtent(field : L4_FieldWithSlot, offset : Option[L4_Index], offset2 : Option[L4_Index] = None) = {
    // honor offsets in field accesses if present - otherwise assume zero
    var minOffset = extractMinOffsetArray(field.numDimsGrid, offset)
    var maxOffset = extractMaxOffsetArray(field.numDimsGrid, offset)

    if (offset2.isDefined) {
      minOffset = (minOffset, minValuesForAnyIndex(offset2.get)).zipped.map(_ + _)
      maxOffset = (maxOffset, maxValuesForAnyIndex(offset2.get)).zipped.map(_ + _)
    }

    // take loop extent offsets into account
    minOffset = (minOffset, beginOffset).zipped.map(_ + _)
    maxOffset = (maxOffset, endOffset).zipped.map(_ + _)

    // update read extents
    if (!readExtentMin.contains(field))
      readExtentMin += (field -> minOffset)
    else
      readExtentMin.update(field, (readExtentMin(field), minOffset).zipped.map(math.min))

    if (!readExtentMax.contains(field))
      readExtentMax += (field -> maxOffset)
    else
      readExtentMax.update(field, (readExtentMax(field), maxOffset).zipped.map(math.max))
  }

  def processWriteExtent(field : L4_FieldWithSlot, offset : Option[L4_ConstIndex]) = {
    // honor offsets in field accesses if present - otherwise assume zero
    var minOffset = extractMinOffsetArray(field.numDimsGrid, offset)
    var maxOffset = extractMaxOffsetArray(field.numDimsGrid, offset)

    // take loop extent offsets into account
    minOffset = (minOffset, beginOffset).zipped.map(_ + _)
    maxOffset = (maxOffset, endOffset).zipped.map(_ + _)

    // update write extents
    if (!writeExtentMin.contains(field))
      writeExtentMin += (field -> minOffset)
    else
      writeExtentMin.update(field, (writeExtentMin(field), minOffset).zipped.map(math.min))

    if (!writeExtentMax.contains(field))
      writeExtentMax += (field -> maxOffset)
    else
      writeExtentMax.update(field, (writeExtentMax(field), maxOffset).zipped.map(math.max))
  }

  override def enter(node : Node) : Unit = {
    node match {
      case loop : L4_ContractingLoop =>
        // store offsets when entering contracting loops

        val contraction = loop.contraction
        contractionOffsetBegin = contraction.posExt.indices.map(_ * loop.number)
        contractionOffsetEnd = contraction.negExt.getOrElse(contraction.posExt).indices.map(_ * loop.number)

      case loop : L4_LoopOverField =>
        // store offsets when entering a loop
        // enable collection mode
        ignore = false

        val field = loop.field.asInstanceOf[L4_FieldLikeAccess].target
        val numDims = field.numDimsGrid
        beginOffset = extractMinOffsetArray(numDims, loop.startOffset)
        endOffset = extractMaxOffsetArray(numDims, loop.endOffset).map(-1 * _) // invert due to specification in DSL

        // count cell iterations -> increase end offset for each dimension where node localization is present
        field.fieldLayout.localization match {
          case L4_AtNode                => endOffset = endOffset.map(_ + 1)
          case L4_AtFaceCenter(faceDim) => endOffset(faceDim) += 1
          case L4_AtCellCenter          =>
          case other                    => Logger.warn(s"Found unknown localization $other")
        }

        // account for contracting loops
        if (!contractionOffsetBegin.isEmpty)
          beginOffset = (beginOffset, contractionOffsetBegin).zipped.map(_ + _)
        if (!contractionOffsetEnd.isEmpty)
          endOffset = (endOffset, contractionOffsetEnd).zipped.map(_ + _)

      case L4_Assignment(field : L4_FieldLikeAccess, _, _, cond) =>
        if (ignore) Logger.warn("Found assignment to field outside kernel")

        // store write access for lhs
        processWriteExtent(L4_FieldWithSlot(field.target, field.slot), field.offset)

      // TODO: find a way to ignore recursive match on (lhs) L4_FieldLikeAccess and the wrongfully detected read access

      case L4_OperatorTimesField(op, field) =>
        if (ignore) Logger.warn("Found stencil field convolution outside kernel")

        // process each entry (offset) of the stencil template
        op.assembleOffsetMap().values.foreach(_.foreach(offset =>
          processReadExtent(L4_FieldWithSlot(field.target, field.slot), field.offset, Some(offset))))

      case access : L4_StencilFieldAccess =>
        // process access to stencil coefficients - no slot
        processReadExtent(L4_FieldWithSlot(access.target.field, L4_ActiveSlot), access.offset)

      // TODO: find a way to ignore recursive match on L4_FieldLikeAccess - issues if (0,0,0) entry is not present

      // TODO: other convolutions - or unify convolutions
      // TODO: model StencilFieldAccesses

      case solve : L4_LocalSolve =>
        if (ignore) Logger.warn("Found local solve outside kernel")

        def slot(field : L4_FieldLikeAccess) : L4_SlotSpecification = {
          field.slot match {
            case sth if !solve.jacobiType => sth
            case L4_ActiveSlot            => L4_NextSlot
            case c : L4_ConstantSlot      => L4_ConstantSlot(c.number + 1)
            case other                    => Logger.error(s"Unsupported slot modifier ${ other.prettyprint() }")
          }
        }

        solve.unknowns.map(_.asInstanceOf[L4_FieldLikeAccess]).foreach(field => processWriteExtent(L4_FieldWithSlot(field.target, slot(field)), field.offset))

      // accesses in equations are handled recursively

      case field : L4_FieldLikeAccess =>
        if (!ignore)
          processReadExtent(L4_FieldWithSlot(field.target, field.slot), field.offset)

      case _ =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case loop : L4_ContractingLoop =>
        contractionOffsetBegin = Array()
        contractionOffsetEnd = Array()

      case loop : L4_LoopOverField =>
        // ignore all field accesses outside kernels (eg in advance statements)
        ignore = true
        beginOffset = Array[Int]()
        endOffset = Array[Int]()

      case _ =>
    }
  }

  override def reset() : Unit = {
    readExtentMin.clear
    readExtentMax.clear
    writeExtentMin.clear
    writeExtentMax.clear
  }
}
