package exastencils.communication.l4

import scala.collection.mutable.HashMap

import exastencils.base.l4._
import exastencils.baseExt.l4._
import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.field.l4.{ L4_Field, L4_SlotSpecification, _ }
import exastencils.logger.Logger
import exastencils.stencil.l4._

/// L4_FieldAccessRangeCollector

object L4_FieldAccessRangeCollector {

  case class L4_FieldWithSlot(var field : L4_Field, var slot : L4_SlotSpecification) {
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
        // deduct offset due to node-based discretizations
        field._1.fieldLayout.discretization.toLowerCase match {
          case "node"   => field._2.transform(_ - 1)
          case "face_x" => field._2(0) -= 1
          case "face_y" => field._2(1) -= 1
          case "face_z" => field._2(2) -= 1
          case "cell"   =>
          case other    => Logger.warn(s"Found unknown discretization $other")
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

  def extractMinOffsetArray(numDims : Int, offset : Option[L4_ExpressionIndex]) : Array[Int] = {
    if (offset.isEmpty)
      Array.fill(numDims)(0)
    else
      minValuesForExprIndex(offset.get)
  }

  def extractMaxOffsetArray(numDims : Int, offset : Option[L4_ExpressionIndex]) : Array[Int] = {
    if (offset.isEmpty)
      Array.fill(numDims)(0)
    else
      maxValuesForExprIndex(offset.get)
  }

  def processReadExtent(field : L4_FieldWithSlot, offset : Option[L4_ExpressionIndex], offset2 : Option[L4_Index] = None) = {
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

  def processWriteExtent(field : L4_FieldWithSlot, offset : Option[L4_ExpressionIndex]) = {
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

        val field = loop.field.asInstanceOf[L4_FieldAccess].target
        val numDims = field.numDimsGrid
        beginOffset = extractMinOffsetArray(numDims, loop.startOffset)
        endOffset = extractMaxOffsetArray(numDims, loop.endOffset).map(-1 * _) // invert due to specification in DSL

        // count cell iterations -> increase end offset for each dimension where node discretization is present
        field.fieldLayout.discretization.toLowerCase match {
          case "node"   => endOffset = endOffset.map(_ + 1)
          case "face_x" => endOffset(0) += 1
          case "face_y" => endOffset(1) += 1
          case "face_z" => endOffset(2) += 1
          case "cell"   =>
          case other    => Logger.warn(s"Encountered unknown localization $other")
        }

        // account for contracting loops
        if (!contractionOffsetBegin.isEmpty)
          beginOffset = (beginOffset, contractionOffsetBegin).zipped.map(_ + _)
        if (!contractionOffsetEnd.isEmpty)
          beginOffset = (endOffset, contractionOffsetEnd).zipped.map(_ + _)

      case L4_Assignment(field : L4_FieldAccess, _, _, cond) =>
        if (ignore) Logger.warn("Found assignment to field outside kernel")

        // store write access for lhs
        processWriteExtent(L4_FieldWithSlot(field.target, field.slot), field.offset)

      // TODO: find a way to ignore recursive match on (lhs) L4_FieldAccess and the wrongfully detected read access

      case L4_StencilConvolution(stencil, field) =>
        if (ignore) Logger.warn("Found stencil convolution outside kernel")

        // process each entry (offset) of the stencil
        for (entry <- stencil.target.entries)
          processReadExtent(L4_FieldWithSlot(field.target, field.slot), field.offset, Some(entry.offset))

      // TODO: find a way to ignore recursive match on L4_FieldAccess - issues if (0,0,0) entry is not present

      case L4_StencilFieldConvolution(op, field) =>
        if (ignore) Logger.warn("Found stencil field convolution outside kernel")

        // process each entry (offset) of the stencil template
        for (offset <- op.target.offsets)
          processReadExtent(L4_FieldWithSlot(field.target, field.slot), field.offset, Some(offset))

        // process access to stencil coefficients - no slot
        processReadExtent(L4_FieldWithSlot(op.target.field, L4_ActiveSlot), op.offset)

      // TODO: find a way to ignore recursive match on L4_FieldAccess - issues if (0,0,0) entry is not present

      // TODO: other convolutions - or unify convolutions
      // TODO: model StencilFieldAccesses

      case field : L4_FieldAccess =>
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
