package exastencils.communication.ir

import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.base.ir._
import exastencils.communication.NeighborInfo
import exastencils.datastructures.Node
import exastencils.fieldlike.ir.IR_FieldLike

/// IR_PackInfo

trait IR_PackInfo extends Node {
  def neighbor : NeighborInfo
  def field : IR_FieldLike

  def neighDir : Array[Int] = neighbor.dir

  def inverseNeighDir : Array[Int] = neighbor.dir.map(-1 * _)

  protected def getPackingIntervalFromStartAndEnd(start : Array[IR_Expression], end : Array[IR_Expression]) =
    IR_ExpressionIndexRange(IR_ExpressionIndex(start), IR_ExpressionIndex(end))

  protected def getPackingIntervalFromStartAndEndTuple(startAndEnd : (Array[IR_Expression], Array[IR_Expression])) =
    IR_ExpressionIndexRange(IR_ExpressionIndex(startAndEnd._1), IR_ExpressionIndex(startAndEnd._2))
  protected def extendWithDataPackInterval(interval : IR_ExpressionIndexRange) : IR_ExpressionIndexRange = {
    // TODO: honor fieldSelection.arrayIndex

    for (dim <- numDimsGrid until numDimsData) {
      interval.begin.indices :+= IR_IntegerConstant(0)
      interval.end.indices :+= resolveIndex("TOT", dim)
    }
    interval
  }

  protected def numDimsGrid : Int = field.layout.numDimsGrid
  protected def numDimsData : Int = field.layout.numDimsData
  protected def resolveIndex(indexId : String, dim : Int) : IR_Expression = field.layout.idxById(indexId, dim)
}

/// IR_RemotePackInfo

trait IR_RemotePackInfo extends IR_PackInfo {
  protected def getGridPackingStartAndEnd(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression])

  protected def getGridPackingInterval(neighDir : Array[Int]) : IR_ExpressionIndexRange = {
    val (s, e) = getGridPackingStartAndEnd(neighDir)
    getPackingIntervalFromStartAndEnd(s, e)
  }

  def getPackInterval() : IR_ExpressionIndexRange = extendWithDataPackInterval(getGridPackingInterval(neighbor.dir))
}

/// IR_LocalPackInfo

trait IR_LocalPackInfo extends IR_PackInfo {

  // dest field
  protected def getGridPackingStartAndEndDest(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression])
  private def getGridPackIntervalDest() : IR_ExpressionIndexRange = getPackingIntervalFromStartAndEndTuple(getGridPackingStartAndEndDest(neighbor.dir))
  def getPackIntervalDest() : IR_ExpressionIndexRange = extendWithDataPackInterval(getGridPackIntervalDest())

  // src field
  protected def getGridPackingStartAndEndSrc(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression])
  private def getGridPackIntervalSrc() : IR_ExpressionIndexRange = getPackingIntervalFromStartAndEndTuple(getGridPackingStartAndEndSrc(neighbor.dir))
  def getPackIntervalSrc() : IR_ExpressionIndexRange = extendWithDataPackInterval(getGridPackIntervalSrc())
}
