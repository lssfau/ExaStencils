package exastencils.communication.ir

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.communication.NeighborInfo
import exastencils.config.Knowledge
import exastencils.fieldlike.ir.IR_FieldLike

/// IR_RefinementPackInfoGhost

trait IR_RefinementPackInfoGhost extends IR_RefinementPackInfo with IR_PackInfoGhost {

  def splitSpaceForSend : Boolean

  def splitSpaceForRecv : Boolean

  def unsplitPackIntervalSend(neighDir : Array[Int]) : IR_ExpressionIndexRange =
    extendWithDataPackInterval(getPackingIntervalFromStartAndEndTuple(unsplitGridPackingStartAndEndForSend(neighDir)))

  def unsplitPackIntervalRecv(neighDir : Array[Int]) : IR_ExpressionIndexRange =
    extendWithDataPackInterval(getPackingIntervalFromStartAndEndTuple(unsplitGridPackingStartAndEndForRecv(neighDir)))

  protected def unsplitGridPackingStartAndEndForSend(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getGridPackingStartAndEndForSend(allowGhostSync = false, neighDir)

  protected def unsplitGridPackingStartAndEndForRecv(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getGridPackingStartAndEndForRecv(allowGhostSync = false, neighDir)

  protected def getRefinedGridPackingStartAndEndForSend(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) = {
    splitIntervalOnDemand(unsplitGridPackingStartAndEndForSend(neighDir), splitSpaceForSend)
  }

  protected def getRefinedGridPackingStartAndEndForRecv(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) = {
    splitIntervalOnDemand(unsplitGridPackingStartAndEndForRecv(neighDir), splitSpaceForRecv)
  }

  protected def splitIntervalOnDemand(interval : (Array[IR_Expression], Array[IR_Expression]), splitSpace : Boolean) : (Array[IR_Expression], Array[IR_Expression]) = {
    if (splitSpace)
      splitPackingStartAndEndForCoarseFragment(neighDir, indexOfRefinedNeighbor, interval._1, interval._2)
    else
      interval
  }

  protected def splitPackingStartAndEndForCoarseFragment(neighDir : Array[Int], indexOfRefinedNeighbor : IR_Expression,
      start : Array[IR_Expression], end : Array[IR_Expression]) : (Array[IR_Expression], Array[IR_Expression]) = {

    // fragment is coarse:
    // - split coarse iteration space of coarse neighbor into equally spaced sections
    // - splitting is done in non-commAxis direction
    val sectionsPerDim = Knowledge.refinement_maxFineNeighborsPerDim
    val (coarseNeighborStart, coarseNeighborEnd) = (0 until numDimsGrid).map {
      case i if neighDir(i) == 0 =>
        val fullInterval = end(i) - start(i)

        // go over grid dimensions in x -> y -> z order and assign each fine neighbor a section of the coarse iteration space
        val s = start(i) + (indexOfRefinedNeighbor Mod sectionsPerDim) * (fullInterval / sectionsPerDim)
        val e = end(i) - ((indexOfRefinedNeighbor + 1) Mod sectionsPerDim) * (fullInterval / sectionsPerDim)

        (s, e)
      case i                     =>
        (start(i), end(i))
    }.unzip

    (coarseNeighborStart.toArray, coarseNeighborEnd.toArray)
  }
}

/// IR_F2CPackInfoGhost

trait IR_F2CPackInfoGhost extends IR_F2CPackInfo with IR_RefinementPackInfoGhost {

  // pack interval for sending fine fragments to a coarse one can be reused from equal-level comm (since ghost layers from fine fragments are sent as a whole)
  override def splitSpaceForSend : Boolean = false
}

/// IR_C2FPackInfoGhost

trait IR_C2FPackInfoGhost extends IR_C2FPackInfo with IR_RefinementPackInfoGhost {

  // pack interval for sending a coarse fragment to finer ones requires adaptations (a coarse block sends N messages, where N is the number of fine neighbors)
  // -> split iteration interval of coarse fragment
  override def splitSpaceForSend : Boolean = true
}

/* fine-to-coarse */

/// IR_F2CPackInfoGhostRemoteSend
case class IR_F2CPackInfoGhostRemoteSend(
    var neighbor : NeighborInfo,
    var indexOfRefinedNeighbor : IR_Expression,
    var field : IR_FieldLike,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_RemotePackInfoGhost with IR_F2CPackInfoGhost {

  // we receive one message from a coarse neighbor, filling up all ghost layers values
  override def splitSpaceForRecv : Boolean = false

  override protected def getGridPackingStartAndEnd(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForSend(neighDir)
}

/// IR_F2CPackInfoGhostRemoteRecv

case class IR_F2CPackInfoGhostRemoteRecv(
    var neighbor : NeighborInfo,
    var indexOfRefinedNeighbor : IR_Expression,
    var field : IR_FieldLike,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_RemotePackInfoGhost with IR_F2CPackInfoGhost {

  // we receive one message from a coarse neighbor, filling up all ghost layers values
  override def splitSpaceForRecv : Boolean = false

  override protected def getGridPackingStartAndEnd(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForRecv(neighDir)
}

/// IR_F2CPackInfoGhostLocalSend

case class IR_F2CPackInfoGhostLocalSend(
    var neighbor : NeighborInfo,
    var indexOfRefinedNeighbor : IR_Expression,
    var field : IR_FieldLike,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_LocalPackInfoGhost with IR_F2CPackInfoGhost {

  // receiver is coarse -> split up its iteration space
  override def splitSpaceForRecv : Boolean = true

  override protected def getGridPackingStartAndEndDest(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForSend(neighDir)

  override protected def getGridPackingStartAndEndSrc(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForRecv(inverseNeighDir)
}

/// IR_F2CPackInfoGhostLocalRecv

case class IR_F2CPackInfoGhostLocalRecv(
    var neighbor : NeighborInfo,
    var indexOfRefinedNeighbor : IR_Expression,
    var field : IR_FieldLike,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_LocalPackInfoGhost with IR_F2CPackInfoGhost {

  // receiver is coarse -> split up its iteration space
  override def splitSpaceForRecv : Boolean = true

  override protected def getGridPackingStartAndEndDest(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForRecv(neighDir)

  override protected def getGridPackingStartAndEndSrc(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForSend(inverseNeighDir)
}

/* coarse-to-fine */

/// IR_C2FPackInfoGhostRemoteSend
case class IR_C2FPackInfoGhostRemoteSend(
    var neighbor : NeighborInfo,
    var indexOfRefinedNeighbor : IR_Expression,
    var field : IR_FieldLike,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_RemotePackInfoGhost with IR_C2FPackInfoGhost {

  // we receive N messages from N fine neighbors -> split iteration space
  override def splitSpaceForRecv : Boolean = true

  override protected def getGridPackingStartAndEnd(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForSend(neighDir)
}

/// IR_C2FPackInfoGhostRemoteRecv

case class IR_C2FPackInfoGhostRemoteRecv(
    var neighbor : NeighborInfo,
    var indexOfRefinedNeighbor : IR_Expression,
    var field : IR_FieldLike,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_RemotePackInfoGhost with IR_C2FPackInfoGhost {

  // we receive N messages from N fine neighbors -> split iteration space
  override def splitSpaceForRecv : Boolean = true

  override protected def getGridPackingStartAndEnd(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForRecv(neighDir)
}

/// IR_C2FPackInfoGhostLocalSend

case class IR_C2FPackInfoGhostLocalSend(
    var neighbor : NeighborInfo,
    var indexOfRefinedNeighbor : IR_Expression,
    var field : IR_FieldLike,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_LocalPackInfoGhost with IR_C2FPackInfoGhost {

  // receiver is fine -> no iteration space splitting
  override def splitSpaceForRecv : Boolean = false

  override protected def getGridPackingStartAndEndDest(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForSend(neighDir)

  override protected def getGridPackingStartAndEndSrc(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForRecv(inverseNeighDir)
}

/// IR_C2FPackInfoGhostLocalRecv

case class IR_C2FPackInfoGhostLocalRecv(
    var neighbor : NeighborInfo,
    var indexOfRefinedNeighbor : IR_Expression,
    var field : IR_FieldLike,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_LocalPackInfoGhost with IR_C2FPackInfoGhost {

  // receiver is fine -> no iteration space splitting
  override def splitSpaceForRecv : Boolean = false

  override protected def getGridPackingStartAndEndDest(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForRecv(neighDir)

  override protected def getGridPackingStartAndEndSrc(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForSend(inverseNeighDir)
}