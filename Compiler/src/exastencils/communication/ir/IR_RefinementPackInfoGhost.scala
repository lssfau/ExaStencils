package exastencils.communication.ir

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.communication.NeighborInfo
import exastencils.config.Knowledge
import exastencils.field.ir.IR_Field

/// IR_RefinementPackInfoGhost

trait IR_RefinementPackInfoGhost extends IR_RefinementPackInfo with IR_PackInfoGhost {
  protected def getRefinedGridPackingStartAndEndForSend(neighDir : Array[Int], indexOfRefinedNeighbor : Int) : (Array[IR_Expression], Array[IR_Expression])

  protected def getRefinedGridPackingStartAndEndForRecv(neighDir : Array[Int], indexOfRefinedNeighbor : Int) : (Array[IR_Expression], Array[IR_Expression])

  protected def splitPackingStartAndEndForCoarseFragment(neighDir : Array[Int], indexOfRefinedNeighbor : Int,
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

  // pack interval for sending fine fragments to a coarse one can be reused from equal-level comm (since fine fragments are sent as a whole)
  override def getRefinedGridPackingStartAndEndForSend(neighDir : Array[Int], indexOfRefinedNeighbor : Int) : (Array[IR_Expression], Array[IR_Expression]) =
    getGridPackingStartAndEndForSend(neighDir)

  // unpacking interval for receiving from fine to coarse requires adaptations (since one of received fine fragments covers only a portion of the coarse fragment)
  // -> split iteration interval of coarse fragment
  override def getRefinedGridPackingStartAndEndForRecv(neighDir : Array[Int], indexOfRefinedNeighbor : Int) : (Array[IR_Expression], Array[IR_Expression]) = {
    val (start, end) = getGridPackingStartAndEndForRecv(neighDir)

    splitPackingStartAndEndForCoarseFragment(neighDir, indexOfRefinedNeighbor, start, end)
  }
}

/// IR_C2FPackInfoGhost

trait IR_C2FPackInfoGhost extends IR_C2FPackInfo with IR_RefinementPackInfoGhost {

  // pack interval for sending a coarse fragment to finer ones requires adaptations (a coarse block sends N messages, where N is the number of fine neighbors)
  // -> split iteration interval of coarse fragment
  override def getRefinedGridPackingStartAndEndForSend(neighDir : Array[Int], indexOfRefinedNeighbor : Int) : (Array[IR_Expression], Array[IR_Expression]) = {
    val (start, end) = getGridPackingStartAndEndForSend(neighDir)

    splitPackingStartAndEndForCoarseFragment(neighDir, indexOfRefinedNeighbor, start, end)
  }

  // pack interval for receiving from a coarse fragment into fine fragments can be reused from equal-level comm
  // (since coarse fragment already deals with iteration space splitting when sending -> just receive data as a whole)
  override def getRefinedGridPackingStartAndEndForRecv(neighDir : Array[Int], indexOfRefinedNeighbor : Int) : (Array[IR_Expression], Array[IR_Expression]) = {
    getGridPackingStartAndEndForRecv(neighDir)
  }
}

/* fine-to-coarse */

/// IR_F2CPackInfoGhostRemoteSend
case class IR_F2CPackInfoGhostRemoteSend(
    var neighbor : NeighborInfo,
    var indexOfRefinedNeighbor : Int,
    var field : IR_Field,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_RemotePackInfoGhost with IR_F2CPackInfoGhost {

  override protected def getGridPackingStartAndEnd(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForSend(neighDir, indexOfRefinedNeighbor)
}

/// IR_F2CPackInfoGhostRemoteRecv

case class IR_F2CPackInfoGhostRemoteRecv(
    var neighbor : NeighborInfo,
    var indexOfRefinedNeighbor : Int,
    var field : IR_Field,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_RemotePackInfoGhost with IR_F2CPackInfoGhost {

  override protected def getGridPackingStartAndEnd(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForRecv(neighDir, indexOfRefinedNeighbor)
}

/// IR_F2CPackInfoGhostLocalSend

case class IR_F2CPackInfoGhostLocalSend(
    var neighbor : NeighborInfo,
    var indexOfRefinedNeighbor : Int,
    var field : IR_Field,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_LocalPackInfoGhost with IR_F2CPackInfoGhost {

  override protected def getGridPackingStartAndEndDest(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForSend(neighDir, indexOfRefinedNeighbor)
  override protected def getGridPackingStartAndEndSrc(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForRecv(inverseNeighDir, indexOfRefinedNeighbor)
}

/// IR_F2CPackInfoGhostLocalRecv

case class IR_F2CPackInfoGhostLocalRecv(
    var neighbor : NeighborInfo,
    var indexOfRefinedNeighbor : Int,
    var field : IR_Field,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_LocalPackInfoGhost with IR_F2CPackInfoGhost {

  override protected def getGridPackingStartAndEndDest(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForRecv(neighDir, indexOfRefinedNeighbor)
  override protected def getGridPackingStartAndEndSrc(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForSend(inverseNeighDir, indexOfRefinedNeighbor)
}

/* coarse-to-fine */

/// IR_C2FPackInfoGhostRemoteSend
case class IR_C2FPackInfoGhostRemoteSend(
    var neighbor : NeighborInfo,
    var indexOfRefinedNeighbor : Int,
    var field : IR_Field,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_RemotePackInfoGhost with IR_C2FPackInfoGhost {

  override protected def getGridPackingStartAndEnd(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForSend(neighDir, indexOfRefinedNeighbor)
}

/// IR_C2FPackInfoGhostRemoteRecv

case class IR_C2FPackInfoGhostRemoteRecv(
    var neighbor : NeighborInfo,
    var indexOfRefinedNeighbor : Int,
    var field : IR_Field,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_RemotePackInfoGhost with IR_C2FPackInfoGhost {

  override protected def getGridPackingStartAndEnd(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForRecv(neighDir, indexOfRefinedNeighbor)
}

/// IR_C2FPackInfoGhostLocalSend

case class IR_C2FPackInfoGhostLocalSend(
    var neighbor : NeighborInfo,
    var indexOfRefinedNeighbor : Int,
    var field : IR_Field,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_LocalPackInfoGhost with IR_C2FPackInfoGhost {

  override protected def getGridPackingStartAndEndDest(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForSend(neighDir, indexOfRefinedNeighbor)
  override protected def getGridPackingStartAndEndSrc(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForRecv(inverseNeighDir, indexOfRefinedNeighbor)
}

/// IR_C2FPackInfoGhostLocalRecv

case class IR_C2FPackInfoGhostLocalRecv(
    var neighbor : NeighborInfo,
    var indexOfRefinedNeighbor : Int,
    var field : IR_Field,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_LocalPackInfoGhost with IR_C2FPackInfoGhost {

  override protected def getGridPackingStartAndEndDest(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForRecv(neighDir, indexOfRefinedNeighbor)
  override protected def getGridPackingStartAndEndSrc(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getRefinedGridPackingStartAndEndForSend(inverseNeighDir, indexOfRefinedNeighbor)
}