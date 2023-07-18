package exastencils.communication.ir

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.communication.NeighborInfo
import exastencils.config.Knowledge
import exastencils.field.ir.IR_Field

/// IR_RefinementPackInfoGhost

trait IR_RefinementPackInfoGhost extends IR_RefinementPackInfo with IR_PackInfoGhost {
  def getRefinedGridPackingStartAndEndForRecv(neighDir : Array[Int], refinedNeighborIndexFromFine : IR_Expression) : (Array[IR_Expression], Array[IR_Expression]) = {
    val (start, end) = getGridPackingStartAndEndForRecv(neighDir)

    // receiver fragment is coarse:
    // - split coarse iteration space of coarse neighbor into equally spaced sections
    // - splitting is done in non-commAxis direction
    val sectionsPerDim = Knowledge.refinement_maxFineNeighborsPerDim
    val (coarseNeighborStart, coarseNeighborEnd) = (0 until numDimsGrid).map {
      case i if neighDir(i) == 0 =>
        val fullInterval = end(i) - start(i)

        // go over grid dimensions in x -> y -> z order and assign each fine neighbor a section of the coarse iteration space
        val s = start(i) + (refinedNeighborIndexFromFine Mod sectionsPerDim) * (fullInterval / sectionsPerDim)
        val e = end(i)   - ((refinedNeighborIndexFromFine + 1) Mod sectionsPerDim) * (fullInterval / sectionsPerDim)

        (s, e)
      case i                     =>
        (start(i), end(i))
    }.unzip

    (coarseNeighborStart.toArray, coarseNeighborEnd.toArray)
  }
}

/// IR_F2CPackInfoGhost

trait IR_F2CPackInfoGhost extends IR_F2CPackInfo with IR_RefinementPackInfoGhost

/// IR_C2FPackInfoGhost

trait IR_C2FPackInfoGhost extends IR_C2FPackInfo with IR_RefinementPackInfoGhost

/* fine-to-coarse */

/// IR_F2CPackInfoGhostRemoteSend
case class IR_F2CPackInfoGhostRemoteSend(
    var neighbor : NeighborInfo,
    var indexOfRefinedNeighbor : Int,
    var field : IR_Field,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_RemotePackInfoGhost with IR_F2CPackInfoGhost {

  // pack interval for sending fine fragments can be reused from equal-level comm
  override protected def getGridPackingStartAndEnd(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getGridPackingStartAndEndForSend(neighDir)
}

/// IR_F2CPackInfoGhostRemoteRecv

case class IR_F2CPackInfoGhostRemoteRecv(
    var neighbor : NeighborInfo,
    var indexOfRefinedNeighbor : Int,
    var field : IR_Field,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_RemotePackInfoGhost with IR_F2CPackInfoGhost {

  // unpacking interval for receiving from fine to coarse requires adaptations
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

  override protected def getGridPackingStartAndEndDest(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) = ???
  override protected def getGridPackingStartAndEndSrc(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) = ???
}

/// IR_F2CPackInfoGhostLocalRecv

case class IR_F2CPackInfoGhostLocalRecv(
    var neighbor : NeighborInfo,
    var indexOfRefinedNeighbor : Int,
    var field : IR_Field,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_LocalPackInfoGhost with IR_F2CPackInfoGhost {

  override protected def getGridPackingStartAndEndDest(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) = ???
  override protected def getGridPackingStartAndEndSrc(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) = ???
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

  override protected def getGridPackingStartAndEnd(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) = ???
}

/// IR_C2FPackInfoGhostRemoteRecv

case class IR_C2FPackInfoGhostRemoteRecv(
    var neighbor : NeighborInfo,
    var indexOfRefinedNeighbor : Int,
    var field : IR_Field,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_RemotePackInfoGhost with IR_C2FPackInfoGhost {

  override protected def getGridPackingStartAndEnd(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) = ???
}

/// IR_C2FPackInfoGhostLocalSend

case class IR_C2FPackInfoGhostLocalSend(
    var neighbor : NeighborInfo,
    var indexOfRefinedNeighbor : Int,
    var field : IR_Field,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_LocalPackInfoGhost with IR_C2FPackInfoGhost {

  override protected def getGridPackingStartAndEndDest(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) = ???
  override protected def getGridPackingStartAndEndSrc(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) = ???
}

/// IR_C2FPackInfoGhostLocalRecv

case class IR_C2FPackInfoGhostLocalRecv(
    var neighbor : NeighborInfo,
    var indexOfRefinedNeighbor : Int,
    var field : IR_Field,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_LocalPackInfoGhost with IR_C2FPackInfoGhost {

  override protected def getGridPackingStartAndEndDest(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) = ???
  override protected def getGridPackingStartAndEndSrc(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) = ???
}