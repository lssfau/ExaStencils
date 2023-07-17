package exastencils.communication.ir

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.communication.NeighborInfo
import exastencils.config.Knowledge
import exastencils.field.ir.IR_Field

/// IR_RefinementPackInfoGhost

trait IR_RefinementPackInfoGhost extends IR_RefinementPackInfo with IR_PackInfoGhost

/// IR_F2CPackInfoGhost

trait IR_F2CPackInfoGhost extends IR_F2CPackInfo with IR_PackInfoGhost

/// IR_C2FPackInfoGhost

trait IR_C2FPackInfoGhost extends IR_C2FPackInfo with IR_PackInfoGhost

/* fine-to-coarse */

/// IR_F2CPackInfoGhostRemoteSend
case class IR_F2CPackInfoGhostRemoteSend(
    var neighbor : NeighborInfo,
    var refinementNeighborIndex : Int,
    var field : IR_Field,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_RemotePackInfoGhost with IR_F2CPackInfoGhost {

  override protected def getGridPackingStartAndEnd(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) = ???
}

/// IR_F2CPackInfoGhostRemoteRecv

case class IR_F2CPackInfoGhostRemoteRecv(
    var neighbor : NeighborInfo,
    var refinementNeighborIndex : Int,
    var field : IR_Field,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_RemotePackInfoGhost with IR_F2CPackInfoGhost {

  override protected def getGridPackingStartAndEnd(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) = ???
}

/// IR_F2CPackInfoGhostLocalSend

case class IR_F2CPackInfoGhostLocalSend(
    var neighbor : NeighborInfo,
    var refinementNeighborIndex : Int,
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
    var refinementNeighborIndex : Int,
    var field : IR_Field,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_LocalPackInfoGhost with IR_F2CPackInfoGhost {

  override protected def getGridPackingStartAndEndDest(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) = ???
  override protected def getGridPackingStartAndEndSrc(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) = ???
}
