package exastencils.communication.ir

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.communication.NeighborInfo
import exastencils.config.Knowledge
import exastencils.field.ir.IR_Field

trait IR_PackInfoGhost extends IR_PackInfo {
  def ghostLayerBegin : IR_ExpressionIndex
  def ghostLayerEnd : IR_ExpressionIndex

  def getGridPackingStartAndEndForSend(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) = {
    (
      // start
      (0 until numDimsGrid).toArray.map {
        case i if neighDir(i) == 0 =>
          if (Knowledge.comm_syncGhostData)
            resolveIndex("GLB", i)
          else
            resolveIndex("DLB", i)
        case i if neighDir(i) < 0  => resolveIndex("IB", i) + ghostLayerBegin(i)
        case i if neighDir(i) > 0  => resolveIndex("IE", i) - ghostLayerEnd(i)
      },
      // end
      (0 until numDimsGrid).toArray.map {
        case i if neighDir(i) == 0 =>
          if (Knowledge.comm_syncGhostData)
            resolveIndex("GRE", i)
          else
            resolveIndex("DRE", i)
        case i if neighDir(i) < 0  => resolveIndex("IB", i) + ghostLayerEnd(i)
        case i if neighDir(i) > 0  => resolveIndex("IE", i) - ghostLayerBegin(i)
      })
  }

  def getGridPackingStartAndEndForRecv(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) = {
    (
      // start
      (0 until numDimsGrid).toArray.map {
        case i if neighDir(i) == 0 =>
          if (Knowledge.comm_syncGhostData)
            resolveIndex("GLB", i)
          else
            resolveIndex("DLB", i)
        case i if neighDir(i) < 0  => resolveIndex("GLE", i) - ghostLayerEnd(i)
        case i if neighDir(i) > 0  => resolveIndex("GRB", i) + ghostLayerBegin(i)
      },
      // end
      (0 until numDimsGrid).toArray.map {
        case i if neighDir(i) == 0 =>
          if (Knowledge.comm_syncGhostData)
            resolveIndex("GRE", i)
          else
            resolveIndex("DRE", i)
        case i if neighDir(i) < 0  => resolveIndex("GLE", i) - ghostLayerBegin(i)
        case i if neighDir(i) > 0  => resolveIndex("GRB", i) + ghostLayerEnd(i)
      }
    )
  }
}

/// IR_RemotePackInfoGhost

trait IR_RemotePackInfoGhost extends IR_RemotePackInfo with IR_PackInfoGhost

/// IR_LocalPackInfoGhost

trait IR_LocalPackInfoGhost extends IR_LocalPackInfo with IR_PackInfoGhost

/// IR_PackInfoGhostRemoteSend
// remote send pack interval for ghost layers

case class IR_PackInfoGhostRemoteSend(
    var neighbor : NeighborInfo,
    var field : IR_Field,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_RemotePackInfoGhost {

  override protected def getGridPackingStartAndEnd(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getGridPackingStartAndEndForSend(neighDir)
}

/// IR_PackInfoGhostRemoteRecv
// remote recv pack interval for ghost layers

case class IR_PackInfoGhostRemoteRecv(
    var neighbor : NeighborInfo,
    var field : IR_Field,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_RemotePackInfoGhost {

  override protected def getGridPackingStartAndEnd(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getGridPackingStartAndEndForRecv(neighDir)
}

/// IR_PackInfoGhostLocalSend
// local send pack intervals for ghost layers

case class IR_PackInfoGhostLocalSend(
    var neighbor : NeighborInfo,
    var field : IR_Field,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_LocalPackInfoGhost {
  override protected def getGridPackingStartAndEndDest(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getGridPackingStartAndEndForSend(neighDir)

  override protected def getGridPackingStartAndEndSrc(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) = {
    (
      // start
      (0 until numDimsGrid).toArray.map {
        case i if -neighDir(i) == 0 =>
          if (Knowledge.comm_syncGhostData)
            resolveIndex("GLB", i)
          else
            resolveIndex("DLB", i)
        case i if -neighDir(i) < 0  => resolveIndex("GLE", i) - ghostLayerEnd(i)
        case i if -neighDir(i) > 0  => resolveIndex("GRB", i) + ghostLayerBegin(i)
      },
      // end
      (0 until numDimsGrid).toArray.map {
        case i if -neighDir(i) == 0 =>
          if (Knowledge.comm_syncGhostData)
            resolveIndex("GRE", i)
          else
            resolveIndex("DRE", i)
        case i if -neighDir(i) < 0  => resolveIndex("GLE", i) - ghostLayerBegin(i)
        case i if -neighDir(i) > 0  => resolveIndex("GRB", i) + ghostLayerEnd(i)
      }
    )
  }
}

/// IR_PackInfoGhostLocalRecv
// local send pack intervals for ghost layers

case class IR_PackInfoGhostLocalRecv(
    var neighbor : NeighborInfo,
    var field : IR_Field,
    var ghostLayerBegin : IR_ExpressionIndex,
    var ghostLayerEnd : IR_ExpressionIndex
) extends IR_LocalPackInfoGhost {

  override protected def getGridPackingStartAndEndDest(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getGridPackingStartAndEndForRecv(neighDir)

  override protected def getGridPackingStartAndEndSrc(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) = {
    (
      // start
      (0 until numDimsGrid).toArray.map {
        case i if -neighDir(i) == 0 =>
          if (Knowledge.comm_syncGhostData)
            resolveIndex("GLB", i)
          else
            resolveIndex("DLB", i)
        case i if -neighDir(i) < 0  => resolveIndex("IB", i) + ghostLayerBegin(i)
        case i if -neighDir(i) > 0  => resolveIndex("IE", i) - ghostLayerEnd(i)
      },
      // end
      (0 until numDimsGrid).toArray.map {
        case i if -neighDir(i) == 0 =>
          if (Knowledge.comm_syncGhostData)
            resolveIndex("GRE", i)
          else
            resolveIndex("DRE", i)
        case i if -neighDir(i) < 0  => resolveIndex("IB", i) + ghostLayerEnd(i)
        case i if -neighDir(i) > 0  => resolveIndex("IE", i) - ghostLayerBegin(i)
      }
    )
  }

}
