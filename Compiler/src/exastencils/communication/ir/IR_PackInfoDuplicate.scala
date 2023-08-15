package exastencils.communication.ir

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.communication.NeighborInfo
import exastencils.config.Knowledge
import exastencils.fieldlike.ir.IR_FieldLike

/// IR_PackInfoDuplicate

trait IR_PackInfoDuplicate extends IR_PackInfo {
  def dupLayerBegin : IR_ExpressionIndex
  def dupLayerEnd : IR_ExpressionIndex

  def getGridPackingStartAndEnd(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) = {
    // TODO: this only works for comm_onlyAxisNeighbors == false if coarse grid topology is regular, otherwise iteration spaces must be adapted
    (
      // start
      (0 until numDimsGrid).toArray.map {
        case i if neighDir(i) == 0 =>
          if (Knowledge.comm_onlyAxisNeighbors)
            resolveIndex("DLB", i)
          else
            resolveIndex("IB", i)
        case i if neighDir(i) < 0  => resolveIndex("DLE", i) - dupLayerEnd(i)
        case i if neighDir(i) > 0  => resolveIndex("DRB", i) + dupLayerBegin(i)
      },
      // end
      (0 until numDimsGrid).toArray.map {
        case i if neighDir(i) == 0 =>
          if (Knowledge.comm_onlyAxisNeighbors)
            resolveIndex("DRE", i)
          else
            resolveIndex("DRE", i)
        case i if neighDir(i) < 0  => resolveIndex("DLE", i) - dupLayerBegin(i)
        case i if neighDir(i) > 0  => resolveIndex("DRB", i) + dupLayerEnd(i)
      }
    )
  }
}

/// IR_RemotePackInfoDuplicate

trait IR_RemotePackInfoDuplicate extends IR_RemotePackInfo with IR_PackInfoDuplicate

/// IR_LocalPackInfoDuplicate

trait IR_LocalPackInfoDuplicate extends IR_LocalPackInfo with IR_PackInfoDuplicate {
  // local packing of dest and src fields is almost identical. only difference is the neighbor direction
  override protected def getGridPackingStartAndEndDest(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getGridPackingStartAndEnd(neighbor.dir)
  override protected def getGridPackingStartAndEndSrc(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getGridPackingStartAndEnd(inverseNeighDir)
}

/// IR_PackInfoDuplicateRemoteSend
// remote send pack interval for duplicate layers

case class IR_PackInfoDuplicateRemoteSend(
    var neighbor : NeighborInfo,
    var field : IR_FieldLike,
    var dupLayerBegin : IR_ExpressionIndex,
    var dupLayerEnd : IR_ExpressionIndex,
) extends IR_RemotePackInfoDuplicate

/// IR_PackInfoDuplicateRemoteRecv
// remote recv pack interval for duplicate layers

case class IR_PackInfoDuplicateRemoteRecv(
    var neighbor : NeighborInfo,
    var field : IR_FieldLike,
    var dupLayerBegin : IR_ExpressionIndex,
    var dupLayerEnd : IR_ExpressionIndex,
) extends IR_RemotePackInfoDuplicate

/// IR_PackInfoDuplicateLocalSend
// local send pack intervals for duplicate layers

case class IR_PackInfoDuplicateLocalSend(
    var neighbor : NeighborInfo,
    var field : IR_FieldLike,
    var dupLayerBegin : IR_ExpressionIndex,
    var dupLayerEnd : IR_ExpressionIndex,
) extends IR_LocalPackInfoDuplicate

/// IR_PackInfoDuplicateLocalRecv
// local send pack intervals for duplicate layers

case class IR_PackInfoDuplicateLocalRecv(
    var neighbor : NeighborInfo,
    var field : IR_FieldLike,
    var dupLayerBegin : IR_ExpressionIndex,
    var dupLayerEnd : IR_ExpressionIndex,
) extends IR_LocalPackInfoDuplicate
