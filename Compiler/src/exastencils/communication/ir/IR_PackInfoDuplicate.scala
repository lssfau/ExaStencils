package exastencils.communication.ir

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.communication.NeighborInfo
import exastencils.config.Knowledge
import exastencils.field.ir.IR_Field

/// IR_PackingIntervalDuplicate

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
      //end
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

/// IR_RemotePackingIntervalDuplicate

trait IR_RemotePackInfoDuplicate extends IR_RemotePackInfo with IR_PackInfoDuplicate

/// IR_LocalPackingIntervalDuplicate

trait IR_LocalPackInfoDuplicate extends IR_LocalPackInfo with IR_PackInfoDuplicate {

  private def inverseNeighDir : Array[Int] = neighbor.dir.map(-1 * _)

  // local packing of dest and src fields is almost identical. only difference is the neighbor direction
  override protected def getGridPackingStartAndEndDest(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getGridPackingStartAndEnd(neighbor.dir)
  override protected def getGridPackingStartAndEndSrc(neighDir : Array[Int]) : (Array[IR_Expression], Array[IR_Expression]) =
    getGridPackingStartAndEnd(inverseNeighDir)
}

/// IR_PackingIntervalDuplicateRemoteSend
// remote send pack interval for duplicate layers

case class IR_PackInfoDuplicateRemoteSend(
    var neighbor : NeighborInfo,
    var field : IR_Field,
    var dupLayerBegin : IR_ExpressionIndex,
    var dupLayerEnd : IR_ExpressionIndex,
) extends IR_RemotePackInfoDuplicate

/// IR_PackingIntervalDuplicateRemoteRecv
// remote recv pack interval for duplicate layers

case class IR_PackInfoDuplicateRemoteRecv(
    var neighbor : NeighborInfo,
    var field : IR_Field,
    var dupLayerBegin : IR_ExpressionIndex,
    var dupLayerEnd : IR_ExpressionIndex,
) extends IR_RemotePackInfoDuplicate

/// IR_PackingIntervalDuplicateLocalSend
// local send pack intervals for duplicate layers

case class IR_PackInfoDuplicateLocalSend(
    var neighbor : NeighborInfo,
    var field : IR_Field,
    var dupLayerBegin : IR_ExpressionIndex,
    var dupLayerEnd : IR_ExpressionIndex,
) extends IR_LocalPackInfoDuplicate

/// IR_PackingIntervalDuplicateLocalRecv
// local send pack intervals for duplicate layers

case class IR_PackInfoDuplicateLocalRecv(
    var neighbor : NeighborInfo,
    var field : IR_Field,
    var dupLayerBegin : IR_ExpressionIndex,
    var dupLayerEnd : IR_ExpressionIndex,
) extends IR_LocalPackInfoDuplicate
