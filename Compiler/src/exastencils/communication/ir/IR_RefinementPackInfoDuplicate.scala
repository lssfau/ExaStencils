package exastencils.communication.ir

import exastencils.base.ir._
import exastencils.communication.NeighborInfo
import exastencils.field.ir.IR_Field

/// IR_RefinementPackInfoDuplicate

trait IR_RefinementPackInfoDuplicate extends IR_RefinementPackInfo with IR_PackInfoDuplicate

/// IR_F2CPackInfoDuplicate

trait IR_F2CPackInfoDuplicate extends IR_F2CPackInfo with IR_PackInfoDuplicate

/// IR_C2FPackInfoDuplicate

trait IR_C2FPackInfoDuplicate extends IR_C2FPackInfo with IR_PackInfoDuplicate

/* fine-to-coarse */

/// IR_F2CPackInfoDuplicateRemoteSend
case class IR_F2CPackInfoDuplicateRemoteSend(
    var neighbor : NeighborInfo,
    var refinementNeighborIndex : Int,
    var field : IR_Field,
    var dupLayerBegin : IR_ExpressionIndex,
    var dupLayerEnd : IR_ExpressionIndex,
) extends IR_RemotePackInfoDuplicate with IR_F2CPackInfoDuplicate

/// IR_F2CPackInfoDuplicateRemoteRecv

case class IR_F2CPackInfoDuplicateRemoteRecv(
    var neighbor : NeighborInfo,
    var refinementNeighborIndex : Int,
    var field : IR_Field,
    var dupLayerBegin : IR_ExpressionIndex,
    var dupLayerEnd : IR_ExpressionIndex,
) extends IR_RemotePackInfoDuplicate with IR_F2CPackInfoDuplicate

/// IR_F2CPackInfoDuplicateLocalSend

case class IR_F2CPackInfoDuplicateLocalSend(
    var neighbor : NeighborInfo,
    var refinementNeighborIndex : Int,
    var field : IR_Field,
    var dupLayerBegin : IR_ExpressionIndex,
    var dupLayerEnd : IR_ExpressionIndex,
) extends IR_LocalPackInfoDuplicate with IR_F2CPackInfoDuplicate

/// IR_F2CPackInfoDuplicateLocalRecv

case class IR_F2CPackInfoDuplicateLocalRecv(
    var neighbor : NeighborInfo,
    var refinementNeighborIndex : Int,
    var field : IR_Field,
    var dupLayerBegin : IR_ExpressionIndex,
    var dupLayerEnd : IR_ExpressionIndex,
) extends IR_LocalPackInfoDuplicate with IR_F2CPackInfoDuplicate