package exastencils.communication.ir

import exastencils.base.ir._
import exastencils.communication.NeighborInfo
import exastencils.field.ir.IR_Field

/// IR_RefinementPackInfoDuplicate

trait IR_RefinementPackInfoDuplicate extends IR_RefinementPackInfo with IR_PackInfoDuplicate

/// IR_F2CPackInfoDuplicate

trait IR_F2CPackInfoDuplicate extends IR_F2CPackInfo with IR_RefinementPackInfoDuplicate

/// IR_C2FPackInfoDuplicate

trait IR_C2FPackInfoDuplicate extends IR_C2FPackInfo with IR_RefinementPackInfoDuplicate

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

/* coarse-to-fine */

/// IR_C2FPackInfoDuplicateRemoteSend
case class IR_C2FPackInfoDuplicateRemoteSend(
    var neighbor : NeighborInfo,
    var refinementNeighborIndex : Int,
    var field : IR_Field,
    var dupLayerBegin : IR_ExpressionIndex,
    var dupLayerEnd : IR_ExpressionIndex,
) extends IR_RemotePackInfoDuplicate with IR_C2FPackInfoDuplicate

/// IR_C2FPackInfoDuplicateRemoteRecv

case class IR_C2FPackInfoDuplicateRemoteRecv(
    var neighbor : NeighborInfo,
    var refinementNeighborIndex : Int,
    var field : IR_Field,
    var dupLayerBegin : IR_ExpressionIndex,
    var dupLayerEnd : IR_ExpressionIndex,
) extends IR_RemotePackInfoDuplicate with IR_C2FPackInfoDuplicate

/// IR_C2FPackInfoDuplicateLocalSend

case class IR_C2FPackInfoDuplicateLocalSend(
    var neighbor : NeighborInfo,
    var refinementNeighborIndex : Int,
    var field : IR_Field,
    var dupLayerBegin : IR_ExpressionIndex,
    var dupLayerEnd : IR_ExpressionIndex,
) extends IR_LocalPackInfoDuplicate with IR_C2FPackInfoDuplicate

/// IR_C2FPackInfoDuplicateLocalRecv

case class IR_C2FPackInfoDuplicateLocalRecv(
    var neighbor : NeighborInfo,
    var refinementNeighborIndex : Int,
    var field : IR_Field,
    var dupLayerBegin : IR_ExpressionIndex,
    var dupLayerEnd : IR_ExpressionIndex,
) extends IR_LocalPackInfoDuplicate with IR_C2FPackInfoDuplicate