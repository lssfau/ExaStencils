package exastencils.grid

import scala.collection.mutable.ListBuffer

/// VirtualField

/// stores identifiers of possible virtual fields
object VirtualField {
  val fields = ListBuffer(
    "vf_nodePosition_x", "vf_nodePosition_y", "vf_nodePosition_z",
    "nodePosition_x", "nodePosition_y", "nodePosition_z",

    "vf_nodePosAsVec",

    "vf_cellCenter_x", "vf_cellCenter_y", "vf_cellCenter_z",
    "cellCenter_x", "cellCenter_y", "cellCenter_z",

    "vf_boundaryCoord_x", "vf_boundaryCoord_y", "vf_boundaryCoord_z",
    "boundaryCoord_x", "boundaryCoord_y", "boundaryCoord_z",

    "vf_gridWidth_x", "vf_gridWidth_y", "vf_gridWidth_z",
    "gridWidth_x", "gridWidth_y", "gridWidth_z",

    "vf_cellWidth_x", "vf_cellWidth_y", "vf_cellWidth_z",
    "cellWidth_x", "cellWidth_y", "cellWidth_z",

    "vf_stagCVWidth_x", "vf_stagCVWidth_y", "vf_stagCVWidth_z",
    "stagCVWidth_x", "stagCVWidth_y", "stagCVWidth_z",

    "vf_cellVolume", "vf_xStagCellVolume", "vf_yStagCellVolume", "vf_zStagCellVolume",
    "cellVolume", "xStagCellVolume", "yStagCellVolume", "zStagCellVolume",

    "vf_cellCenterToFace_x", "vf_cellCenterToFace_y", "vf_cellCenterToFace_z").map(_.toLowerCase())
}